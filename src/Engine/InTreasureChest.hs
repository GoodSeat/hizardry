module Engine.InTreasureChest
where

import PreludeL
import Control.Monad (forM, forM_, when, join)
import Control.Monad.Reader (asks)
import Engine.GameAuto
import Engine.Utils
import Engine.CharacterAction (gainItem)
import Data.World
import Data.Primitive
import qualified Data.Characters as Chara
import qualified Data.Enemies as Enemy
import qualified Data.Items as Item
import qualified Data.Maze as Maze
import qualified Data.Map as Map
import qualified Data.Spells as Spell
import Data.Formula (parse')
import Data.List (find)
import Data.Char (toLower)
import Data.Bifunctor (bimap)


data TreasureCondition = TreasureCondition {
      afterChest   :: GameMachine
    , dropGold     :: Int
    , dropItems    :: [Int]
    , trap         :: Enemy.Trap
    , whenAlarm    :: EnemyID -> GameMachine
    }

actionForTreasureChest :: TreasureCondition
                       -> [PartyPos]  -- ^ already inspect characters.
                       -> GameMachine
actionForTreasureChest con ps =
    if trap con == Enemy.DropDirectly then getTreasures con
    else GameAuto $ do
        msgCmd <- switchL $ EnJp "^I)nspect\n^S)pell\n^D)isarm Trap\n^O)pen\n^L)eave `[`E`s`c`]"
                                 "^I)調べる\n^S)呪文\n^D)罠を外す\n^O)開ける\n^L)立ち去る `[`E`s`c`]"
        run $ selectEsc (battleCommand msgCmd)
                       [ (Key "l", afterChest con                                       )
                       , (Key "i", inspectTreasureChest ps con                          )
                       , (Key "s", spellToTreasureChest ps con                          )
                       , (Key "d", disarmTrap con (actionForTreasureChest con ps)       )
                       , (Key "o", openTreasureChest con (actionForTreasureChest con ps))]

inspectTreasureChest :: [PartyPos] -> TreasureCondition -> GameMachine
inspectTreasureChest ps con = GameAuto $ do
    msg1 <- switchL $ EnJp "^#)Inspect\n^L)eave `[`E`S`C`]"
                           "^#)調べる\n^L)戻る `[`E`S`C`]"
    cmds <- cmdNumPartiesWhen $ bimap inspect' (not . isCantFight)
    run $ selectWhenEsc (message msg1) $ (Key "l", actionForTreasureChest con ps, True)
                                                                   : cmds
  where
    inspect' p = if p `elem` ps 
                 then GameAuto $ do
                     m <- switchL $ EnJp "Already inspected." "既に調べました。"
                     run $ events [message m] $ inspectTreasureChest ps con
                 else inspectTreasureChestBy p ps con

inspectTreasureChestBy :: PartyPos -> [PartyPos] -> TreasureCondition -> GameMachine
inspectTreasureChestBy i ps con = GameAuto $ do
    c <- characterInPartyAt i
    m <- formulaMapC c
    successed  <- happens =<< evalWith m (Chara.inspectTrapAbility $ Chara.job c)
    invokeTrap <- happens =<< evalWith m . parse' =<< asks (invokeTrapInspectProb . scenarioFormulas)
    trap'      <- randomIn [Enemy.NoTrap .. Enemy.Alarm]
    let afterInspect = actionForTreasureChest con $ i:ps
    run $ if      successed  then events [message $ inspectMessage (trap con)] afterInspect
          else if invokeTrap then invokingTrap con i
          else                    events [message $ inspectMessage trap'] afterInspect

inspectMessage :: Enemy.Trap -> String
inspectMessage trap = case trap of Enemy.NoTrap -> "No traps."
                                   _            -> show trap ++ "."

spellToTreasureChest :: [PartyPos] -> TreasureCondition -> GameMachine
spellToTreasureChest ps con = GameAuto $ do
    msg <- switchL $ EnJp "^#)Spell\n^L)eave `[`E`S`C`]"
                          "^#)呪文\n^L)戻る `[`E`S`C`]"
    cmds <- cmdNumPartiesWhen $ bimap spell' (not . isCantSpell)
    run $ selectWhenEsc (message msg) $ (Key "l", actionForTreasureChest con ps, True)
                                                                 : cmds
  where
    spell' p = spellToTreasureChestBy p ps con

spellToTreasureChestBy :: PartyPos -> [PartyPos] -> TreasureCondition -> GameMachine
spellToTreasureChestBy i ps con = GameAuto $ do
    msgCmd <- switchL $ EnJp "Input spell.\n(Empty to cancel.)" "呪文を入力してください。\n(空欄でキャンセル)"
    return (spellCommand msgCmd, \(Key s) -> if isNullKey s then next else trySpell s)
  where
    next = actionForTreasureChest con ps
    trySpell s = GameAuto $ do
      spellDef <- spellByName s
      case spellDef of
        Nothing -> do
          msgWhat <- switchL $ EnJp "what?" "何？"
          run $ events [Resume $ changeMessage msgWhat] next
        Just def -> do
          c <- characterInPartyAt i
          know <- knowSpell' c def
          can  <- canSpell'  c def
          if not know then do
            msgCant <- switchL $ EnJp "you can't casting it." "それを唱えることはできない。"
            run $ events [Resume $ changeMessage msgCant] next
          else if not can then do
            msgNoMP <- switchL $ EnJp "no more MP." "魔力が足りない。"
            run $ events [Resume $ changeMessage msgNoMP] next
          else do
            case Spell.effect def of
              Spell.IdentifyTrap t -> do
                join $ updateCharacter <$> characterIDInPartyAt i <*> costSpell' c def
                m <- formulaMapC c
                successed  <- happens =<< evalWith m t
                trap'      <- randomIn [Enemy.NoTrap .. Enemy.Alarm]
                let trapFound = if successed then trap con else trap'
                    msgText   = inspectMessage trapFound
                run $ events [message msgText] next
              _ -> do
                msgCantUse <- switchL $ EnJp "can't use it here." "ここでは使えない。"
                run $ events [Resume $ changeMessage msgCantUse] next

disarmTrap :: TreasureCondition -> GameMachine -> GameMachine
disarmTrap con afterNotDisarm = GameAuto $ do
    msg <- switchL $ EnJp "^#)Disarm\n^L)eave `[`E`S`C`]"
                          "^#)罠を外す\n^L)戻る `[`E`S`C`]"
    cmds <- cmdNumPartiesWhen $ bimap disarm' (not . isCantFight)
    run $ selectWhenEsc (message msg) $ (Key "l", afterNotDisarm, True)
                                                                  : cmds
  where
    disarm' p = GameAuto $ do
        msgAsk <- switchL $ EnJp "Input trap.\n(Empty to cancel.)" "罠の名前を入力してください。\n(空欄でキャンセル)"
        return (ask msgAsk Nothing,
                \(Key s) -> if isNullKey s then afterNotDisarm else tryDisarm con s p afterNotDisarm)

tryDisarm :: TreasureCondition -> String -> PartyPos -> GameMachine -> GameMachine
tryDisarm con t i afterNotDisarm = GameAuto $ do
    c <- characterInPartyAt i
    m <- formulaMapC c
    let matchTrap = (toLower <$> show (trap con)) == (toLower <$> t)
    sucessDisarming <- happens =<< evalWith m (Chara.disarmTrapAbility $ Chara.job c)
    invokeTrap      <- happens =<< evalWith m . parse' =<< asks (invokeTrapDisarmProb . scenarioFormulas)
    if not matchTrap 
    -- 罠が一致しない場合はトラップ作動
    then run $ invokingTrap con i
    else if sucessDisarming then do
        msgOk <- switchL $ EnJp "Trap successfully disarmed!" "罠の解除に成功した！"
        run $ events [message msgOk] (getTreasures con)
    else if invokeTrap then run $ invokingTrap con i
    else do
        msgFail <- switchL $ EnJp "The trap was not disarmed." "罠を解除できなかった。"
        run $ events [message msgFail] afterNotDisarm


openTreasureChest :: TreasureCondition -> GameMachine -> GameMachine
openTreasureChest con afterNotOpen = GameAuto $ do
    msg <- switchL $ EnJp "^#)Open\n^L)eave `[`E`S`C`]"
                          "^#)開ける\n^L)戻る `[`E`S`C`]"
    cmds <- cmdNumPartiesWhen $ bimap open' (not . isCantFight)
    run $ selectWhenEsc (message msg) $ (Key "l", afterNotOpen, True)
                                                                : cmds
  where
    open' p = tryDisarm con "" p (openTreasureChest con afterNotOpen)


invokingTrap :: TreasureCondition -> PartyPos -> GameMachine
invokingTrap con i = GameAuto $ do
    cid               <- characterIDInPartyAt i
    (msg, eid', gain) <- effectTrap cid $ trap con
    sortPartyAuto
    msgText <- switchL msg
    case eid' of
      Nothing  -> run $ events [message msgText] (if gain then getTreasures con else afterChest con)
      Just eid -> run $ events [message msgText] (whenAlarm con eid)


effectTrap :: CharacterID -> Enemy.Trap -> GameState (LanguageSet String, Maybe EnemyID, Bool)
effectTrap _ Enemy.NoTrap = return (EnJp "No traps were set." "罠は仕掛けられていなかった。", Nothing, True)
effectTrap _ Enemy.DropDirectly = undefined
effectTrap i Enemy.PoisonNeedle = do
    updateCharacterWith i (addPoison 1)
    return (EnJp "Ooops!! Poison Needle!!" "おおっと！！ 毒針だ！！", Nothing, True)
effectTrap i Enemy.GasBomb = do
    ps <- party <$> world
    forM_ ps $ \i -> do
      c   <- characterByID i
      m   <- formulaMapC c
      hit <- happens =<< evalWith m (parse' "100*(20-luc)/20")
      when hit $ updateCharacterWith i (addPoison 1)
    return (EnJp "Ooops!! Gas Bomb!!" "おおっと！！ ガス爆弾だ！！", Nothing, True)
effectTrap i Enemy.CrossbowBolt = do
    floor <- (+1) . Maze.z <$> currentPosition
    dmg <- eval (parse' $ show floor ++ "d8")
    updateCharacterWith i (damageHp dmg)
    return (EnJp "Ooops!! Crossbow Bolt!!" "おおっと！！ 石弓の矢だ！！", Nothing, True)
effectTrap i Enemy.ExplodingBox = do
    ps    <- party <$> world
    floor <- (+1) . Maze.z <$> currentPosition
    forM_ ps $ \i -> do
      hit <- happens 75
      flg <- happens 66
      dmg1 <- eval (parse' $ show floor ++ "d5")
      dmg2 <- eval (parse' $ show floor ++ "d8")
      when hit $ updateCharacterWith i (damageHp $ if flg then dmg1 else dmg2)
    return (EnJp "Ooops!! Exploding Box!!" "おおっと！！ 爆弾だ！！", Nothing, True)
effectTrap i Enemy.Stunner = do
    updateCharacterWith i (addStatusError Paralysis)
    return (EnJp "Ooops!! Stunner!!" "おおっと！！ スタナーだ！！", Nothing, True)
effectTrap _ Enemy.Teleporter = do
    p       <- currentPosition
    size    <- mazeSizeAt $ Maze.z p
    (x',y') <- case size of Just ((x0, y0), (w, h)) -> (,) <$> randomIn [x0..(x0+w-1)] <*> randomIn [y0..(y0+h-1)]
                            Nothing                 -> (,) <$> randomIn [(Maze.x p-100)..(Maze.x p+100)] <*> randomIn [(Maze.y p-100)..(Maze.y p+100)]
    movePlace $ FindTreasureChest (p { Maze.x = x', Maze.y = y' }) False
    return (EnJp "Ooops!! Teleporter!!" "おおっと！！ テレポーターだ！！", Nothing, False)
effectTrap _ Enemy.MageBlaster = undefined -- TODO
effectTrap _ Enemy.PriestBlaster = undefined -- TODO
effectTrap _ Enemy.Alarm = do
    c    <- Maze.coordOf <$> currentPosition
    emap <- asks roomBattleMap
    case emap c of
      Nothing      -> return (EnJp "No traps were set." "罠は仕掛けられていなかった。", Nothing, True)
      Just (_, es) -> do
        eid <- randomIn es
        return (EnJp "Ooops!! Alarm!!" "おおっと！！ 警報だ！！", Just eid, True)


getTreasures :: TreasureCondition -> GameMachine
getTreasures con = GameAuto $ do
    ps <- party <$> world
    let np   = length ps
        gp   = dropGold con `div` np
    
    msg1 <- if gp > 0 
            then do
                m <- switchL $ EnJp ("Each survivor got " ++ show gp ++ "G.P.")
                                    ("生存者はそれぞれ " ++ show gp ++ "G.P. を得た。")
                return [message m]
            else return []
            
    itemsMsgs <- divideItems (dropItems con)
    msg2 <- forM itemsMsgs $ \lmsg -> do
        m <- switchL lmsg
        return $ message m
        
    forM_ ps $ flip spentGold (-gp)
    movePlace =<< FindTreasureChest <$> currentPosition <*> pure True
    run $ events (msg2 ++ msg1) (afterChest con)


divideItems :: [Int] -> GameState [LanguageSet String]
divideItems [] = return []
divideItems (i:is) = do
    ids <- party <$> world
    ps  <- forM ids (\cid -> (,) cid <$> characterByID cid)
    case find (\(_, c) -> not (Chara.hasMaxCountItem c)) ps of
      Nothing         -> return []
      Just (cid', c') -> do
        gainItem cid' (ItemInf (ItemID i) False)
        idef <- itemByID (ItemID i)
        rest <- divideItems is
        return $ (EnJp (Chara.name c' ++ " got " ++ Item.nameUndetermined idef ++ ".")
                       (Chara.name c' ++ " は " ++ Item.nameUndetermined idef ++ " を手に入れた。")) : rest





