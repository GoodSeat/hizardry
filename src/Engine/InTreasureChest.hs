module Engine.InTreasureChest
where
import Control.Monad (forM, forM_, when)
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
    else selectEsc (BattleCommand "^I)nspect\n^D)isarm Trap\n^O)pen\n^L)eave `[`E`s`c`]")
                   [ (Key "l", afterChest con                                       )
                   , (Key "i", inspectTreasureChest ps con                          )
                   , (Key "d", disarmTrap con (actionForTreasureChest con ps)       )
                   , (Key "o", openTreasureChest con (actionForTreasureChest con ps))]

inspectTreasureChest :: [PartyPos] -> TreasureCondition -> GameMachine
inspectTreasureChest ps con = GameAuto $ do
    cmds <- cmdNumPartiesWhen $ bimap inspect' (not . isCantFight)
    run $ selectWhenEsc (Message "^#)Inspect\n^L)eave `[`E`S`C`]") $ (Key "l", actionForTreasureChest con ps, True)
                                                                   : cmds
  where
    inspect' p = if p `elem` ps then events [Message "Already inspected."] $ inspectTreasureChest ps con
                                else inspectTreasureChestBy p ps con

inspectTreasureChestBy :: PartyPos -> [PartyPos] -> TreasureCondition -> GameMachine
inspectTreasureChestBy i ps con = GameAuto $ do
    c <- characterInPartyAt i
    m <- formulaMapS (Left c)
    successed  <- happens =<< evalWith m (Chara.inspectTrapAbility $ Chara.job c)
    invokeTrap <- happens =<< evalWith m (parse' "100*(19-agi)/20")
    trap'      <- randomIn [Enemy.NoTrap .. Enemy.Alarm]
    let afterInspect = actionForTreasureChest con $ i:ps
    run $ if      successed  then events [Message $ inspectMessage (trap con)] afterInspect
          else if invokeTrap then invokingTrap con i
          else                    events [Message $ inspectMessage trap'] afterInspect
  where
    inspectMessage :: Enemy.Trap -> String
    inspectMessage trap = case trap of Enemy.NoTrap -> "No traps."
                                       _            -> show trap ++ "."


disarmTrap :: TreasureCondition -> GameMachine -> GameMachine
disarmTrap con afterNotDisarm = GameAuto $ do
    cmds <- cmdNumPartiesWhen $ bimap disarm' (not . isCantFight)
    run $ selectWhenEsc (Message "^#)Disarm\n^L)eave `[`E`S`C`]") $ (Key "l", afterNotDisarm, True)
                                                                  : cmds
  where
    disarm' p = GameAuto $ return (Ask "Input trap.\n(Empty to cancel.)" Nothing,
                                   \(Key s) -> if null s then afterNotDisarm else tryDisarm con s p afterNotDisarm)

tryDisarm :: TreasureCondition -> String -> PartyPos -> GameMachine -> GameMachine
tryDisarm con t i afterNotDisarm = GameAuto $ do
    c <- characterInPartyAt i
    m <- formulaMapS (Left c)
    let matchTrap = (toLower <$> show (trap con)) == (toLower <$> t)
    sucessDisarming <- happens =<< evalWith m (Chara.disarmTrapAbility $ Chara.job c)
    invokeTrap      <- happens =<< evalWith m (parse' "100*(20-agi)/20")
    run $ if      not matchTrap   then invokingTrap con i
          else if sucessDisarming then events [Message "Trap successfully disarmed!"] (getTreasures con)
          else if invokeTrap      then invokingTrap con i
          else                         events [Message "The trap was not disarmed."] afterNotDisarm


openTreasureChest :: TreasureCondition -> GameMachine -> GameMachine
openTreasureChest con afterNotOpen = GameAuto $ do
    cmds <- cmdNumPartiesWhen $ bimap open' (not . isCantFight)
    run $ selectWhenEsc (Message "^#)Open\n^L)eave `[`E`S`C`]") $ (Key "l", afterNotOpen, True)
                                                                : cmds
  where
    open' p = tryDisarm con "" p (openTreasureChest con afterNotOpen)


invokingTrap :: TreasureCondition -> PartyPos -> GameMachine
invokingTrap con i = GameAuto $ do
    cid         <- characterIDInPartyAt i
    (msg, eid') <- effectTrap cid $ trap con
    sortPartyAuto
    case eid' of
      Nothing  -> run $ events [Message msg] (getTreasures con)
      Just eid -> run $ events [Message msg] (whenAlarm con eid)


effectTrap :: CharacterID -> Enemy.Trap -> GameState (String, Maybe EnemyID)
effectTrap _ Enemy.NoTrap = return ("No traps were set.", Nothing)
effectTrap i Enemy.PoisonNeedle = do
    updateCharacterWith i (addPoison 1)
    return ("Ooops!! Poison Needle!!", Nothing)
effectTrap i Enemy.GasBomb = do
    ps <- party <$> world
    forM_ ps $ \i -> do
      c   <- characterByID i
      m   <- formulaMapS (Left c)
      hit <- happens =<< evalWith m (parse' "100*(20-luc)/20")
      when hit $ updateCharacterWith i (addPoison 1)
    return ("Ooops!! Gas Bomb!!", Nothing)
effectTrap i Enemy.CrossbowBolt = do
    floor <- (+1) . Maze.z <$> currentPosition
    dmg <- eval (parse' $ show floor ++ "d8")
    updateCharacterWith i (damageHp dmg)
    return ("Ooops!! Crossbow Bolt!!", Nothing)
effectTrap i Enemy.ExplodingBox = do
    ps    <- party <$> world
    floor <- (+1) . Maze.z <$> currentPosition
    forM_ ps $ \i -> do
      hit <- happens 75
      flg <- happens 66
      dmg1 <- eval (parse' $ show floor ++ "d5")
      dmg2 <- eval (parse' $ show floor ++ "d8")
      when hit $ updateCharacterWith i (damageHp $ if flg then dmg1 else dmg2)
    return ("Ooops!! Exploding Box!!", Nothing)
effectTrap i Enemy.Stunner = do
    updateCharacterWith i (addStatusError Paralysis)
    return ("Ooops!! Stunner!!", Nothing)
effectTrap i Enemy.Teleporter = do
    p     <- currentPosition
    (w,h) <- mazeSizeAt $ Maze.z p
    x'    <- randomIn [1..w]
    y'    <- randomIn [1..h]
    movePlace $ FindTreasureChest (p { Maze.x = x', Maze.y = y' }) False
    return ("Ooops!! Teleporter!!", Nothing)
effectTrap i Enemy.Alarm = do
    c    <- Maze.coordOf <$> currentPosition
    emap <- asks roomBattleMap
    case Map.lookup c emap of
      Nothing      -> return ("No traps were set.", Nothing)
      Just (_, es) -> do
        eid <- randomIn es
        return ("Ooops!! Alarm!!", Just eid)


getTreasures :: TreasureCondition -> GameMachine
getTreasures con = GameAuto $ do
    np <- length . party <$> world
    let gp   = dropGold con `div` np
        msg1 = [Message ("Each survivor got " ++ show gp ++ "G.P.") | gp > 0]
    msg2 <- fmap Message <$> divideItems (dropItems con)
    movePlace =<< FindTreasureChest <$> currentPosition <*> pure True
    run $ events (msg2 ++ msg1) (afterChest con)


divideItems :: [Int] -> GameState [String]
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
        return $ (Chara.name c' ++ " got " ++ Item.nameUndetermined idef ++ ".") : rest





