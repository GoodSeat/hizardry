module Engine.InTreasureChest
where
import Control.Monad (forM, forM_, when)
import Control.Monad.Reader (asks)
import Engine.GameAuto
import Engine.Utils
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
    else selectWhen (BattleCommand "I)nspect\nD)isarm Trap\nO)pen\nL)eave")
                             [( Key "l", afterChest con                                       , True)
                             ,( Key "i", inspectTreasureChest ps con                          , True)
                             ,( Key "d", disarmTrap con (actionForTreasureChest con ps)       , True)
                             ,( Key "o", openTreasureChest con (actionForTreasureChest con ps), True)
                             ]

inspectTreasureChest :: [PartyPos] -> TreasureCondition -> GameMachine
inspectTreasureChest ps con = GameAuto $ do
    np <- length . party <$> world
    cs <- mapM characterOf . party =<< world
    run $ selectWhen (Message "#)Inspect\nL)eave")
          [(Key "l", actionForTreasureChest con ps, True)
          ,(Key "1", inspect' F1, np >= 1 && (not . isCantFight) (cs !! 0))
          ,(Key "2", inspect' F2, np >= 2 && (not . isCantFight) (cs !! 1))
          ,(Key "3", inspect' F3, np >= 3 && (not . isCantFight) (cs !! 2))
          ,(Key "4", inspect' B4, np >= 4 && (not . isCantFight) (cs !! 3))
          ,(Key "5", inspect' B5, np >= 5 && (not . isCantFight) (cs !! 4))
          ,(Key "6", inspect' B6, np >= 6 && (not . isCantFight) (cs !! 5))
          ]
  where
    inspect' p = if p `elem` ps then events [Message "Already inspected."] $ inspectTreasureChest ps con
                                else inspectTreasureChestBy p ps con

inspectTreasureChestBy :: PartyPos -> [PartyPos] -> TreasureCondition -> GameMachine
inspectTreasureChestBy i ps con = GameAuto $ do
    c <- partyAt' i
    successed  <- happens =<< evalWith (formulaMapS c) (Chara.inspectTrapAbility $ Chara.job c)
    invokeTrap <- happens =<< evalWith (formulaMapS c) (parse' "100*(19-agi)/20")
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
    np <- length . party <$> world
    cs <- mapM characterOf . party =<< world
    run $ selectWhen (Message "#)Disarm\nL)eave")
          [(Key "l", afterNotDisarm, True)
          ,(Key "1", disarm' F1, np >= 1 && (not . isCantFight) (cs !! 0))
          ,(Key "2", disarm' F2, np >= 2 && (not . isCantFight) (cs !! 1))
          ,(Key "3", disarm' F3, np >= 3 && (not . isCantFight) (cs !! 2))
          ,(Key "4", disarm' B4, np >= 4 && (not . isCantFight) (cs !! 3))
          ,(Key "5", disarm' B5, np >= 5 && (not . isCantFight) (cs !! 4))
          ,(Key "6", disarm' B6, np >= 6 && (not . isCantFight) (cs !! 5))
          ]
  where
    disarm' p = GameAuto $ return (Ask "Input trap.\n(Empty to cancel.)" Nothing,
                                   \(Key s) -> if null s then afterNotDisarm else tryDisarm con s p afterNotDisarm)

tryDisarm :: TreasureCondition -> String -> PartyPos -> GameMachine -> GameMachine
tryDisarm con t i afterNotDisarm = GameAuto $ do
    c <- partyAt' i
    let matchTrap = (toLower <$> show (trap con)) == (toLower <$> show t)
    sucessDisarming <- happens =<< evalWith (formulaMapS c) (Chara.disarmTrapAbility $ Chara.job c)
    invokeTrap      <- happens =<< evalWith (formulaMapS c) (parse' "100*(20-agi)/20")
    run $ if      not matchTrap   then invokingTrap con i
          else if sucessDisarming then events [Message "Trap successfully disarmed!"] (getTreasures con)
          else if invokeTrap      then invokingTrap con i
          else                         events [Message "The trap was not disarmed."] afterNotDisarm


openTreasureChest :: TreasureCondition -> GameMachine -> GameMachine
openTreasureChest con afterNotOpen = GameAuto $ do
    np <- length . party <$> world
    cs <- mapM characterOf . party =<< world
    run $ selectWhen (Message "#)Open\nL)eave")
          [(Key "l", afterNotOpen, True)
          ,(Key "1", open' F1, np >= 1 && (not . isCantFight) (cs !! 0))
          ,(Key "2", open' F2, np >= 2 && (not . isCantFight) (cs !! 1))
          ,(Key "3", open' F3, np >= 3 && (not . isCantFight) (cs !! 2))
          ,(Key "4", open' B4, np >= 4 && (not . isCantFight) (cs !! 3))
          ,(Key "5", open' B5, np >= 5 && (not . isCantFight) (cs !! 4))
          ,(Key "6", open' B6, np >= 6 && (not . isCantFight) (cs !! 5))
          ]
  where
    open' p = tryDisarm con "" p (openTreasureChest con afterNotOpen)


invokingTrap :: TreasureCondition -> PartyPos -> GameMachine
invokingTrap con i = GameAuto $ do
    cid         <- partyAt i
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
      c   <- characterOf i
      hit <- happens =<< evalWith (formulaMapS c) (parse' "100*(20-luc)/20")
      when hit $ updateCharacterWith i (addPoison 1)
    return ("Ooops!! Gas Bomb!!", Nothing)
effectTrap i Enemy.CrossbowBolt = do
    floor <- Maze.z <$> currentPosition 
    dmg <- eval (parse' $ show floor ++ "d8")
    updateCharacterWith i (damageHp dmg)
    return ("Ooops!! Crossbow Bolt!!", Nothing)
effectTrap i Enemy.ExplodingBox = do
    ps    <- party <$> world
    floor <- Maze.z <$> currentPosition 
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
    p  <- currentPosition
    x' <- randomIn [1..20]
    y' <- randomIn [1..20] -- TODO!:size is fixed when teleporter.
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
    ps  <- forM ids (\cid -> (,) cid <$> characterOf cid)
    case find (\(cid, c) -> length (Chara.items c) < 10) ps of
      Nothing         -> return []
      Just (cid', c') -> do
        let itms = Chara.items c'
        updateCharacter cid' (c' { Chara.items = itms ++ [ItemInf (ItemID i) False] })
        rest <- divideItems is
        idef <- itemByID (ItemID i)
        return $ (Chara.name c' ++ " got " ++ Item.nameUndetermined idef ++ ".") : rest
                




