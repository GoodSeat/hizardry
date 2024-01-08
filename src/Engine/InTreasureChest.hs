module Engine.InTreasureChest
where
import Engine.GameAuto
import Data.World
import Engine.Utils
import Data.Primitive
import qualified Data.Characters as Chara
import qualified Data.Enemies as Enemy
import qualified Data.Items as Item
import qualified Data.Maze as Maze
import Data.Formula (parse')
import Control.Monad (forM, forM_, when)
import Data.List (find)
import Data.Char (toLower)


data TreasureCondition = TreasureCondition {
      afterChest   :: GameMachine
    , dropGold     :: Int
    , dropItems    :: [Int]
    , trap         :: Enemy.Trap
    }

actionForTreasureChest :: TreasureCondition
                       -> [PartyPos]  -- ^ already inspect characters.
                       -> GameMachine
actionForTreasureChest con ps = selectWhen (BattleCommand "I)nspect\nD)isarm Trap\nO)pen\nL)eave")
                             [( Key "l", afterChest con                                       , True)
                             ,( Key "i", inspectTreasureChest ps con                          , True)
                             ,( Key "d", disarmTrap con (actionForTreasureChest con ps)       , True)
                             ,( Key "o", openTreasureChest con (actionForTreasureChest con ps), True)
                             ]

inspectTreasureChest :: [PartyPos] -> TreasureCondition -> GameMachine
inspectTreasureChest ps con = GameAuto $ do
    np <- length . party <$> world
    run $ selectWhen (Message "#)Inspect\nL)eave")
          [(Key "l", actionForTreasureChest con ps, True)
          ,(Key "1", inspect' F1, np >= 1)
          ,(Key "2", inspect' F2, np >= 2)
          ,(Key "3", inspect' F3, np >= 3)
          ,(Key "4", inspect' B4, np >= 4)
          ,(Key "5", inspect' B5, np >= 5)
          ,(Key "6", inspect' B6, np >= 6)
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
    run $ selectWhen (Message "#)Disarm\nL)eave")
          [(Key "l", afterNotDisarm, True)
          ,(Key "1", disarm' F1, np >= 1)
          ,(Key "2", disarm' F2, np >= 2)
          ,(Key "3", disarm' F3, np >= 3)
          ,(Key "4", disarm' B4, np >= 4)
          ,(Key "5", disarm' B5, np >= 5)
          ,(Key "6", disarm' B6, np >= 6)
          ]
  where
    disarm' p = GameAuto $ return (SpellCommand "Input trap.\n(Empty to cancel.)",
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
    run $ selectWhen (Message "#)Open\nL)eave")
          [(Key "l", afterNotOpen, True)
          ,(Key "1", open' F1, np >= 1)
          ,(Key "2", open' F2, np >= 2)
          ,(Key "3", open' F3, np >= 3)
          ,(Key "4", open' B4, np >= 4)
          ,(Key "5", open' B5, np >= 5)
          ,(Key "6", open' B6, np >= 6)
          ]
  where
    open' p = tryDisarm con "" p (openTreasureChest con afterNotOpen)


invokingTrap :: TreasureCondition -> PartyPos -> GameMachine
invokingTrap con i = GameAuto $ do
    cid <- partyAt i
    msg <- effectTrap cid $ trap con
    sortPartyAuto
    run $ events [Message msg] (getTreasures con)


effectTrap :: CharacterID -> Enemy.Trap -> GameState String
effectTrap _ Enemy.NoTrap = return "No traps were set."
effectTrap i Enemy.PoisonNeedle = do
    updateCharacterWith i (addPoison 1)
    return "Ooops!! Poison Needle!!"
effectTrap i Enemy.GasBomb = do
    ps <- party <$> world
    forM_ ps $ \i -> do
      c   <- characterOf i
      hit <- happens =<< evalWith (formulaMapS c) (parse' "100*(20-luc)/20")
      when hit $ updateCharacterWith i (addPoison 1)
    return "Ooops!! Gas Bomb!!"
effectTrap i Enemy.CrossbowBolt = do
    floor <- Maze.z <$> currentPosition 
    dmg <- eval (parse' $ show floor ++ "d8")
    updateCharacterWith i (damageHp dmg)
    return "Ooops!! Crossbow Bolt!!"


getTreasures :: TreasureCondition -> GameMachine
getTreasures con = GameAuto $ do
    np <- length . party <$> world
    let gp   = dropGold con `div` np
        msg1 = [Message ("Each survivor got " ++ show gp ++ "G.P.") | gp > 0]
    msg2 <- fmap Message <$> divideItems (dropItems con)
    run $ events (msg1 ++ msg2) (afterChest con)


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
        return $ (Chara.name c' ++ " got " ++ Item.nameUndetermined idef) : rest
                




