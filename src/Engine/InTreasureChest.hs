module Engine.InTreasureChest
where
import Engine.GameAuto
import Data.World
import Engine.Utils
import Data.Primitive
import qualified Data.Characters as Chara
import qualified Data.Enemies as Enemy
import qualified Data.Items as Item
import Data.Formula (parse')
import Control.Monad (forM)
import Data.List (find)


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
                             [( Key "l"
                              , afterChest con
                              , True
                              )
                             ,( Key "i"
                              , inspectTreasureChest ps (trap con) (actionForTreasureChest con) (invokingTrap con)
                              , True
                              )
                             ]

inspectTreasureChest :: [PartyPos] -> Enemy.Trap -> ([PartyPos] -> GameMachine) -> (PartyPos -> GameMachine) -> GameMachine
inspectTreasureChest ps trap afterInspect whenTrapInvoking = GameAuto $ do
    np <- length . party <$> world
    run $ selectWhen (Message "#)Inspect\nL)eave")
          [(Key "l", afterInspect ps, True)
          ,(Key "1", inspect' F1, np >= 1)
          ,(Key "2", inspect' F2, np >= 2)
          ,(Key "3", inspect' F3, np >= 3)
          ,(Key "4", inspect' B4, np >= 4)
          ,(Key "5", inspect' B5, np >= 5)
          ,(Key "6", inspect' B6, np >= 6)
          ]
  where
    inspect' p = if p `elem` ps then events [Message "Already inspected."] $ inspectTreasureChest ps trap afterInspect whenTrapInvoking
                                else inspectTreasureChestBy p trap (afterInspect $ p:ps) whenTrapInvoking


inspectTreasureChestBy :: PartyPos -> Enemy.Trap -> GameMachine -> (PartyPos -> GameMachine) -> GameMachine
inspectTreasureChestBy i trap afterInspect whenTrapInvoking = GameAuto $ do
    c <- partyAt' i
    successed  <- happens =<< evalWith (formulaMapS c) (Chara.inspectTrapAbility $ Chara.job c)
    invokeTrap <- happens =<< evalWith (formulaMapS c) (parse' "100*(19-agi)/20")
    trap'      <- randomIn [Enemy.NoTrap .. Enemy.Alarm]
    run $ if      successed  then events [Message $ inspectMessage trap] afterInspect
          else if invokeTrap then whenTrapInvoking i
          else                    events [Message $ inspectMessage trap'] afterInspect
  where
    inspectMessage :: Enemy.Trap -> String
    inspectMessage trap = case trap of Enemy.NoTrap -> "No traps."
                                       _            -> show trap ++ "."


invokingTrap :: TreasureCondition -> PartyPos -> GameMachine
invokingTrap con i = GameAuto $ do
    cid <- partyAt i
    msg <- effectTrap cid $ trap con
    run $ events [Message msg] (getTreasures con)


effectTrap :: CharacterID -> Enemy.Trap -> GameState String
effectTrap _ Enemy.NoTrap = return "No traps were set."
effectTrap i Enemy.PoisonNeedle = do
    updateCharacterWith i (addPoison 1)
    return "Ooops!! Poison Needle!!"


getTreasures :: TreasureCondition -> GameMachine
getTreasures con = GameAuto $ do
    np <- length . party <$> world
    let gp   = dropGold con `div` np
        msg1 = [Message ("Each survivor got " ++ "G.P.") | gp > 0]
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
                




