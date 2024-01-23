module Engine.InEdgeOfTown
where

import Control.Monad.State (modify)
import Control.Monad.Reader (asks)
import Data.List (sort)

import Engine.GameAuto
import Engine.Utils
import Engine.InMaze
import Data.World
import Data.Maze
import Data.Primitive
import qualified Data.Characters as Character

inEdgeOfTown :: GameMachine
inEdgeOfTown = GameAuto $ do
    movePlace InEdgeOfTown
    notnull  <- not . null . party <$> world
    toCastle <- home
    run $ selectWhen msg [(Key "m", enteringMaze, notnull)
                         ,(Key "t", inTrainingGrounds, True)
                         ,(Key "r", toCastle, True)
                         ,(Key "q", exitGame, True)]
  where
    msg = Message $ "M)aze\n"
                 ++ "T)raining Grounds\n"
                 ++ "R)eturn to the Castle\n"
                 ++ "Q)uit Game\n"

-- =======================================================================

enteringMaze :: GameMachine
enteringMaze = GameAuto $ movePlace EnteringMaze >> run (events [msg] $ openCamp p)
  where
    msg = MessageTime (-1500) "\n\n  *** Entering Test Maze... *** \n\n\n" Nothing
    p   = Position { direction = N, x = 1, y = 1, z = 0 }

-- =======================================================================

inTrainingGrounds :: GameMachine
inTrainingGrounds = GameAuto $ do
    movePlace TrainingGrounds
    modify $ \w -> w { party = [], inTarvernMember = sort (inTarvernMember w ++ party w) }
    run $ selectEsc msg [(Key "l", inEdgeOfTown)
                        ,(Key "c", createNewCharacter)
                        ,(Key "q", exitGame)]
  where
    msg = Message $ "C)reate Character\n"
                 ++ "S)how List of Characters\n"
                 ++ "D)elete Character\n"
                 ++ "N)ame Change of Character\n"
                 ++ "J)ob Change of Character\n"
                 ++ "R)eorder List\n"
                 ++ "L)eave [ESC]\n"

createNewCharacter :: GameMachine
createNewCharacter = GameAuto $
    return (Ask "Input name of character. \n(Empty to cancel.)" Nothing,
           \(Key s) -> if null s then inTrainingGrounds else GameAuto $ do
              isOK <- not <$> existSameName s
              run $ if isOK then selectKind s
                    else events [Message $ s ++ " is already exist."] createNewCharacter)
  where
    existSameName :: String -> GameState Bool
    existSameName name = do
      w <- world
      let cids = inTarvernMember w ++ (fst <$> inMazeMember w)
      ns <- map Character.name <$> mapM characterByID cids
      return $ name `elem` ns

selectKind :: String -> GameMachine
selectKind name = GameAuto $ do
    ks <- asks kinds
    let ts  = zipWith (++) ((++ ")") . show <$> [1..]) (Character.kindName <$> ks)
        cs  = zip (Key <$> (show <$> [1..])) (selectAlignment name <$> ks)
        msg = Message $ showCharacter name Nothing Nothing Nothing
                     ++ "\n=========================================================\n"
                     ++ "Select kind.(ESC to cancel)\n\n"
                     ++ unlines ts
    run $ select msg ((Key "\ESC", inTrainingGrounds) : cs)

selectAlignment :: String -> Character.Kind -> GameMachine
selectAlignment name k = select msg [(Key "\ESC", inTrainingGrounds)
                                    ,(Key "g", determineParameter name k Character.G)
                                    ,(Key "n", determineParameter name k Character.N)
                                    ,(Key "e", determineParameter name k Character.E)]
  where
    msg = Message $ showCharacter name (Just k) Nothing Nothing
                 ++ "\n=========================================================\n"
                 ++ "Select alignment. (ESC to cancel)\n\n"
                 ++ "G)ood\n"
                 ++ "N)eutral\n"
                 ++ "E)vil"

determineParameter :: String -> Character.Kind -> Character.Alignment -> GameMachine
determineParameter name k a = GameAuto $ do
    bns <- eval $ Character.initialBonus k
    run $ determineParameter' bns emptyParam name k a

determineParameter' :: Int -> Parameter -> String -> Character.Kind -> Character.Alignment -> GameMachine
determineParameter' bns aps name k a = GameAuto $ do
    let ibns = bns + strength aps + iq aps + piety aps + vitality aps + agility aps + luck aps
        msg  = Message $ showCharacter name (Just k) (Just a) Nothing
                      ++ "\n=========================================================\n"
                      ++ "Select add parameter from bonus. R)eset\n\n"
                      ++ "  S)trength :" ++ rightTxt 4 (strength ips + strength aps) ++ "\n"
                      ++ "  I)Q       :" ++ rightTxt 4 (iq       ips + iq       aps) ++ "\n"
                      ++ "  P)iety    :" ++ rightTxt 4 (piety    ips + piety    aps) ++ "\n"
                      ++ "  V)itality :" ++ rightTxt 4 (vitality ips + vitality aps) ++ "\n"
                      ++ "  A)gility  :" ++ rightTxt 4 (agility  ips + agility  aps) ++ "\n"
                      ++ "  L)uck     :" ++ rightTxt 4 (luck     ips + luck     aps) ++ "\n"
                      ++ "---------------------------------------------------------\n"
                      ++ "      Bonus :" ++ rightTxt 4 bns ++ " ([ESC] to change bonus)\n"
    run $ select msg [(Key "\ESC", determineParameter name k a)
                     ,(Key "r"   , determineParameter' ibns emptyParam name k a)
                     ,(Key "s"   , addParameter strength (\p -> p { strength = strength p + 1 }) )
                     ,(Key "i"   , addParameter iq       (\p -> p { iq       = iq       p + 1 }) )
                     ,(Key "p"   , addParameter piety    (\p -> p { piety    = piety    p + 1 }) )
                     ,(Key "v"   , addParameter vitality (\p -> p { vitality = vitality p + 1 }) )
                     ,(Key "a"   , addParameter agility  (\p -> p { agility  = agility  p + 1 }) )
                     ,(Key "l"   , addParameter luck     (\p -> p { luck     = luck     p + 1 }) )
                     ]
  where
    ips = Character.initialParam k
    mps = Character.maxParam     k
    sumParameter :: Parameter -> Parameter -> Parameter
    sumParameter p1 p2 = Parameter {
          strength = strength p1 + strength p2
        , iq       = iq       p1 + iq       p2
        , piety    = piety    p1 + piety    p2
        , vitality = vitality p1 + vitality p2
        , agility  = agility  p1 + agility  p2
        , luck     = luck     p1 + luck     p2
    }
    addParameter :: (Parameter -> Int) -> (Parameter -> Parameter) -> GameMachine
    addParameter paramOf addParam1 = GameAuto $ do
        let bns' = bns - 1
            aps' = addParam1 aps
        run $ if      paramOf aps' + paramOf ips > paramOf mps  then determineParameter' bns aps name k a
              else if bns' <= 0 || sumParameter aps' ips == mps then selectJob aps' name k a
              else                                                   determineParameter' bns' aps' name k a

selectJob :: Parameter -> String -> Character.Kind -> Character.Alignment -> GameMachine
selectJob aps name k a = events [msg] inTrainingGrounds
  where
    msg = Message $ showCharacter name (Just k) (Just a) Nothing
                 ++ "\n=========================================================\n"
                 ++ "TODO: not implemented."

showCharacter :: String -> Maybe Character.Kind -> Maybe Character.Alignment -> Maybe Character.Job -> String
showCharacter name k' a' j' = "\n    " ++ name ++ replicate (40 - length name) ' ' ++ kt ++ at ++ jt ++ "\n"
  where kt = case k' of Nothing -> "??"
                        Just k  -> take 2 (Character.kindName k)
        at = case a' of Nothing -> "??"
                        Just a  -> "-" ++ show a
        jt = case j' of Nothing -> "????"
                        Just j  -> "-" ++ take 3 (Character.jobName j)

-- =======================================================================

exitGame :: GameMachine
exitGame = GameAuto $ return (Exit, const exitGame)




