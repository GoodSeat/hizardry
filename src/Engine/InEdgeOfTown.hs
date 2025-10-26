module Engine.InEdgeOfTown (inEdgeOfTown) where

import Control.Monad (join)
import Control.Monad.State (modify, put, gets)
import Control.Monad.Reader (asks)
import Data.List (sort, sortOn)
import Data.Char (toLower)
import qualified Data.Map as Map

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
                         ,(Key "r", restartAnOutParty 0, True)
                         ,(Key "c", toCastle, True)
                         ,(Key "q", exitGame, True)]
  where
    msg = Message $ "^M)aze\n"
                 ++ "^T)raining Grounds\n"
                 ++ "^R)estart an \"OUT\" Party\n"
                 ++ "Return to the ^C)astle\n"
                 ++ "^Q)uit Game\n"

-- =======================================================================

restartAnOutParty :: Int -> GameMachine
restartAnOutParty page = GameAuto $ do
    cs  <- gets (fmap fst . inMazeMember)
    cs' <- mapM characterByID cs
    let ccs = filter ((> 0) . Character.hp . fst) $ zip cs' cs
        mxPage = max 0 ((length ccs - 1) `div` 10)
    if      null ccs      then run inEdgeOfTown
    else if page < 0      then run $ restartAnOutParty mxPage
    else if page > mxPage then run $ restartAnOutParty 0
    else do
      let msg = Message $ unlines (zipWith (++) (('^':) . (++")") <$> ms) (Character.toText 34 . fst <$> ccs)) ++
               "\n==========================(" ++ show (page+1) ++ "/" ++ show (mxPage+1) ++ ")=========================\n\n" ++
               "^A‾)Restart  ^N)ext list  ^P)revious list  ^L)eave `[`E`S`C`]\n"
          ts' = if null ccs then [] else take 10 $ drop (page*10) ccs 
          cmds = zip (Key <$> (fmap toLower <$> ms)) (restart . snd <$> ts')
      run $ selectEsc msg $ [(Key "l", inEdgeOfTown)
                            ,(Key "n", restartAnOutParty $ page + 1)
                            ,(Key "p", restartAnOutParty $ page - 1)
                            ] ++ cmds
  where
    ms = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J"]

restart :: CharacterID -> GameMachine
restart cid = GameAuto $ do
    cps <- gets inMazeMember
    let (_, p) = head $ filter ((== cid) . fst) cps
        cs  = filter ((== coordOf p) . coordOf . snd) cps
    cs1 <- mapM characterByID (fst <$> cs)
    let ccs  = zip cs1 (fst <$> cs)
        ccs1 = filter ((/= cid) . snd) $ filter ((>  0) . Character.hp . fst) ccs
        ccs2 = filter ((== 0) . Character.hp . fst) ccs
        ps = cid : take 5 (snd <$> (ccs1 ++ ccs2))
    modify $ \w -> w {
        party = ps
      , inTarvernMember = sort (inTarvernMember w ++ party w)
      , inMazeMember = filter ((`notElem` ps) . fst) (inMazeMember w)
      }
    run $ openCamp p

-- =======================================================================

enteringMaze :: GameMachine
enteringMaze = with [movePlace EnteringMaze] (events [msg] $ openCamp p)
  where
    msg = MessageTime (-1500) "\n\n  *** Entering Test Maze... *** \n\n\n" Nothing
    p   = Position { direction = N, x = 0, y = 0, z = 0 }

-- =======================================================================

inTrainingGrounds :: GameMachine
inTrainingGrounds = with [ movePlace TrainingGrounds
                         , modify $ \w -> w { party = [], inTarvernMember = sort (inTarvernMember w ++ party w) }]
                  $ selectEsc msg [(Key "l", inEdgeOfTown)
                                  ,(Key "c", createNewCharacter)
                                  ,(Key "s", showListOfCharacters 0)
                                  ,(Key "d", selectDeleteTargetCharacter 0)
                                  ,(Key "n", selectCharacterToChangeName 0)
                                  ,(Key "q", exitGame)]
  where
    msg = Message $ "^C)reate Character\n"
                 ++ "^S)how List of Characters\n"
                 ++ "^D)elete Character\n"
                 ++ "^N)ame Change of Character\n"
                 ++ "^J)ob Change of Character\n"
                 ++ "^R)eorder List\n"
                 ++ "^L)eave `[`E`S`C`]\n"

-- -----------------------------------------------------------------------

createNewCharacter :: GameMachine
createNewCharacter = GameAuto $
    return (Ask ">Input name of character. \n(Empty to cancel.)" Nothing,
           \(Key s') -> let s = filter (/= '\n') . filter (/= '\r') $ s' in
              if null s then inTrainingGrounds else GameAuto $ do
              isOK <- not <$> existSameName s
              run $ if isOK then selectRace s
                    else events [Message $ s ++ " is already exist."] createNewCharacter)

existSameName :: String -> GameState Bool
existSameName name = do
  w <- world
  let cids = inTarvernMember w ++ (fst <$> inMazeMember w)
  ns <- map Character.name <$> mapM characterByID cids
  return $ name `elem` ns

selectRace :: String -> GameMachine
selectRace name = GameAuto $ do
    ks <- asks racies
    let ts  = zipWith (++) (("  ^"++) . (++")") . show <$> [1..]) (Character.raceName <$> ks)
        cs  = zip (Key <$> (show <$> [1..])) (selectAlignment name <$> ks)
        msg = Message $ showCharacter name Nothing Nothing Nothing
                     ++ "\n=========================================================\n"
                     ++ ">Select race.(ESC to cancel)\n\n"
                     ++ unlines ts
    run $ select msg ((Key "\ESC", inTrainingGrounds) : cs)

selectAlignment :: String -> Character.Race -> GameMachine
selectAlignment name k = select msg [(Key "\ESC", inTrainingGrounds)
                                    ,(Key "g", determineParameter name k Character.G)
                                    ,(Key "n", determineParameter name k Character.N)
                                    ,(Key "e", determineParameter name k Character.E)]
  where
    msg = Message $ showCharacter name (Just k) Nothing Nothing
                 ++ "\n=========================================================\n"
                 ++ ">Select alignment. (ESC to cancel)\n\n"
                 ++ "  ^G)ood\n"
                 ++ "  ^N)eutral\n"
                 ++ "  ^E)vil"

determineParameter :: String -> Character.Race -> Character.Alignment -> GameMachine
determineParameter name k a = GameAuto $ do
    bns <- eval $ Character.initialBonus k
    run $ determineParameter' bns emptyParam name k a

determineParameter' :: Int -> Parameter -> String -> Character.Race -> Character.Alignment -> GameMachine
determineParameter' bns aps name k a = GameAuto $ do
    js <- asks (filter (isEnableJob a param) . jobs)
    let ibns = bns + totalParameter aps
        jts  = ("  *)"++) . Character.jobName <$> js
        msg  = Message $ showCharacter name (Just k) (Just a) Nothing
                      ++ "\n=========================================================\n"
                      ++ ">Select add parameter from bonus. ^R)eset\n\n"
                      ++ "  ^S)trength :" ++ rightTxt 4 (strength param) ++ "\n"
                      ++ "  ^I)Q       :" ++ rightTxt 4 (iq       param) ++ "\n"
                      ++ "  ^P)iety    :" ++ rightTxt 4 (piety    param) ++ "\n"
                      ++ "  ^V)itality :" ++ rightTxt 4 (vitality param) ++ "\n"
                      ++ "  ^A)gility  :" ++ rightTxt 4 (agility  param) ++ "\n"
                      ++ "  ^L)uck     :" ++ rightTxt 4 (luck     param) ++ "\n"
                      ++ "---------------------------------------------------------\n"
                      ++ "      Bonus :" ++ rightTxt 4 bns ++ " (`[`E`S`C`] to change bonus)\n\n"
                      ++ unlines jts
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
    param = sumParameter aps ips
    mps = Character.maxParam     k
    addParameter :: (Parameter -> Int) -> (Parameter -> Parameter) -> GameMachine
    addParameter paramOf addParam1 = GameAuto $ do
        let bns' = bns - 1
            aps' = addParam1 aps
        run $ if      paramOf aps' + paramOf ips > paramOf mps  then determineParameter' bns aps name k a
              else if bns' <= 0 || sumParameter aps' ips == mps then selectJob aps' name k a
              else                                                   determineParameter' bns' aps' name k a

selectJob :: Parameter -> String -> Character.Race -> Character.Alignment -> GameMachine
selectJob aps name k a = GameAuto $ do
    js <- asks (filter (isEnableJob a param) . jobs)
    if null js then run $ determineParameter' 0 aps name k a
    else do
      let jts = zipWith (++) (("  ^"++) . (++")") . show <$> [1..]) (Character.jobName <$> js)
          msg = Message $ showCharacter name (Just k) (Just a) Nothing
                       ++ "\n=========================================================\n"
                       ++ "\n\n" ++ showParameter param
                       ++ "---------------------------------------------------------\n"
                       ++ ">Select job. ^R)eset\n\n"
                       ++ unlines jts
          cmds = cmdNums (length js) $ \i -> makeCharacter param name k a (js !! (i-1))
      run $ select msg $ (Key "r", determineParameter' (totalParameter aps) emptyParam name k a) : cmds
  where
    ips = Character.initialParam k
    param = sumParameter aps ips

makeCharacter :: Parameter -> String -> Character.Race -> Character.Alignment -> Character.Job -> GameMachine
makeCharacter param name k a j = select msg [(Key "r", with [register] inTrainingGrounds)
                                            ,(Key "c", inTrainingGrounds)]
  where
    msg = Message $ showCharacter name (Just k) (Just a) (Just j)
                 ++ "\n=========================================================\n"
                 ++ "\n\n" ++ showParameter param
                 ++ "---------------------------------------------------------\n"
                 ++ "\n               ^R)egister  or  ^C)ancel \n\n"
    register :: GameState ()
    register = do
      let c = Character.Character {
                Character.name      = name
              , Character.race      = k
              , Character.age       = 10 -- TODO!
              , Character.days      = 10 -- TODO!
              , Character.lv        = 1
              , Character.exp       = 0
              , Character.gold      = 0

              , Character.job       = j
              , Character.alignment = a

              , Character.hp        = 0 -- MEMO:temporary value.
              , Character.maxhp     = 0 -- MEMO:temporary value.
              , Character.param     = param
              , Character.marks     = 0
              , Character.rips      = 0
              , Character.statusErrors = []
              , Character.paramDelta   = []

              , Character.items        = []
              , Character.equips       = []

              , Character.spells       = []
              , Character.mp           = (replicate 7 0, replicate 7 0)
              , Character.maxmp        = (replicate 7 0, replicate 7 0)
              }
      hp' <- join $ evalWith <$> formulaMapS (Left c) <*> pure (Character.hpFormula j)
      (sn, maxmp') <- learnSpellsAndMps c
      let c' = c { Character.maxhp = hp', Character.hp = hp',
                   Character.spells = sn, Character.maxmp = maxmp', Character.mp = maxmp' }
      w <- world
      let cmap = allCharacters w
          midn = maximum $ characterId . fst <$> Map.toList cmap
          nid  = CharacterID $ midn + 1
      put w { allCharacters = Map.insert nid c' cmap
            , inTarvernMember = sort (nid : inTarvernMember w) }


totalParameter :: Parameter -> Int
totalParameter param = strength param + iq param + piety param + vitality param + agility param + luck param

sumParameter :: Parameter -> Parameter -> Parameter
sumParameter p1 p2 = Parameter {
      strength = strength p1 + strength p2
    , iq       = iq       p1 + iq       p2
    , piety    = piety    p1 + piety    p2
    , vitality = vitality p1 + vitality p2
    , agility  = agility  p1 + agility  p2
    , luck     = luck     p1 + luck     p2
}

showCharacter :: String -> Maybe Character.Race -> Maybe Character.Alignment -> Maybe Character.Job -> String
showCharacter name k' a' j' = "\n    " ++ name ++ replicate (40 - length name) ' ' ++ kt ++ at ++ jt ++ "\n"
  where kt = case k' of Nothing -> "??"
                        Just k  -> take 2 (Character.raceName k)
        at = case a' of Nothing -> "??"
                        Just a  -> "-" ++ show a
        jt = case j' of Nothing -> "????"
                        Just j  -> "-" ++ take 3 (Character.jobName j)

showParameter :: Parameter -> String
showParameter param = "  Strength  :" ++ rightTxt 4 (strength param) ++ "\n"
                   ++ "  IQ        :" ++ rightTxt 4 (iq       param) ++ "\n"
                   ++ "  Piety     :" ++ rightTxt 4 (piety    param) ++ "\n"
                   ++ "  Vitality  :" ++ rightTxt 4 (vitality param) ++ "\n"
                   ++ "  Agility   :" ++ rightTxt 4 (agility  param) ++ "\n"
                   ++ "  Luck      :" ++ rightTxt 4 (luck     param) ++ "\n"

isEnableJob :: Character.Alignment -> Parameter -> Character.Job -> Bool
isEnableJob a param j = a `elem` Character.enableAlignments j
    && strength param >= strength (Character.needParameter j)
    && iq       param >= iq       (Character.needParameter j)
    && piety    param >= piety    (Character.needParameter j)
    && vitality param >= vitality (Character.needParameter j)
    && agility  param >= agility  (Character.needParameter j)
    && luck     param >= luck     (Character.needParameter j)

-- -----------------------------------------------------------------------

showListOfCharacters :: Int -> GameMachine
showListOfCharacters = cmdWithCharacterList ("Inspect", inspectCharacter)

inspectCharacter :: GameMachine -> CharacterID -> GameMachine
inspectCharacter h cid = selectEsc (ShowStatus cid msg SingleKey)
                                   [(Key "l", h)
-- TODO                            ,(Key "r", readSpell cid)
                                   ]
  where
    msg = "^R)ead Spell   ^L)eave `[`E`S`C`]"

selectDeleteTargetCharacter :: Int -> GameMachine
selectDeleteTargetCharacter = cmdWithCharacterList ("Delete", showDeleteTargetCharacter)

showDeleteTargetCharacter :: GameMachine -> CharacterID -> GameMachine
showDeleteTargetCharacter h cid = selectEsc (ShowStatus cid msg SingleKey)
                                   [(Key "n", h)
                                   ,(Key "y", with [deleteCharacter cid] h)
                                   ]
  where
    msg = "Are you sure? (his items are also lost)\n ^Y)es   ^N)o `[`E`S`C`]"


selectCharacterToChangeName :: Int -> GameMachine
selectCharacterToChangeName = cmdWithCharacterList ("Change Name", changeCharacterName)

changeCharacterName :: GameMachine -> CharacterID -> GameMachine
changeCharacterName h cid = GameAuto $
    return (Ask ">Input name of character. \n(Empty to cancel.)" Nothing,
           \(Key s') -> let s = filter (/= '\n') . filter (/= '\r') $ s' in
              if null s then h else GameAuto $ do
              isOK <- not <$> existSameName s
              run $ if isOK then with [changeName s] h
                    else events [Message $ s ++ " is already exist."] (changeCharacterName h cid))
  where
    changeName newName = do
        c <- characterByID cid
        updateCharacter cid $ c { Character.name = newName }

-- -----------------------------------------------------------------------

cmdWithCharacterList :: (String, GameMachine -> CharacterID -> GameMachine) -> Int -> GameMachine
cmdWithCharacterList cmd (-1) = GameAuto $ do
    mxPage <- lastPage
    run $ cmdWithCharacterList cmd mxPage
cmdWithCharacterList cmd page = GameAuto $ do
    mxPage <- lastPage
    cids <- take sizePage . drop (page * sizePage) . sortOn fst . Map.toList . allCharacters <$> world 
    if page > mxPage then run $ cmdWithCharacterList cmd 0
    else if null cids then run inTrainingGrounds
    else do
      let cst = zipWith (++) (("^"++) .(++")") . show <$> [1..]) (Character.name . snd <$> cids)
          msg = Message $ "^N)ext list  ^P)revious list  ^#)" ++ fst cmd ++"  ^L)eave `[`E`s`c`]"
                      ++ "\n\n-------------------------(" ++ show (page+1) ++ "/" ++ show (mxPage+1) ++ ")--------------------------\n\n"
                      ++ unlines cst
          cmds = cmdNums (length cids) (\i -> (snd cmd) (cmdWithCharacterList cmd page) $ (fst <$> cids) !! (i-1))
      run $ selectEsc msg $ (Key "l", inTrainingGrounds)
                          : (Key "n", cmdWithCharacterList cmd (page+1))
                          : (Key "p", cmdWithCharacterList cmd (page-1))
                          : cmds

sizePage :: Int
sizePage = 9

lastPage :: GameState Int
lastPage = flip div sizePage . flip (-) 1 . length . Map.toList . allCharacters <$> world


-- =======================================================================

exitGame :: GameMachine
exitGame = GameAuto $ return (Exit, const exitGame)




