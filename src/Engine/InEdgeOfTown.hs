module Engine.InEdgeOfTown (inEdgeOfTown, autoSaveToSlot0) where

import Control.Monad (join)
import Control.Monad.State (modify, put, gets)
import Control.Monad.Reader (asks)
import Data.List (sort, sortOn, elemIndex, intersperse)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import qualified Data.Map as Map

import Engine.GameAuto
import Engine.Utils
import Engine.InMaze
import Engine.CharacterAction (readSpell)
import Data.World
import Data.Maze
import Data.Primitive
import qualified Data.Characters as Character

inEdgeOfTown :: GameMachine
inEdgeOfTown = with [movePlace InEdgeOfTown] $ autoSaveToSlot0 $ GameAuto $ do
    notnull  <- not . null . party <$> world
    toCastle <- home
    run $ selectWhenEsc msg [(Key "c", toCastle, True)
                            ,(Key "m", enteringMaze, notnull)
                            ,(Key "t", inTrainingGrounds, True)
                            ,(Key "r", restartAnOutParty 0, True)
                            ,(Key "l", exitGame, True)
                            ,(Key "u", utilities, True)
                            ]
  where
    msg = message $ "^M)aze\n"
                 ++ "^T)raining Grounds\n"
                 ++ "^R)estart an \"OUT\" Party\n"
                 ++ "^U)tilities\n"
                 ++ "^L)eave Game\n"
                 ++ "Return to the ^C)astle `[`E`S`C`]\n"

-- =======================================================================

utilities :: GameMachine
utilities = selectEsc msg [(Key "l", inEdgeOfTown)
--                        ,(Key "c", config)
                          ,(Key "s", selectSaveSlot)
                          ,(Key "r", selectLoadSlot)
                          ]
  where
    msg = message $ "^C)onfig\n"
                 ++ "^S)ave backup\n"
                 ++ "^R)estore from backup\n"
                 ++ "^L)eave Utilities `[`E`S`C`]\n"

autoSaveToSlot0 :: GameMachine -> GameMachine
autoSaveToSlot0 = events [SaveGame 0 "AutoSave"]

selectSaveSlot :: GameMachine
selectSaveSlot = GameAuto $ do
    bs <- backUpSlotInfo <$> world
    let lst = zipWith (\i n -> "^" ++ show i ++ ")" ++ n) [1..9] (bs ++ repeat "")
    let cmds = cmdNums 9 (`inputSaveTag` msg)
        msg  = "Save to which slot (^1-^9)?\n ^L)eave `[`E`S`C`]\n\n ==================================================\n\n"
            ++ unlines lst
    run $ selectEsc (message msg) ((Key "l", utilities) : cmds)

inputSaveTag :: Int -> String -> GameMachine
inputSaveTag slot msg = GameAuto $
    return (askFlashAndMessage msg "\n  Tag? (empty to cancel)  \n " Nothing, \(Key s) -> events [SaveGame slot s | not (isNullKey s)] utilities)

selectLoadSlot :: GameMachine
selectLoadSlot = GameAuto $ do
    bs <- backUpSlotInfo <$> world
    let lst = zipWith (\i n -> "^" ++ show i ++ ")" ++ n) [1..9] (bs ++ repeat "")
    let cmds = cmdNums 9 (\i -> events [LoadGame i] utilities)
        msg  = message $ "Load from which slot (^1-^9)?\n ^L)eave `[`E`S`C`]\n\n ==================================================\n\n"
                      ++ unlines lst
    run $ selectEsc msg ((Key "l", utilities) : cmds)

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
      let msg = message $ unlines (zipWith (++) (('^':) . (++")") <$> ms) (Character.toText 34 . fst <$> ccs)) ++
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
      , inTavernMember = sort (inTavernMember w ++ party w)
      , inMazeMember = filter ((`notElem` ps) . fst) (inMazeMember w)
      }
    run $ openCamp p

-- =======================================================================

enteringMaze :: GameMachine
enteringMaze = with [movePlace EnteringMaze] (events [msg] $ openCamp p)
  where
    msg = messageTime (-1500) "\n\n  *** Entering Test Maze... *** \n\n\n" Nothing
    p   = Position { direction = N, x = 0, y = 0, z = 0 }

-- =======================================================================

inTrainingGrounds :: GameMachine
inTrainingGrounds = with [ movePlace TrainingGrounds
                         , modify $ \w -> w { party = [], inTavernMember = sort (inTavernMember w ++ party w) }]
                  $ selectEsc msg [(Key "l", inEdgeOfTown)
                                  ,(Key "c", createNewCharacter)
                                  ,(Key "s", showListOfCharacters 0)
                                  ,(Key "d", selectDeleteTargetCharacter 0)
                                  ,(Key "n", selectCharacterToChangeName 0)
                                  ,(Key "j", selectCharacterToChangeJob 0)
                                  ,(Key "r", selectReorderTargetCharacter 0)
                                  ,(Key "q", exitGame)]
  where
    msg = message $ "^C)reate Character\n"
                 ++ "^S)how List of Characters\n"
                 ++ "^D)elete Character\n"
                 ++ "^N)ame Change of Character\n"
                 ++ "^J)ob Change of Character\n"
                 ++ "^R)eorder List\n"
                 ++ "^L)eave `[`E`S`C`]\n"

-- -----------------------------------------------------------------------
-- Job Change
-- -----------------------------------------------------------------------

selectCharacterToChangeJob :: Int -> GameMachine
selectCharacterToChangeJob = cmdWithCharacterListOnlyIn ("Job Change", selectNewJob)

selectNewJob :: GameMachine -> CharacterID -> GameMachine
selectNewJob h cid = GameAuto $ do
    c <- characterByID cid
    allJobs <- asks jobs
    let availableJobs = filter (canChangeToJob c) allJobs
    if null availableJobs then
        run $ events [message "There are no jobs you can change to."] h
    else do
        let jobItems = zipWith (\i j -> (show i, Character.jobName j)) [1..] availableJobs
            jobCmds = zipWith (\i j -> (Key (show i), confirmChangeJob h cid j)) [1..] availableJobs
            msg' = message $ "Select new job for " ++ Character.name c ++ ".\n\n"
                         ++ unlines (map (\(i, name) -> "  ^" ++ i ++ ") " ++ name) jobItems)
                         ++ "\n^L)eave `[`E`S`C`]"
        run $ selectEsc msg' ((Key "l", h) : jobCmds)

canChangeToJob :: Character.Character -> Character.Job -> Bool
canChangeToJob c j =
    let currentJobName = Character.jobName (Character.job c)
        newJobName = Character.jobName j
    in currentJobName /= newJobName && isEnableJob (Character.alignment c) (Character.param c) j

confirmChangeJob :: GameMachine -> CharacterID -> Character.Job -> GameMachine
confirmChangeJob h cid newJob = GameAuto $ do
    c <- characterByID cid
    let msg' = message $ "Change " ++ Character.name c ++ "'s job to " ++ Character.jobName newJob ++ "?\n"
                      ++ "This will reset LV, EXP.\n\n"
                      ++ "^Y)es / ^N)o `[`E`S`C`]"
    run $ selectEsc msg' [(Key "n", h), (Key "y", with [doChangeJob cid newJob] h)]

doChangeJob :: CharacterID -> Character.Job -> GameState ()
doChangeJob cid newJob = do
    c <- characterByID cid
    updateCharacter cid $ c {
          Character.job   = newJob
        , Character.lv    = 1
        , Character.exp   = 0
        , Character.age   = Character.age c + 1
        , Character.param = Character.initialParam (Character.race c)
    }

-- -----------------------------------------------------------------------

createNewCharacter :: GameMachine
createNewCharacter = GameAuto $
    return (ask ">Input name of character. \n(Empty to cancel.)" Nothing,
           \(Key s') -> let s = filter (/= '\n') . filter (/= '\r') $ s' in
              if null s then inTrainingGrounds else GameAuto $ do
              isOK <- not <$> existSameName s
              run $ if isOK then selectRace s
                    else events [message $ s ++ " is already exist."] createNewCharacter)

existSameName :: String -> GameState Bool
existSameName name = do
  w <- world
  let cids = inTavernMember w ++ (fst <$> inMazeMember w)
  ns <- map Character.name <$> mapM characterByID cids
  return $ name `elem` ns

selectRace :: String -> GameMachine
selectRace name = GameAuto $ do
    ks <- asks racies
    let ts  = zipWith (++) (("  ^"++) . (++")") . show <$> [1..]) (Character.raceName <$> ks)
        cs  = zip (Key <$> (show <$> [1..])) (selectAlignment name <$> ks)
        msg = message $ showCharacter name Nothing Nothing Nothing
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
    msg = message $ showCharacter name (Just k) Nothing Nothing
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
        msg  = message $ showCharacter name (Just k) (Just a) Nothing
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
          msg = message $ showCharacter name (Just k) (Just a) Nothing
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
    msg = message $ showCharacter name (Just k) (Just a) (Just j)
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
          midn = maximum $ 0 : (characterId . fst <$> Map.toList cmap)
          nid  = CharacterID $ midn + 1
      put w { allCharacters = Map.insert nid c' cmap
            , inTavernMember = sort (nid : inTavernMember w) }


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
inspectCharacter h cid = selectEsc (showStatus cid msg)
                                   [(Key "l", h)
                                   ,(Key "r", readSpell (inspectCharacter h cid) cid)
                                   ]
  where
    msg = "^R)ead Spell   ^L)eave `[`E`S`C`]"

selectDeleteTargetCharacter :: Int -> GameMachine
selectDeleteTargetCharacter = cmdWithCharacterList ("Delete", showDeleteTargetCharacter)

showDeleteTargetCharacter :: GameMachine -> CharacterID -> GameMachine
showDeleteTargetCharacter h cid = selectEsc (showStatus cid msg)
                                   [(Key "n", h)
                                   ,(Key "y", with [deleteCharacter cid] h)
                                   ]
  where
    msg = "Are you sure? (his items are also lost)\n ^Y)es   ^N)o `[`E`S`C`]"


selectCharacterToChangeName :: Int -> GameMachine
selectCharacterToChangeName = cmdWithCharacterList ("Change Name", changeCharacterName)

changeCharacterName :: GameMachine -> CharacterID -> GameMachine
changeCharacterName h cid = GameAuto $
    return (ask ">Input name of character. \n(Empty to cancel.)" Nothing,
           \(Key s') -> let s = filter (/= '\n') . filter (/= '\r') $ s' in
              if null s then h else GameAuto $ do
              isOK <- not <$> existSameName s
              run $ if isOK then with [changeName s] h
                    else events [message $ s ++ " is already exist."] (changeCharacterName h cid))
  where
    changeName newName = do
        c <- characterByID cid
        updateCharacter cid $ c { Character.name = newName }

selectReorderTargetCharacter :: Int -> GameMachine
selectReorderTargetCharacter = cmdWithCharacterList ("Change Order", changeOrder)

changeOrder :: GameMachine -> CharacterID -> GameMachine
changeOrder _ cid = GameAuto $ do
    cs <- Map.toList . allCharacters <$> world
    let tos  = fromJust (elemIndex cid $ fst <$> cs)
        fromPage = div tos sizePage
    run $ cmdWithCharacterList ("Insert", insertCharacter cid) fromPage

insertCharacter :: CharacterID -> GameMachine -> CharacterID -> GameMachine
insertCharacter cid _ cidTo = GameAuto $ do
    cs <- Map.toList . allCharacters <$> world
    let cis  = fst <$> cs
        toi  = fromJust (elemIndex cidTo cis)
        cis' = filter (/= cid) cis
        cist = filter (== cid) cis
        cisn = take toi cis' ++ cist ++ drop toi cis'
        conv n = CharacterID $ fromJust (elemIndex n cisn) + 1
        conv2 (a, b) = (conv a, b)
    w <- world
    put $ w { party          = conv <$> party w
            , inTavernMember = conv <$> inTavernMember w
            , inMazeMember   = conv2 <$> inMazeMember w
            , allCharacters  = Map.fromList (conv2 <$> cs)
            }
    let toPage = div toi sizePage
    run $ selectReorderTargetCharacter toPage

-- -----------------------------------------------------------------------

cmdWithCharacterListOnly :: (CharacterID -> Bool) -> (String, GameMachine -> CharacterID -> GameMachine) -> Int -> GameMachine
cmdWithCharacterListOnly be cmd (-1) = GameAuto $ do
    mxPage <- lastPage
    run $ cmdWithCharacterListOnly be cmd mxPage
cmdWithCharacterListOnly be cmd page = GameAuto $ do
    mxPage <- lastPage
    cids   <- take sizePage . drop (page * sizePage) . sortOn fst . Map.toList . allCharacters <$> world 
    inCids <- inTavernMember <$> world 
    if page > mxPage then run $ cmdWithCharacterListOnly be cmd 0
    else if null cids then run inTrainingGrounds
    else do
      let toT (cid, c) = Character.toText 30 c ++ rightString  4 (if cid `elem` inCids then "IN" else "OUT")
      let cst'= zip (zipWith (++) ((++")") . show <$> [1..]) (toT <$> cids)) (be . fst <$> cids)
          cst = fmap (\(l, valid) -> if valid then '^' : l else '`' : intersperse '`' l) cst'
          msg = message $ "^N)ext list  ^P)revious list  ^#)" ++ fst cmd ++"  ^L)eave `[`E`s`c`]"
                      ++ "\n\n-------------------------(" ++ show (page+1) ++ "/" ++ show (mxPage+1) ++ ")--------------------------\n\n"
                      ++ unlines cst
          cmds = zip (be . fst <$> cids) (cmdNums (length cids) (\i -> (snd cmd) (cmdWithCharacterListOnly be cmd page) $ (fst <$> cids) !! (i-1)))

      run $ selectEsc msg $ (Key "l", inTrainingGrounds)
                          : (Key "n", cmdWithCharacterListOnly be cmd (page+1))
                          : (Key "p", cmdWithCharacterListOnly be cmd (page-1))
                          : (snd <$> filter fst cmds)

cmdWithCharacterList :: (String, GameMachine -> CharacterID -> GameMachine) -> Int -> GameMachine
cmdWithCharacterList = cmdWithCharacterListOnly $ const True

cmdWithCharacterListOnlyIn :: (String, GameMachine -> CharacterID -> GameMachine) -> Int -> GameMachine
cmdWithCharacterListOnlyIn cmd page = GameAuto $ do
    inCids <- inTavernMember <$> world 
    run $ cmdWithCharacterListOnly (`elem` inCids) cmd page

sizePage :: Int
sizePage = 9

lastPage :: GameState Int
lastPage = flip div sizePage . flip (-) 1 . length . Map.toList . allCharacters <$> world


-- =======================================================================

exitGame :: GameMachine
exitGame = GameAuto $ return (Exit, const exitGame)




