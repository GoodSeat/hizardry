module Engine.InEdgeOfTown (inEdgeOfTown, autoSaveToSlot0) where

import PreludeL
import Control.Monad (join)
import Control.Monad.State (modify, put, gets)
import Control.Monad.Reader (asks)
import Data.List (sort, sortOn, elemIndex, intersperse)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import qualified Data.Map as Map

import Engine.GameAuto
import Engine.Utils
import Engine.InMaze
import Engine.CharacterAction (readSpell)
import Data.World
import Data.Maze
import Data.Primitive
import qualified Data.Spells as Spell
import qualified Data.Characters as Character

inEdgeOfTown :: GameMachine
inEdgeOfTown = with [movePlace InEdgeOfTown] $ autoSaveToSlot0 $ GameAuto $ do
    notnull  <- not . null . party <$> world
    toCastle <- home
    msg      <- switchL (EnJp msgENG msgJPN)
    run $ selectWhenEsc msg [(Key "c", toCastle, True)
                            ,(Key "m", enteringMaze, notnull)
                            ,(Key "t", inTrainingGrounds, True)
                            ,(Key "r", restartAnOutParty 0, True)
                            ,(Key "l", exitGame, True)
                            ,(Key "u", utilities, True)
                            ]
  where
    msgENG = message $ "^M)aze\n"
                    ++ "^T)raining Grounds\n"
                    ++ "^R)estart an \"OUT\" Party\n"
                    ++ "^U)tilities\n"
                    ++ "^L)eave Game\n"
                    ++ "Return to the ^C)astle `[`E`S`C`]\n"
    msgJPN = message $ "^M)迷宮\n"
                    ++ "^T)訓練場\n"
                    ++ "^R)迷宮内パーティの冒険の再開\n"
                    ++ "^U)ユーティリティ\n"
                    ++ "^L)ゲームを終える\n"
                    ++ "^C)城下町に戻る `[`E`S`C`]\n"

-- =======================================================================

utilities :: GameMachine
utilities = GameAuto $ do
    msg <- switchL (EnJp msgENG msgJPN)
    run $ selectEsc msg [(Key "l", inEdgeOfTown)
                        ,(Key "c", config)
                        ,(Key "s", selectSaveSlot)
                        ,(Key "r", selectLoadSlot)
                        ]
  where
    msgENG = message $ "^C)onfig\n"
                    ++ "^S)ave backup\n"
                    ++ "^R)estore from backup\n"
                    ++ "^L)eave Utilities `[`E`S`C`]\n"
    msgJPN = message $ "^C)コンフィグ\n"
                    ++ "^S)バックアップの作成\n"
                    ++ "^R)バックアップからの復元\n"
                    ++ "^L)戻る `[`E`S`C`]\n"

config :: GameMachine
config = GameAuto $ do
    w <- world
    let tE = effectDumapic    $ worldOption w
        tM = minimapType      $ worldOption w
        tH = hpHealType       $ worldOption w
        tI = ignoreAlignment  $ worldOption w
        tS = switchSE         $ worldOption w
        tB = switchBGM        $ worldOption w
        tW = waitTimeInBattle $ worldOption w
        tL = language         $ worldOption w
        isENG = tL == ENG
    let eToT Spell.OnlyCoord      = if isENG then "Show coordinate"          else "座標表示"
        eToT Spell.ViewMap        = if isENG then "Show map"                 else "地図表示"
        mToT Disable              = if isENG then "OFF"                      else "非表示"
        mToT Normal               = if isENG then "ON"                       else "表示"
        mToT AlwaysN              = if isENG then "ON(Disorientation lost)"  else "表示(方向感覚無効)"
        hToT Classic              = if isENG then "No recovery"              else "回復しない"
        hToT CureWhenInn          = if isENG then "Recovered at Inn"         else "宿泊時に全回復"
        hToT CureWhenReturnCastle = if isENG then "Recovered upon Returning" else "城帰還時に全回復"
        iToT True                 = if isENG then "Ignore alignment"         else "可能"
        iToT False                = if isENG then "Consider alignment"       else "不可"
        iToS True                 = if isENG then "ON"                       else "有効"
        iToS False                = if isENG then "OFF"                      else "無効"
        iToB True                 = if isENG then "ON"                       else "有効"
        iToB False                = if isENG then "OFF"                      else "無効"
    let msgENG = "\n^C)hange language               : " ++ lToT tL
              ++ "\n^E)ffect of dumapic             : " ++ eToT tE
              ++ "\n^M)inimap visible               : " ++ mToT tM
              ++ "\n^H)p heal type when return      : " ++ hToT tH
              ++ "\n^I)gnore alignment in tavern    : " ++ iToT tI
              ++ "\n^S)E                            : " ++ iToS tS
              ++ "\n^B)GM                           : " ++ iToB tB
              ++ "\n^W)ait time of battle message   : " ++ show tW ++ " ms"
              ++ "\n    (* set 0 to disable auto progress)"
              ++ "\n                            "
              ++ "\n^L)eave  `[`E`S`C`]         "
    let msgJPN = "\n^C)言語設定                     : " ++ lToT tL
              ++ "\n^E)位置判別呪文の効果           : " ++ eToT tE
              ++ "\n^M)ミニマップの表示             : " ++ mToT tM
              ++ "\n^H)城帰還時のHP回復             : " ++ hToT tH
              ++ "\n^I)酒場パーティ編成時の属性混合 : " ++ iToT tI
              ++ "\n^S)効果音の有無                 : " ++ iToS tS
              ++ "\n^B)音楽の有無                   : " ++ iToB tB
              ++ "\n^W)戦闘時のテキスト自動進行時間 : " ++ show tW ++ " ms"
              ++ "\n    (※ 0 を設定すると、自動進行しなくなります)"
              ++ "\n                            "
              ++ "\n^L)離れる  `[`E`S`C`]       "
    msg  <- switchL (EnJp msgENG msgJPN)
    txt1 <- switchL (EnJp "Input wait time (ms)." "進行時間(ms)を入力してください。") 
    let changeL = do
            let ts = [ENG, JPN]
            put $ w { worldOption = (worldOption w) { language = next ts tL } }
        changeE = do
            ts <- asks (enableEffectDumapic . scenarioOption)
            put $ w { worldOption = (worldOption w) { effectDumapic = next ts tE } }
        changeM = do
            ts <- asks (enableMinimapType . scenarioOption)
            put $ w { worldOption = (worldOption w) { minimapType = next ts tM } }
        changeH = do
            let ts = [Classic, CureWhenInn, CureWhenReturnCastle]
            put $ w { worldOption = (worldOption w) { hpHealType = next ts tH } }
        changeI = put $ w { worldOption = (worldOption w) { ignoreAlignment = not tI } }
        changeS = put $ w { worldOption = (worldOption w) { switchSE        = not tS } }
        changeB = put $ w { worldOption = (worldOption w) { switchBGM       = not tB } }
        changeW :: Int -> GameState ()
        changeW t = put $ w { worldOption = (worldOption w) { waitTimeInBattle = abs t } }
        inputW = GameAuto $ return (askFlashAndMessage msg txt1 Nothing
                                   ,\(Key s) -> case readMaybe s of Nothing -> config
                                                                    Just t  -> with [changeW t] config)
    run $ selectEsc (message msg) [(Key "l", utilities)
                                  ,(Key "c", with [changeL] config)
                                  ,(Key "e", with [changeE] config)
                                  ,(Key "m", with [changeM] config)
                                  ,(Key "h", with [changeH] config)
                                  ,(Key "i", with [changeI] config)
                                  ,(Key "s", with [changeS] config)
                                  ,(Key "b", with [changeB] config)
                                  ,(Key "w", inputW)
                                  ]
  where
    lToT ENG = "English"
    lToT JPN = "日本語"

    next :: Eq a => [a] -> a -> a
    next as = next' (head as) as
      where
        next' a []     _ = a
        next' a [b]    _ = a
        next' a (b:bs) c = if b == c then head bs else next' a bs c

autoSaveToSlot0 :: GameMachine -> GameMachine
autoSaveToSlot0 = events [SaveGame 0 "AutoSave"]

selectSaveSlot :: GameMachine
selectSaveSlot = GameAuto $ do
    bs <- backUpSlotInfo <$> world
    txt1 <- switchL (EnJp "Save to which slot (^1-^9)?  ^L)eave `[`E`S`C`]" "保存先スロットを指定 (^1-^9)?  ^L)離れる `[`E`S`C`]")
    let lst = zipWith (\i n -> "^" ++ show i ++ ")" ++ n) [1..9] (bs ++ repeat "")
    let cmds = cmdNums 9 (`inputSaveTag` msg)
        msg  = txt1 ++ "\n\n ==================================================\n\n" ++ unlines lst
    run $ selectEsc (message msg) ((Key "l", utilities) : cmds)

inputSaveTag :: Int -> String -> GameMachine
inputSaveTag slot msg = GameAuto $ do
    txt1 <- switchL (EnJp "Tag? (Empty to cancel)" "タグを入力 (空文字でキャンセル)") 
    return (askFlashAndMessage msg txt1 Nothing, \(Key s) -> events [SaveGame slot s | not (isNullKey s)] utilities)

selectLoadSlot :: GameMachine
selectLoadSlot = GameAuto $ do
    bs <- backUpSlotInfo <$> world
    txt1 <- switchL (EnJp "Load from which slot (^1-^9)?  ^L)eave `[`E`S`C`]" "読み込むスロットを指定 (^1-^9)?  ^L)離れる `[`E`S`C`]")
    let lst = zipWith (\i n -> "^" ++ show i ++ ")" ++ n) [1..9] (bs ++ repeat "")
    let cmds = cmdNums 9 (\i -> events [LoadGame i] utilities)
        msg  = message $ txt1 ++ "\n\n ==================================================\n\n" ++ unlines lst
    run $ selectEsc msg ((Key "l", utilities) : cmds)

-- =======================================================================

restartAnOutParty :: Int -> GameMachine
restartAnOutParty page = GameAuto $ do
    cs  <- gets (fmap fst . inMazeMember)
    cs' <- mapM characterByID cs
    txt1 <- switchL (EnJp "Nobody is in the maze." "迷宮内には誰もいない。")
    txt2 <- switchL (EnJp "^A‾)Restart  ^N)ext list  ^P)revious list  ^L)eave `[`E`S`C`]\n"
                          "^A‾)冒険の再開  ^N)次のリスト  ^P)前のリスト  ^L)離れる `[`E`S`C`]\n")
    let ccs = filter ((> 0) . Character.hp . fst) $ zip cs' cs
        mxPage = max 0 ((length ccs - 1) `div` 10)
    if      null ccs      then run $ events [Resume $ changeFlashTime txt1 (-1500)] inEdgeOfTown
    else if page < 0      then run $ restartAnOutParty mxPage
    else if page > mxPage then run $ restartAnOutParty 0
    else do
      let msg = message $ unlines (zipWith (++) (('^':) . (++")") <$> ms) (Character.toText 34 . fst <$> ccs)) ++
               "\n==========================(" ++ show (page+1) ++ "/" ++ show (mxPage+1) ++ ")=========================\n\n" ++
               txt2
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
enteringMaze = GameAuto $ do
    txt <- asks enteringMazeMessage
    let msg = messageTime (-1500) txt Nothing
    run $ with [movePlace EnteringMaze] (events [msg] $ openCamp p)
  where
    p   = Position { direction = N, x = 0, y = 0, z = 0 }

-- =======================================================================

inTrainingGrounds :: GameMachine
inTrainingGrounds = GameAuto $ do
    msg <- switchL (EnJp msgENG msgJPN)
    run $ with [ movePlace TrainingGrounds, modify $ \w -> w { party = [], inTavernMember = sort (inTavernMember w ++ party w) }]
        $ selectEsc msg [(Key "l", inEdgeOfTown)
                        ,(Key "n", createNewCharacter)
                        ,(Key "s", showListOfCharacters 0)
                        ,(Key "d", selectDeleteTargetCharacter 0)
                        ,(Key "r", selectCharacterToChangeName 0)
                        ,(Key "c", selectCharacterToChangeJob 0)
                        ,(Key "o", selectReorderTargetCharacter 0)
                        ,(Key "q", exitGame)]
  where
    msgENG = message $ "^Create N)ew Character\n"
                    ++ "^S)how List of Characters\n"
                    ++ "^D)elete Character\n"
                    ++ "^R)ename Character\n"
                    ++ "^C)lass Change of Character\n"
                    ++ "^Change O)rder List\n"
                    ++ "^L)eave `[`E`S`C`]\n"
    msgJPN = message $ "^N)キャラクターを新規作成する\n"
                    ++ "^S)キャラクターの一覧を見る\n"
                    ++ "^D)キャラクターを削除する\n"
                    ++ "^R)キャラクターの名前を変える\n"
                    ++ "^C)キャラクターのクラスを変更する\n"
                    ++ "^O)キャラクターの一覧を並び替える\n"
                    ++ "^L)離れる `[`E`S`C`]\n"

-- -----------------------------------------------------------------------
-- Class Change
-- -----------------------------------------------------------------------

selectCharacterToChangeJob :: Int -> GameMachine
selectCharacterToChangeJob = cmdWithCharacterListOnlyIn (EnJp "Class Change" "クラスの変更", selectNewJob)

selectNewJob :: GameMachine -> CharacterID -> GameMachine
selectNewJob h cid = GameAuto $ do
    c <- characterByID cid
    l  <- language . worldOption <$> world
    allJobs <- asks jobs
    let availableJobs = filter (canChangeToJob c) allJobs
    if null availableJobs then do
        msg <- switchL $ EnJp  "There are no classes you can change to." "就けるクラスがありません。"
        run $ events [message msg] h
    else do
        let jobItems = zipWith (\i j -> (show i, switchL' l (Character.jobName j))) [1..] availableJobs
            jobCmds = zipWith (\i j -> (Key (show i), confirmChangeJob h cid j)) [1..] availableJobs
            msgEng = "Select new class for " ++ Character.name c ++ ".\n\n"
                  ++ unlines (map (\(i, name) -> "  ^" ++ i ++ ") " ++ name) jobItems)
                  ++ "\n^L)eave `[`E`S`C`]"
            msgJpn = Character.name c ++ " の新しいクラスを選択してください。" ++ ".\n\n"
                  ++ unlines (map (\(i, name) -> "  ^" ++ i ++ ") " ++ name) jobItems)
                  ++ "\n^L)離れる `[`E`S`C`]"
        msg' <- message <$> switchL (EnJp msgEng msgJpn)
        run $ selectEsc msg' ((Key "l", h) : jobCmds)

canChangeToJob :: Character.Character -> Character.Job -> Bool
canChangeToJob c j =
    let currentJobName = Character.jobName (Character.job c)
        newJobName = Character.jobName j
    in currentJobName /= newJobName && isEnableJob (Character.alignment c) (Character.param c) j

confirmChangeJob :: GameMachine -> CharacterID -> Character.Job -> GameMachine
confirmChangeJob h cid newJob = GameAuto $ do
    c <- characterByID cid
    j <- switchL $ Character.jobName newJob
    let msgEng = "Change " ++ Character.name c ++ "'s class to " ++ j ++ "?\n"
               ++ "This will reset LV, EXP.\n\n"
               ++ "^Y)es / ^N)o `[`E`S`C`]"
    let msgJpn = Character.name c ++ " のクラスを " ++ j ++ " に変更しますか?\n"
               ++ "(Lvと経験値がリセットされます)\n\n"
               ++ "^Y)はい / ^N)いいえ `[`E`S`C`]"
    msg' <- message <$> switchL (EnJp msgEng msgJpn)
    run $ selectEsc msg' [(Key "n", h), (Key "y", with [doChangeJob cid newJob] h)]

doChangeJob :: CharacterID -> Character.Job -> GameState ()
doChangeJob cid newJob = do
    c <- characterByID cid
    updateCharacter cid $ c {
          Character.job    = newJob
        , Character.lv     = 1
        , Character.exp    = 0
        , Character.age    = Character.age c + 1
        , Character.param  = Character.initialParam (Character.race c)
        , Character.equips = []
    }

-- -----------------------------------------------------------------------

createNewCharacter :: GameMachine
createNewCharacter = GameAuto $ do
    msg <- switchL $ EnJp ">Input character's name. \n(Empty to cancel.)" ">キャラクターの名前を入力してください。 \n(空文字でキャンセル)"
    err <- switchL $ EnJp " already exists." " は既に存在します。"
    return (ask msg Nothing,
           \(Key s') -> let s = filter (/= '\n') . filter (/= '\r') $ s' in
              if null s then inTrainingGrounds else GameAuto $ do
              isOK <- not <$> existSameName s
              run $ if isOK then selectRace s
                    else events [message $ s ++ err] createNewCharacter)

existSameName :: String -> GameState Bool
existSameName name = do
  w <- world
  let cids = inTavernMember w ++ (fst <$> inMazeMember w)
  ns <- map Character.name <$> mapM characterByID cids
  return $ name `elem` ns

selectRace :: String -> GameMachine
selectRace name = GameAuto $ do
    ks  <- asks racies
    txt <- switchL $ EnJp ">Select race.(ESC to cancel)\n\n" ">種族を選択してください。(ESC でキャンセル)\n\n"
    l   <- language . worldOption <$> world
    let ts  = zipWith (++) (("  ^"++) . (++")") . show <$> [1..]) (switchL' l . Character.raceName <$> ks)
        cs  = zip (Key <$> (show <$> [1..])) (selectAlignment name <$> ks)
        msg = message $ showCharacter name Nothing Nothing Nothing
                     ++ "\n=========================================================\n"
                     ++ txt
                     ++ unlines ts
    run $ select msg ((Key "\ESC", inTrainingGrounds) : cs)

selectAlignment :: String -> Character.Race -> GameMachine
selectAlignment name k = GameAuto $ do
    msg <- switchL $ EnJp msgEng msgJpn
    run $ select msg [(Key "\ESC", inTrainingGrounds)
                     ,(Key "g", determineParameter name k Character.G)
                     ,(Key "n", determineParameter name k Character.N)
                     ,(Key "e", determineParameter name k Character.E)]
  where
    msgEng = message $ showCharacter name (Just k) Nothing Nothing
                    ++ "\n=========================================================\n"
                    ++ ">Select alignment. (ESC to cancel)\n\n"
                    ++ "  ^G)ood\n"
                    ++ "  ^N)eutral\n"
                    ++ "  ^E)vil"
    msgJpn = message $ showCharacter name (Just k) Nothing Nothing
                    ++ "\n=========================================================\n"
                    ++ ">属性を選択してください。 (ESC でキャンセル)\n\n"
                    ++ "  ^G)善\n"
                    ++ "  ^N)中立\n"
                    ++ "  ^E)悪"

determineParameter :: String -> Character.Race -> Character.Alignment -> GameMachine
determineParameter name k a = GameAuto $ do
    bns <- eval $ Character.initialBonus k
    run $ determineParameter' bns emptyParam name k a

determineParameter' :: Int -> Parameter -> String -> Character.Race -> Character.Alignment -> GameMachine
determineParameter' bns aps name k a = GameAuto $ do
    js <- asks (filter (isEnableJob a param) . jobs)
    l  <- language . worldOption <$> world
    let ibns = bns + totalParameter aps
        jts  = ("  *)"++) . switchL' l . Character.jobName <$> js
        txtEng =  "\n=========================================================\n"
               ++ ">Select add parameter from bonus. ^R)eset\n\n"
               ++ "  ^S)trength :" ++ rightTxt 4 (strength param) ++ "\n"
               ++ "  ^I)Q       :" ++ rightTxt 4 (iq       param) ++ "\n"
               ++ "  ^P)iety    :" ++ rightTxt 4 (piety    param) ++ "\n"
               ++ "  ^V)itality :" ++ rightTxt 4 (vitality param) ++ "\n"
               ++ "  ^A)gility  :" ++ rightTxt 4 (agility  param) ++ "\n"
               ++ "  ^L)uck     :" ++ rightTxt 4 (luck     param) ++ "\n"
               ++ "---------------------------------------------------------\n"
               ++ "      Bonus :" ++ rightTxt 4 bns ++ " (`[`E`S`C`] to change bonus)\n\n"
        txtJpn =  "\n=========================================================\n"
               ++ ">ボーナスを振り分けてください。 ^R)リセット\n\n"
               ++ "  ^S)力      :" ++ rightTxt 4 (strength param) ++ "\n"
               ++ "  ^I)知恵    :" ++ rightTxt 4 (iq       param) ++ "\n"
               ++ "  ^P)信仰心  :" ++ rightTxt 4 (piety    param) ++ "\n"
               ++ "  ^V)体力    :" ++ rightTxt 4 (vitality param) ++ "\n"
               ++ "  ^A)素早さ  :" ++ rightTxt 4 (agility  param) ++ "\n"
               ++ "  ^L)運の良さ:" ++ rightTxt 4 (luck     param) ++ "\n"
               ++ "---------------------------------------------------------\n"
               ++ "     ボーナス:" ++ rightTxt 4 bns ++ " (`[`E`S`C`] でボーナスを変更)\n\n"
        msg  = message $ showCharacter name (Just k) (Just a) Nothing
                      ++ switchL' l (EnJp txtEng txtJpn)
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
    l  <- language . worldOption <$> world
    if null js then run $ determineParameter' 0 aps name k a
    else do
      let jts = zipWith (++) (("  ^"++) . (++")") . show <$> [1..]) (switchL' l . Character.jobName <$> js)
          msg = message $ showCharacter name (Just k) (Just a) Nothing
                       ++ "\n=========================================================\n"
                       ++ "\n\n" ++ showParameter param l
                       ++ "---------------------------------------------------------\n"
                       ++ switchL' l (EnJp ">Select class. ^R)eset\n\n" ">クラスを選択してください。 ^R)リセット\n\n" )
                       ++ unlines jts
          cmds = cmdNums (length js) $ \i -> makeCharacter param name k a (js !! (i-1))
      run $ select msg $ (Key "r", determineParameter' (totalParameter aps) emptyParam name k a) : cmds
  where
    ips = Character.initialParam k
    param = sumParameter aps ips

makeCharacter :: Parameter -> String -> Character.Race -> Character.Alignment -> Character.Job -> GameMachine
makeCharacter param name k a j = GameAuto $ do
    l  <- language . worldOption <$> world
    let msg = message $ showCharacter name (Just k) (Just a) (Just j)
                     ++ "\n=========================================================\n"
                     ++ "\n\n" ++ showParameter param l
                     ++ "---------------------------------------------------------\n"
                     ++ switchL' l (EnJp "\n               ^R)egister  or  ^C)ancel \n\n" "\n               ^R)登録  or  ^C)キャンセル \n\n")
    run $ select msg [(Key "r", with [register] inTrainingGrounds)
                     ,(Key "c", inTrainingGrounds)]
  where
    register :: GameState ()
    register = do
      age <- randomIn [16, 16, 16, 16, 17]
      day <- randomIn [0..364]
      let c = Character.Character {
                Character.name      = name
              , Character.race      = k
              , Character.age       = age
              , Character.days      = day
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
                        Just k  -> take 2 (Character.raceID k)
        at = case a' of Nothing -> "??"
                        Just a  -> "-" ++ show a
        jt = case j' of Nothing -> "????"
                        Just j  -> "-" ++ take 3 (Character.jobID j)

showParameter :: Parameter -> Language -> String
showParameter param l = switchL' l $ EnJp eng jpn
  where
    eng = "  Strength  :" ++ rightTxt 4 (strength param) ++ "\n"
       ++ "  IQ        :" ++ rightTxt 4 (iq       param) ++ "\n"
       ++ "  Piety     :" ++ rightTxt 4 (piety    param) ++ "\n"
       ++ "  Vitality  :" ++ rightTxt 4 (vitality param) ++ "\n"
       ++ "  Agility   :" ++ rightTxt 4 (agility  param) ++ "\n"
       ++ "  Luck      :" ++ rightTxt 4 (luck     param) ++ "\n"
    jpn = "  力        :" ++ rightTxt 4 (strength param) ++ "\n"
       ++ "  知恵      :" ++ rightTxt 4 (iq       param) ++ "\n"
       ++ "  信仰心    :" ++ rightTxt 4 (piety    param) ++ "\n"
       ++ "  体力      :" ++ rightTxt 4 (vitality param) ++ "\n"
       ++ "  素早さ    :" ++ rightTxt 4 (agility  param) ++ "\n"
       ++ "  運の良さ  :" ++ rightTxt 4 (luck     param) ++ "\n"

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
showListOfCharacters = cmdWithCharacterList (EnJp "Inspect" "調査", inspectCharacter)

inspectCharacter :: GameMachine -> CharacterID -> GameMachine
inspectCharacter h cid = GameAuto $ do
    msg <- switchL $ EnJp msgEng msgJpn
    run $ selectEsc (showStatus cid msg)
                    [(Key "l", h)
                    ,(Key "r", readSpell (inspectCharacter h cid) cid)
                    ]
  where
    msgEng = "^R)ead Spell   ^L)eave `[`E`S`C`]"
    msgJpn = "^R)呪文書を読む  ^L)離れる `[`E`S`C`]"

selectDeleteTargetCharacter :: Int -> GameMachine
selectDeleteTargetCharacter = cmdWithCharacterList (EnJp "Delete" "削除", showDeleteTargetCharacter)

showDeleteTargetCharacter :: GameMachine -> CharacterID -> GameMachine
showDeleteTargetCharacter h cid = GameAuto $ do
    msg <- switchL $ EnJp msgEng msgJpn
    run $ selectEsc (showStatus cid msg)
                    [(Key "n", h)
                    ,(Key "y", with [deleteCharacter cid] h)
                    ]
  where
    msgEng = "Are you sure? (Their items will also be lost)\n ^Y)es   ^N)o `[`E`S`C`]"
    msgJpn = "本当によろしいですか? (所持するアイテムも失われます)\n ^Y)はい   ^N)いいえ `[`E`S`C`]"


selectCharacterToChangeName :: Int -> GameMachine
selectCharacterToChangeName = cmdWithCharacterList (EnJp "Change Name" "名前の変更", changeCharacterName)

changeCharacterName :: GameMachine -> CharacterID -> GameMachine
changeCharacterName h cid = GameAuto $ do
    msg1 <- switchL $ EnJp ">Input character's name. \n(Empty to cancel.)" ">キャラクターの名前を入力してください。 \n(空文字でキャンセル)"
    msg2 <- switchL $ EnJp " already exists." " は既に存在します。"
    return (ask msg1 Nothing,
           \(Key s') -> let s = filter (/= '\n') . filter (/= '\r') $ s' in
              if null s then h else GameAuto $ do
              isOK <- not <$> existSameName s
              run $ if isOK then with [changeName s] h
                    else events [message $ s ++ msg2] (changeCharacterName h cid))
  where
    changeName newName = do
        c <- characterByID cid
        updateCharacter cid $ c { Character.name = newName }

selectReorderTargetCharacter :: Int -> GameMachine
selectReorderTargetCharacter = cmdWithCharacterList (EnJp "Change Order" "並び替え", changeOrder)

changeOrder :: GameMachine -> CharacterID -> GameMachine
changeOrder _ cid = GameAuto $ do
    cs <- Map.toList . allCharacters <$> world
    let tos  = fromJust (elemIndex cid $ fst <$> cs)
        fromPage = div tos sizePage
    run $ cmdWithCharacterList (EnJp "Insert" "挿入", insertCharacter cid) fromPage

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

cmdWithCharacterListOnly :: (CharacterID -> Bool) -> (LanguageSet String, GameMachine -> CharacterID -> GameMachine) -> Int -> GameMachine
cmdWithCharacterListOnly be cmd (-1) = GameAuto $ do
    mxPage <- lastPage
    run $ cmdWithCharacterListOnly be cmd mxPage
cmdWithCharacterListOnly be cmd page = GameAuto $ do
    mxPage <- lastPage
    cids   <- take sizePage . drop (page * sizePage) . sortOn fst . Map.toList . allCharacters <$> world 
    inCids <- inTavernMember <$> world 
    txt    <- switchL $ fst cmd
    msg1   <- switchL $ EnJp ("^N)ext list  ^P)revious list  ^#)" ++ txt ++"  ^L)eave `[`E`s`c`]")
                             ("^N)次のリスト ^P)前のリスト ^#)" ++ txt ++"  ^L)離れる `[`E`s`c`]")
    if page > mxPage then run $ cmdWithCharacterListOnly be cmd 0
    else if null cids then run inTrainingGrounds
    else do
      let toT (cid, c) = Character.toText 30 c ++ rightString  4 (if cid `elem` inCids then "IN" else "OUT")
      let cst'= zip (zipWith (++) ((++")") . show <$> [1..]) (toT <$> cids)) (be . fst <$> cids)
          cst = fmap (\(l, valid) -> if valid then '^' : l else '`' : intersperse '`' l) cst'
          msg = message $ msg1
                      ++ "\n\n-------------------------(" ++ show (page+1) ++ "/" ++ show (mxPage+1) ++ ")--------------------------\n\n"
                      ++ unlines cst
          cmds = zip (be . fst <$> cids) (cmdNums (length cids) (\i -> (snd cmd) (cmdWithCharacterListOnly be cmd page) $ (fst <$> cids) !! (i-1)))

      run $ selectEsc msg $ (Key "l", inTrainingGrounds)
                          : (Key "n", cmdWithCharacterListOnly be cmd (page+1))
                          : (Key "p", cmdWithCharacterListOnly be cmd (page-1))
                          : (snd <$> filter fst cmds)

cmdWithCharacterList :: (LanguageSet String, GameMachine -> CharacterID -> GameMachine) -> Int -> GameMachine
cmdWithCharacterList = cmdWithCharacterListOnly $ const True

cmdWithCharacterListOnlyIn :: (LanguageSet String, GameMachine -> CharacterID -> GameMachine) -> Int -> GameMachine
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




