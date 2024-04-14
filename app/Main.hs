module Main where

import System.IO (getChar, hSetBuffering, stdin, BufferMode(..), hReady)
import System.Console.ANSI (clearScreen, clearLine, hideCursor, showCursor, setCursorPosition, cursorUp)
import System.Random
import System.Directory
import qualified Data.Map as Map
import Data.Maybe (maybe, catMaybes, isJust, isNothing, fromJust)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (void, when)

import Engine.GameAuto
import Engine.InCastle
import Data.Primitive
import Data.World
import Data.Maze
import Data.Formula
import qualified Data.Characters as Character
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell
import qualified Data.Items as Item
import qualified Data.GameEvent as Ev

import Control.CUI
import UI.CuiRender

import qualified SampleScenario.Spells as SampleSpells
import qualified SampleScenario.Items as SampleItems
import qualified SampleScenario.Enemies as SampleEnemies


-- note
-- * game over
-- *   search other parties
-- * items
-- *   sp
-- * shop
-- *   remove cursed item.
-- * temple
-- * training ground
-- * room battle
-- *   treasure chest
-- * secret door
-- * other spells
-- * other events

-- * scenario parser, save data parser.
-- *   hashable-1.4.1.0 [Data.Hashable] hash:: a -> Int
-- *   zip compression with secret keyword. using another exe? deflate?


main :: IO ()
main = do
    let param = Parameter {
          strength = 12
        , iq       = 10
        , piety    = 10
        , vitality = 10
        , agility  = 10
        , luck     = 10
        }
        fighter = Character.Job {
          Character.jobName              = "Fighter"
        , Character.enableAlignments     = [Character.G, Character.N, Character.E]
        , Character.enableBattleCommands = [
              Character.Fight
            , Character.Parry
            , Character.Spell
            , Character.UseItem
            , Character.Run
          ]
        , Character.inspectTrapAbility = parse' "agi"
        , Character.disarmTrapAbility  = parse' "(lv-7)*100/70"
        , Character.needParameter = Parameter {
              strength = 11
            , iq       = 0
            , piety    = 0
            , vitality = 0
            , agility  = 0
            , luck     = 0
          }
        , Character.baseWeaponAttr = Item.WeaponAttr {
              Item.targetF       = [L1, L2]
            , Item.targetB       = []
            , Item.damage        = read "2d2"
            , Item.doubleLabels  = []
            , Item.attrLabels    = []
            , Item.addStatusErrors = []
            , Item.atackMessages = []
          }
        , Character.fightTryCount = read "min(lv/5+1,10)"
        , Character.fightHitBonus = read "lv/3+2"
        , Character.baseAC        = read "10"
        , Character.lvupExps      = [
            1000,724,1248,2152,3710,6397,11029,19015,32785,56526,97458,168031,289709
          ]
        , Character.hpFormula     = read "(lv)d10 + lv*max(vit-15,min(-(vit=5)+vit-5,0)))"
        , Character.mpFormula     = ([], [])
        , Character.learningSpells= []
        }
        priest = Character.Job {
          Character.jobName              = "Priest"
        , Character.enableAlignments     = [Character.G, Character.N]
        , Character.enableBattleCommands = [
              Character.Fight
            , Character.Parry
            , Character.Spell
            , Character.UseItem
            , Character.Run
          ]
        , Character.inspectTrapAbility = parse' "agi"
        , Character.disarmTrapAbility  = parse' "(lv-7)*100/70"
        , Character.needParameter = Parameter {
              strength = 0
            , iq       = 0
            , piety    = 11
            , vitality = 0
            , agility  = 0
            , luck     = 0
          }
        , Character.baseWeaponAttr = Item.WeaponAttr {
              Item.targetF       = [L1, L2]
            , Item.targetB       = []
            , Item.damage        = read "2d2"
            , Item.doubleLabels  = []
            , Item.attrLabels    = []
            , Item.addStatusErrors = []
            , Item.atackMessages = []
          }
        , Character.fightTryCount = read "1"
        , Character.fightHitBonus = read "lv/3+2"
        , Character.baseAC        = read "10"
        , Character.lvupExps      = [
            1050,760,1310,2259,3895,6715,11578,19962,34417,59343,102307,176397,304132
          ]
        , Character.hpFormula     = read "(lv)d8 + lv*max(vit-15,min(-(vit=5)+vit-5,0)))"
        , Character.mpFormula     = ( replicate 7 (read "0")
                                    , read "min(9,max(1,lv*2-mlv*4+pie/6+1d3))"
                                    : replicate 6 (read "min(9,lv*2-mlv*4+pie/6+1d3)"))
        , Character.learningSpells= [(read "max(1,lv*2-4+1d(pie/5))", SpellID <$> [111..115])
                                    ,(read "lv*2- 8+1d(pie/5)"      , SpellID <$> [121..124])
                                    ,(read "lv*2-12+1d(pie/5)"      , SpellID <$> [131..134])
                                    ,(read "lv*2-16+1d(pie/5)"      , SpellID <$> [141..144])
                                    ,(read "lv*2-20+1d(pie/5)"      , SpellID <$> [151..156])
                                    ,(read "lv*2-24+1d(pie/5)"      , SpellID <$> [161..164])
                                    ,(read "lv*2-28+1d(pie/5)"      , SpellID <$> [171..172])
                                    ]
        }
        thief = Character.Job {
          Character.jobName              = "Thief"
        , Character.enableAlignments     = [Character.E, Character.N]
        , Character.enableBattleCommands = [
              Character.Fight
            , Character.Parry
            , Character.UseItem
            , Character.Run
            ]
        , Character.inspectTrapAbility = parse' "min(agi*6, 95)"
        , Character.disarmTrapAbility  = parse' "(lv-7+50)*100/70"
        , Character.needParameter = Parameter {
              strength = 0
            , iq       = 0
            , piety    = 0
            , vitality = 0
            , agility  = 11
            , luck     = 0
           }
        , Character.baseWeaponAttr = Item.WeaponAttr {
              Item.targetF       = [L1, L2]
            , Item.targetB       = []
            , Item.damage        = read "2d2"
            , Item.doubleLabels  = []
            , Item.attrLabels    = []
            , Item.addStatusErrors = []
            , Item.atackMessages = []
          }
        , Character.fightTryCount = read "min(lv/5+1,10)"
        , Character.fightHitBonus = read "lv/5"
        , Character.baseAC        = read "10"
        , Character.lvupExps      = [
            900,651,1123,1936,3338,5755,9922,17107,29495,50854,87679,151171,260639
          ]
        , Character.hpFormula     = read "(lv)d6 + lv*max(vit-15,min(-(vit=5)+vit-5,0)))"
        , Character.mpFormula     = ([], [])
        , Character.learningSpells= []
        }
    let bonus = parse' "min(60, 4+1d5+max(0,1d10-9)*10+max(0,1d100-99)*20+max(0,1d1000-999)*30)"
        human = Character.Kind {
            Character.kindName = "Human"
          , Character.initialParam = Parameter {
              strength = 8
            , iq       = 8
            , piety    = 8
            , vitality = 8
            , agility  = 8
            , luck     = 8
          }
          , Character.maxParam = Parameter {
              strength = 18
            , iq       = 18
            , piety    = 18
            , vitality = 18
            , agility  = 18
            , luck     = 18
          }
          , Character.initialBonus = bonus
        }
        elf = Character.Kind {
            Character.kindName = "Elf"
          , Character.initialParam = Parameter {
              strength =  7
            , iq       = 10
            , piety    = 10
            , vitality =  6
            , agility  =  9
            , luck     =  6
          }
          , Character.maxParam = Parameter {
              strength = 17
            , iq       = 20
            , piety    = 20
            , vitality = 16
            , agility  = 19
            , luck     = 16
          }
          , Character.initialBonus = bonus
        }


    let testChara1 = Character.Character {
          Character.name     = "FIG1"
        , Character.kind     = human
        , Character.age      = 18
        , Character.days     = 0

        , Character.lv       = 1
        , Character.exp      = 40000000
        , Character.gold     = 1000

        , Character.job      = fighter
        , Character.alignment= Character.G

        , Character.hp       = 12
        , Character.maxhp    = 20
        , Character.param    = param
        , Character.marks    = 0
        , Character.rips     = 0
        , Character.statusErrors = []
        , Character.paramDelta = []

        , Character.items    = [ItemInf (ItemID 1) True
                               ,ItemInf (ItemID 1) False
                               ,ItemInf (ItemID 4) True
                               ,ItemInf (ItemID 5) True
                               ,ItemInf (ItemID 11) True
                               ,ItemInf (ItemID 12) False
                               ,ItemInf (ItemID 13) False
                               ,ItemInf (ItemID 103) True
                               ]
        , Character.equips   = []

        , Character.spells   = []
        , Character.mp       = (replicate 7 0, replicate 7 0)
        , Character.maxmp    = (replicate 7 0, replicate 7 0)
        }
        testChara2 = testChara1 {
          Character.name     = "FIG2"
        , Character.hp       = 126
        , Character.maxhp    = 148
        , Character.lv       = 15
        , Character.statusErrors = [] -- [Poison 5]
        , Character.paramDelta = []
        }
        testChara3 = testChara1 {
          Character.name     = "PRI1"
        , Character.kind     = elf
        , Character.hp       = 34
        , Character.maxhp    = 48
        , Character.lv       = 1
        , Character.statusErrors = []
        , Character.paramDelta = []

        , Character.job      = priest
        , Character.alignment= Character.N
        , Character.spells   = [SpellID 11, SpellID 13, SpellID 14, 
                                SpellID 112, SpellID 114 
                               ]
        , Character.items    = [ItemInf (ItemID 2) True, ItemInf (ItemID 2) False]
        , Character.mp       = (replicate 7 5, replicate 7 5)
        , Character.maxmp    = (replicate 7 4, replicate 7 4)
        }
        testChara4 = testChara1 {
          Character.name     = "THI1"
        , Character.hp       = 104
        , Character.maxhp    = 108
        , Character.lv       = 5
        , Character.statusErrors = []
        , Character.paramDelta = []

        , Character.job      = thief
        , Character.alignment= Character.N
        , Character.spells   = []
        , Character.items    = [ItemInf (ItemID 2) True
                               ,ItemInf (ItemID 2) False
                               ,ItemInf (ItemID 103) True
                               ,ItemInf (ItemID 104) True
                               ]
        }
    --gen <- getStdGen
    let gen = mkStdGen 0 
    let w = World {
        randomGen       = gen
      , guideOn         = True
      , statusOn        = True
      , worldOption     = WorldOption {
          effectDumapic = Spell.ViewMap
        , minimapType   = Normal
        }

      , party           = []
      , place           = InCastle
      , roomBattled     = []
      , partyLight      = 0
      , partyLight'     = 0
      , partyParamDelta = []

      , visitHitory     = Map.empty

      , inTarvernMember = [CharacterID 1, CharacterID 2, CharacterID 3, CharacterID 4]
      , inMazeMember    = []
      , shopItems       = Map.fromList [ (ItemID 1, 10), (ItemID 2, 2), (ItemID 3, 2)
                                       , (ItemID 11, 3), (ItemID 12, 3), (ItemID 13, 3)
                                       , (ItemID 14, 3), (ItemID 15, 3), (ItemID 16, 3)
                                       , (ItemID 17, 3)
                                       ]
      , allCharacters   = Map.fromList [
                              (CharacterID 1, testChara1)
                            , (CharacterID 2, testChara2)
                            , (CharacterID 3, testChara3)
                            , (CharacterID 4, testChara4)
                            ]
      , sceneTrans      = id
      , eventFlags      = repeat 0

      , debugMode       = True -- MEMO:forDebug
      , debugMessage    = []
      }
    let option = ScenarioOption {
          enableEffectDumapic = [Spell.OnlyCoord, Spell.ViewMap]
        , enableMinimapType   = [Disable, Normal, AlwaysN]
        }
        scenario = Scenario {
              scenarioOption = option
            , scenarioHome   = inCastle
            , kinds          = [human, elf]
            , jobs           = [fighter, priest, thief]
            --, mazes          = [("B1F", (14, 15), testMaze), ("B2F", (36, 35), testMaze2)]
            , mazes          = [("B1F", (4, 5), testMaze), ("B2F", (26, 25), testMaze2)]
            , encountMap     = Map.fromList [
                  ((0, 0, 0), (10, [EnemyID 1, EnemyID 2]))
                , ((0, 1, 0), (10, [EnemyID 1, EnemyID 2]))
                , ((0, 2, 0), (10, [EnemyID 1, EnemyID 2]))
                , ((0, 3, 0), (10, [EnemyID 1, EnemyID 2]))
                , ((0, 4, 0), (10, [EnemyID 1, EnemyID 2]))
                , ((1, 2, 0), (10, [EnemyID 1, EnemyID 2]))
                ]
            , roomBattleMap  = Map.fromList [
                  ((1, 2, 0), (75, [EnemyID 1, EnemyID 2]))
                ]
            , roomDefine = []
            , mazeEvents = Map.fromList [
                  (GameEventID 010100, Ev.Events [
                     Ev.Select "there is climbing stairs.\n...climbing?\n\n(^Y/^N)" Nothing [
                       ("y", Ev.ReturnCastle), ("n", Ev.Escape)]
                   ])
                , (GameEventID 020400, Ev.Events [
                     Ev.Select "there is ladder to go down.\n...go down?\n\n(^Y/^N)" Nothing [
                       ("y", Ev.StairsToLower (1, 3, 1) <> Ev.End)
                     , ("n", Ev.Escape)
                     ]
                   ])
                , (GameEventID 020401, Ev.Events [
                     Ev.Select "there is ladder to go up.\n...go up?\n\n(^Y/^N)" Nothing [
                       ("y", Ev.StairsToUpper (1, 3, 0) <> Ev.End)
                     , ("n", Ev.Escape)
                     ]
                   ])
                , (GameEventID 010101, Ev.Events [
                     Ev.Ask "what's your name?" (Just $ PictureID 1001) [
                       ("werdna", Ev.Message "OH MY GOD!" (Just $ PictureID 1002))
                     , ("", Ev.Message "who?" (Just $ PictureID 1001))
                     ]
                   ])

                -- like NPC
                , (GameEventID 010102, 
                     Ev.Message "何者かが近づいてきた。" Nothing
                  <> Ev.MessageT (-15) "私はデバッグ用NPC\n\nHaskellを賛美せよ!!" (Just $ PictureID 1002)
                  <> Ev.Reference (GameEventID 010104)
                   )
                , (GameEventID 010104, 
                     Ev.Select "Party's Option\n  ^T)alk  ^L)eave" (Just $ PictureID 1002)
                     [("l", Ev.MessageT (-15) "さらばだ！！" (Just $ PictureID 1002))
                     ,("t", Ev.Reference (GameEventID 010103))
                     ]
                   )
                , (GameEventID 010103, Ev.Ask "何について話す？ (say \"bye\" to exit.)" (Just $ PictureID 1002)
                     [ ("hello\nhi", Ev.MessageT (-15) "私はデバッグ用NPC\n\nHaskellを賛美せよ!!" (Just $ PictureID 1002)
                                  <> Ev.Reference (GameEventID 010103))
                     , ("name" , Ev.MessageT (-15) "名前はまだない。" (Just $ PictureID 1002)
                              <> Ev.Reference (GameEventID 010103))
                     , ("haskell", Ev.MessageT (-15) "Haskellはこの世界を作っている言語だ。\nつまり神の言語だ!!" (Just $ PictureID 1002)
                                <> Ev.Reference (GameEventID 010103))
                     , ("god\n神", Ev.MessageT (-15) "まぁ私もよく分からず言っている。" (Just $ PictureID 1002)
                                <> Ev.Reference (GameEventID 010103))
                     , ("fight", Ev.MessageT (-15) "私は平和主義者だ。\n戦いは好まない。" (Just $ PictureID 1002)
                              <> Ev.Reference (GameEventID 010103))
                     , ("dance"  , Ev.MessageTimeT (-15) "\nそれなら知っている.\n" (Just $ PictureID 1002) (-500)
                                <> Ev.MessageTime        "\nそれなら知っている..\n" (Just $ PictureID 1002) (-500)
                                <> Ev.MessageTime        "\nそれなら知っている...\n" (Just $ PictureID 1002) (-500)
                                <> Ev.MessageTime        "\nWNWSEENE\n\nだ。" (Just $ PictureID 1002) 500
                                <> Ev.MessageT (-15) "これをある場所で踏むのだ。" (Just $ PictureID 1002)
                                <> Ev.Reference (GameEventID 010103))
                     , ("place" ,  Ev.MessageT (-15) "自分で探すのだ!" (Just $ PictureID 1002)
                                <> Ev.Reference (GameEventID 010103))

                     , ("goodbye\nbye", Ev.MessageT (-15) "またいつでも来ると良い!!" (Just $ PictureID 1002) <> Ev.Reference (GameEventID 010104))
                     , ("castle" , Ev.SelectT (-15) "なんだ、城に帰りたいのか？\n(^Y/^N)" (Just $ PictureID 1002)
                                   [("y",
                                        Ev.MessageTimeT (-15) "\nちょっと待っとれ."   (Just $ PictureID 1002) (500)
                                     <> Ev.MessageTime        "\nちょっと待っとれ.."  (Just $ PictureID 1002) (500)
                                     <> Ev.MessageTime        "\nちょっと待っとれ..." (Just $ PictureID 1002) (500)
                                     <> Ev.MessageTimeT (-10) "\nMAPILO MAHAMA DILOMAT!!" (Just $ PictureID 1002) 750
                                     <> Ev.MessageTime        "\nMAPILO MAHAMA DILOMAT!! だったかな?" (Just $ PictureID 1002) 300 <> Ev.ReturnCastle)
                                   ,("n",
                                       Ev.MessageT (-15) "そうなの?" (Just $ PictureID 1002) <> Ev.Reference (GameEventID 010103))
                                   ])
                     , ("", Ev.MessageT (-15) "それは知らない..." (Just $ PictureID 1002)
                         <> Ev.Reference (GameEventID 010103))
                     ]
                  )

                -- dance event on (1, 5, 0)
                --         WNWSE->NE
                --    step:123456 78
                -- reset flags
                , (GameEventID 01050100, Ev.Events [
                     Ev.ChangeEventFlag 1 (parse' "0")
                   , Ev.ChangeEventFlag 2 (parse' "0")
                   ])
                -- dance stap1, 3
                , (GameEventID 01050101, Ev.Switch [
                     ( Ev.FormulaCheckParty (parse' "(evf.1=2)*100")
                     , Ev.ChangeEventFlag 1 (parse' "3")
                     ),
                     ( Ev.Otherwise
                     , Ev.ChangeEventFlag 1 (parse' "1")
                     )
                  ])
                -- dance stap2
                , (GameEventID 01050102, Ev.Switch [
                     ( Ev.FormulaCheckParty (parse' "(evf.1=1)*100")
                     , Ev.ChangeEventFlag 1 (parse' "2")
                     ),
                     ( Ev.Otherwise, Ev.Reference (GameEventID 01050100))
                   ])
                -- dance stap4
                , (GameEventID 01050103, Ev.Switch [
                     ( Ev.FormulaCheckParty (parse' "(evf.1=3)*100")
                     , Ev.ChangeEventFlag 1 (parse' "4")
                     ),
                     ( Ev.Otherwise, Ev.Reference (GameEventID 01050100))
                   ])
                -- dance stap5
                , (GameEventID 01050104, Ev.Switch [
                     ( Ev.FormulaCheckParty (parse' "(evf.1=4)*100")
                     , Ev.ChangeEventFlag 1 (parse' "5")
                     ),
                     ( Ev.Otherwise, Ev.Reference (GameEventID 01050100))
                   ])
                -- dance stap6,8
                , (GameEventID 01050105, Ev.Switch [
                     ( Ev.FormulaCheckParty (parse' "(evf.1=5)*100")
                     , Ev.ChangeEventFlag 1 (parse' "6")
                     ),
                     ( Ev.FormulaCheckParty (parse' "(evf.1=7)*100")
                     , Ev.Events [
                         Ev.Message "Your dance is NICE!!!" (Just $ PictureID 1002)
                       , Ev.Reference (GameEventID 01050100)
                     ]
                     ),
                     ( Ev.Otherwise, Ev.Reference (GameEventID 01050100))
                   ])
                -- dance stap7
                , (GameEventID 01050106, Ev.Switch [
                     ( Ev.FormulaCheckParty (parse' "(evf.1=6)*100")
                     , Ev.ChangeEventFlag 1 (parse' "7")
                     ),
                     ( Ev.Otherwise, Ev.Reference (GameEventID 01050100))
                   ])

                ]
            , eventMap = Map.fromList [
                  ((0, 0, 0), GameEventID 010100)
                , ((1, 3, 0), GameEventID 020400)
                , ((1, 3, 1), GameEventID 020401)
                , ((0, 0, 1), GameEventID 010101)
                , ((3, 0, 1), GameEventID 010102)
                ]
            , eventMapDir = Map.fromList [
                  (Position W 0 4 0, GameEventID 01050101)
                , (Position N 0 4 0, GameEventID 01050102)
                , (Position S 0 4 0, GameEventID 01050103)
                , (Position E 0 4 0, GameEventID 01050104)
                , (Position E 1 4 0, GameEventID 01050105)
                , (Position N 1 4 0, GameEventID 01050106)
                ]
            , enemies = SampleEnemies.enemies
            , spells  = SampleSpells.spells
            , items   = SampleItems.items
            }

    let pic id | id == PictureID 1001 = himiko
               | id == PictureID 1002 = werdna
               | id == PictureID 2001 = treasure
               | id == PictureID 2002 = werdna
               | otherwise            = mempty
    let picOf = maybe mempty pic

    existSaveData <- doesFileExist saveDataPath
    
    run <- if not existSaveData then return runGame
           else do
             ls <- lines <$> readFile saveDataPath
             let is  = read <$> ls
                 is' = foldl (\acc i -> if i == Abort then tail acc else i:acc) [] is
             return $ loadGame (reverse is')
             
    drawCache <- newDrawCache
    let renderMethod = renderWithCache drawCache
        cmd = getKey (clearCache drawCache)
    --let renderMethod = render
    --    cmd = getKey (return ())

    clearScreen
    hideCursor
    putStrLn =<< run (testRender renderMethod picOf scenario) cmd scenario w inCastle
    showCursor

    appendFile saveDataPath $ show Abort ++ "\n"


saveDataPath = "save.txt"

-- ==========================================================================

getKey :: IO () -> InputType -> IO Input
getKey refresh itype = do
    i <- getKey' itype
    appendFile saveDataPath (show i ++ "\n")
    return i
  where
    getKey' SingleKey = do
        hSetBuffering stdin NoBuffering
        x <- getChar
        when (x == '\ESC') refresh
        return $ Key [x]
    getKey' SequenceKey = do
        hSetBuffering stdin LineBuffering
        showCursor
        (Key <$> getLine) <* (cursorUp 1 >> clearLine >> hideCursor >> refresh)
    getKey' (WaitClock n)
      | n > 0     = threadDelay (n * 1000) >> return Clock
      | otherwise = do
          x <- race (threadDelay $ n * (-1000)) waitKey
          return $ case x of Left  _ -> Clock
                             Right c -> Key [c]

waitKey :: IO Char
waitKey = do
    hSetBuffering stdin NoBuffering
    buf <- hReady stdin
    if buf then getChar
           else threadDelay 50000 >> waitKey

-- ==========================================================================

type RenderMethod = Bool -> Craphic -> IO ()

testRender :: RenderMethod -> (Maybe PictureID -> Craphic) -> Scenario -> Event -> World -> IO()
testRender renderMethod picOf s (Ask m picID)           w = testRender renderMethod picOf s (MessagePic m picID) w
testRender renderMethod picOf s (MessageTime t m picID) w = testRender renderMethod picOf s (MessagePic m picID) w
testRender renderMethod picOf s (FlashMessage t m)      w = rendering  renderMethod picOf s "" m "" Nothing Nothing w
testRender renderMethod picOf s (Message m)             w = testRender renderMethod picOf s (MessagePic m Nothing) w
testRender renderMethod picOf s (SpellCommand m)        w = testRender renderMethod picOf s (BattleCommand m) w
testRender renderMethod picOf s None                    w = testRender renderMethod picOf s (Time 0 Nothing) w

testRender renderMethod picOf s (MessagePic m picID)    w = rendering  renderMethod picOf s m "" ""  Nothing  picID w
testRender renderMethod picOf s (BattleCommand m)       w = rendering  renderMethod picOf s "" "" m  Nothing  Nothing w
testRender renderMethod picOf s (Time _ picID)          w = rendering  renderMethod picOf s "" "" "" Nothing  picID w
testRender renderMethod picOf s (ShowStatus cid m _)    w = rendering  renderMethod picOf s m "" ""  (Just cid) Nothing w

testRender renderMethod _ s (ShowMap m trans) w = setCursorPosition 0 0 >> renderMethod (debugMode w) (mapView m (place w) trans (visitHitory w) s)

testRender renderMethod _ _ Exit w = undefined

-- --------------------------------------------------------------------------

rendering :: RenderMethod
          -> (Maybe PictureID -> Craphic)
          -> Scenario
          -> String -- ^ message on MessageBox
          -> String -- ^ message on FlashMessageBox
          -> String -- ^ message on CommandBox
          -> Maybe CharacterID -- ^ inspection view target.
          -> Maybe PictureID
          -> World
          -> IO()
rendering renderMethod picOf s mMsg fMsg cMsg cid' picID w = do
    setCursorPosition 0 0
    renderMethod (debugMode w)
           $ t1 (if null locationText         then mempty else location locationText)
          <> t1 (if null mMsg' || isJust cid' then mempty else (msgTrans . msgBox') mMsg')
          <> t1 (if null fMsg                 then mempty else flashMsgBox fMsg)
          <> t1 (if null cMsg                 then mempty else cmdBox cMsg )
          <> t1 (if visibleStatusWindow w && not hideStatus then status s w (catMaybes ps) else mempty)
          <> t1 (if visibleGuideWindow w then guide else mempty)
          <>    (if null cMsg && null mMsg && isNothing picID then minimapScreen else mempty)
--        <> t1 location (show $ (take 5 . eventFlags) w) -- MEMO:forDebug
          <> t1 statusScene
          <> t1 (debugWindow $ debugMessage w) -- MEMO:forDebug
          <> t1 frame
          <> t1 (enemyScene picOf s (place w))
          <> t1 treasureScene
          <> t1 (picOf picID)
          <> t1 (sceneTrans w (scene (place w) (partyLight w > 0) (partyLight' w > 0) s))
  where
    t1    = translate (1, 1)
    ps    = flip Map.lookup (allCharacters w) <$> party w
    cs    = allCharacters w
    ess   = case place w of InBattle _ ess' -> ess'
                            _               -> []
    isInBattle   = case place w of InBattle _ _ -> True
                                   _            -> False
    isChestOpend = case place w of FindTreasureChest _ True  -> True
                                   _                         -> False
    isOnTreasure = case place w of FindTreasureChest {} -> True
                                   _                    -> False
    treasureScene = case place w of FindTreasureChest _ False -> treasureChest
                                    FindTreasureChest _ True  -> treasure
                                    _                         -> mempty
    statusScene   = case cid' of Nothing  -> mempty
                                 Just cid -> statusView s w mMsg itemDefOf (cs Map.! cid)
    msgBox' = case place w of Camping _ -> msgBoxCamp
                              _         -> msgBox
    mMsg' | not (null mMsg) = mMsg
          | not (null ess)  = unlines $ take 4 $ fmap txtEnemy (zip [1..] ess) ++ repeat "\n"
          | isOnTreasure    = "you found a treasure chest."
          | otherwise       = mMsg
    hideStatus = ((not . null) ess && null cMsg && isNothing cid')
              || (isOnTreasure && (not . null) cMsg || isChestOpend)
              || (isInBattle && null ess)
    txtEnemy (l, es) = let
         e          = head es
         edef       = enemies s Map.! Enemy.id e
         determined = Enemy.determined e
         ename      = if determined then Enemy.name edef else Enemy.nameUndetermined edef
         nAll       = show $ length es
         nActive    = show $ length . filter (null . Enemy.statusErrors) $ es
      in show l ++ ") " ++ nAll ++ " " ++ ename ++ replicate (43 - length ename) ' '  ++ " (" ++ nActive ++ ")"
    itemDefOf = (Map.!) (items s)
    locationText = if isJust cid' then "" else
                   case place w of InCastle            -> "Castle" 
                                   Gilgamesh'sTarvern  -> "ギルガメッシュの酒場" --"Gilgamesh's Tarvern"
                                   Adventure'sInn      -> "Adventure's Inn"
                                   Boltac'sTradingPost -> "Boltac's Trading Post"
                                   TempleOfCant        -> "Temple of Cant"
                                   InEdgeOfTown        -> "Edge of Town"
                                   TrainingGrounds     -> "Training Grounds"
                                   Camping _           -> "Camp"
                                   _ -> []
    msgTrans = if null locationText then id else translate (0, 1)

    minimapScreen = case minimapType (worldOption w) of
                      Disable -> mempty
                      Normal  -> miniMapView  (place w) (visitHitory w) (6, 6) True s
                      AlwaysN -> miniMapViewN (place w) (visitHitory w) (6, 6) True s
                            

enemyScene :: (Maybe PictureID -> Craphic) -> Scenario -> Place -> Craphic
enemyScene picOf s (InBattle _ (es:_)) =
    let e    = head es
        edef = enemies s Map.! Enemy.id e
    in if Enemy.determined e then picOf (Just $ Enemy.pic edef)
                             else changeSGR 'B' $ picOf (Just $ Enemy.picUndetermined edef)
enemyScene _ _ _ = mempty


visibleStatusWindow :: World -> Bool
visibleStatusWindow w = (statusOn w && inMaze) || showStatusAlways
  where
    inMaze = case place w of InMaze _ -> True
                             _        -> False
    showStatusAlways = case place w of InMaze _        -> False
                                       TrainingGrounds -> False
                                       _               -> True

visibleGuideWindow :: World -> Bool
visibleGuideWindow w = let inMaze = case place w of InMaze _ -> True
                                                    _        -> False
    in guideOn w && inMaze



-- ==========================================================================
