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


-- note
-- * game over
-- * items
-- *   sp
-- * shop
-- *   identify items
-- * temple
-- * lvup
-- * training ground
-- * room battle
-- *   treasure chest
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
        , Character.exp      = 4000
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
                               ,ItemInf (ItemID 11) True
                               ,ItemInf (ItemID 12) False
                               ,ItemInf (ItemID 13) False
                               ,ItemInf (ItemID 103) True
                               ]
        , Character.equips   = []

        , Character.spells   = [SpellID 11, SpellID 21]
        , Character.mp       = (replicate 7 7, replicate 7 7)
        , Character.maxmp    = (replicate 7 7, replicate 7 7)
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
        , Character.lv       = 5
        , Character.statusErrors = []
        , Character.paramDelta = []

        , Character.job      = priest
        , Character.alignment= Character.N
        , Character.spells   = [SpellID 11, SpellID 12, SpellID 71, SpellID 111, SpellID 112, SpellID 113, SpellID 114, SpellID 121]
        , Character.items    = [ItemInf (ItemID 2) True, ItemInf (ItemID 2) False]
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
                     , ("god\n神", Ev.MessageT (-15) "まぁよく分からず言っている。" (Just $ PictureID 1002)
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
            , enemies        = Map.fromList [
                (EnemyID 1, Enemy.Define {
                      Enemy.name              = "slime"
                    , Enemy.nameUndetermined  = "moving object"
                    , Enemy.pic               = PictureID 1001
                    , Enemy.picUndetermined   = PictureID 2001
                    , Enemy.lv                = 1
                    , Enemy.hpFormula         = parse' "1d6"

                    , Enemy.param             = Parameter 5 8 8 8 8 8
                    , Enemy.ac                = 10

                    , Enemy.exp               = 55
                    , Enemy.friendlyProb      = 0
                    , Enemy.numOfOccurrences  = parse' "2d2"
                    , Enemy.healPerTurn       = 2
                    , Enemy.moveFrontProb     = 20

                    , Enemy.resistError       = [(Dead, read "6")]
                    , Enemy.vsEffectLabels    = [(EffectLabel "fire", read "value*5")]
                    , Enemy.attrLabels        = [EnemyLabel "animal"]

                    , Enemy.actions           = [Enemy.Fight 1 (parse' "1d1") (parse' "1d3")
                                                 [(read "lv*20-o.lv", Silence,  [EffectLabel "mucus"])
                                                 ,(read "lv*20-o.lv", Poison 2, [EffectLabel "mucus"])
                                                 ]
                                                ,Enemy.Breath (read "hp/2") [EffectLabel "fire"]
                                                ]

                    , Enemy.dropItem          = [(50, parse' "1d3")]
                    , Enemy.dropGold          = parse' "2d10"

                    , Enemy.withBackProb      = 50
                    , Enemy.backEnemyID       = parse' "1"

                    , Enemy.enableRun         = True
                    , Enemy.trapCandidate     = [Enemy.NoTrap, Enemy.PoisonNeedle, Enemy.GasBomb, Enemy.CrossbowBolt, Enemy.ExplodingBox]
                })
                , (EnemyID 2, Enemy.Define {
                      Enemy.name              = "goblin"
                    , Enemy.nameUndetermined  = "humanoid creature"
                    , Enemy.pic               = PictureID 1002
                    , Enemy.picUndetermined   = PictureID 2002
                    , Enemy.lv                = 2
                    , Enemy.hpFormula         = parse' "2d3+1"

                    , Enemy.param             = Parameter 5 8 8 8 8 8
                    , Enemy.ac                = 8

                    , Enemy.exp               = 415
                    , Enemy.friendlyProb      = 15
                    , Enemy.numOfOccurrences  = parse' "2d3"
                    , Enemy.healPerTurn       = 0
                    , Enemy.moveFrontProb     = 20

                    , Enemy.resistError       = [(Dead, read "12")]
                    , Enemy.vsEffectLabels    = []
                    , Enemy.attrLabels        = [EnemyLabel "beast"]

                    , Enemy.actions           = [Enemy.Fight 2 (parse' "1d2+1") (parse' "1d3")
                                                 [(read "lv*20-o.lv", Dead,  [EffectLabel "cutlery"])]
                                                ,Enemy.Spelling (parse' "11")
                                                --,Enemy.Spelling (parse' "21")
                                                --,Enemy.Spelling (parse' "71")
                                                ,Enemy.Run
                                                ]

                    , Enemy.dropItem          = [(50, parse' "1d3")]
                    , Enemy.dropGold          = parse' "2d10"

                    , Enemy.withBackProb      = 15
                    , Enemy.backEnemyID       = parse' "2"

                    , Enemy.enableRun         = True
                    , Enemy.trapCandidate     = [Enemy.Alarm, Enemy.Teleporter, Enemy.Stunner]
                })
                ]
            , spells         = Map.fromList [
                (SpellID 11, Spell.Define {
                      Spell.name       = "halito"
                    , Spell.kind       = Spell.M
                    , Spell.lv         = 1
                    , Spell.attrLabels = [EffectLabel "mage", EffectLabel "fire"]
                    , Spell.target     = Spell.OpponentSingle
                    , Spell.effect     = Spell.Damage (parse' "1d6")
                    , Spell.enableIn   = [Spell.InBattle]
                })
                ,
                (SpellID 12, Spell.Define {
                      Spell.name       = "dumapic"
                    , Spell.kind       = Spell.M
                    , Spell.lv         = 1
                    , Spell.attrLabels = [EffectLabel "mage"]
                    , Spell.target     = Spell.AllyAll
                    --, Spell.effect     = Spell.CheckLocation Spell.OnlyCoord
                    , Spell.effect     = Spell.CheckLocation Spell.ViewMap
                    , Spell.enableIn   = [Spell.InCamp]
                })
                ,
                (SpellID 21, Spell.Define {
                      Spell.name       = "mahalito"
                    , Spell.kind       = Spell.M
                    , Spell.lv         = 2
                    , Spell.attrLabels = [EffectLabel "mage", EffectLabel "fire"]
                    , Spell.target     = Spell.OpponentGroup
                    , Spell.effect     = Spell.Damage (parse' "2d6")
                    , Spell.enableIn   = [Spell.InBattle]
                })
                ,
                (SpellID 71, Spell.Define {
                      Spell.name       = "tiltowait"
                    , Spell.kind       = Spell.M
                    , Spell.lv         = 7
                    , Spell.attrLabels = [EffectLabel "mage"]
                    , Spell.target     = Spell.OpponentAll
                    , Spell.effect     = Spell.Damage (parse' "10d10")
                    , Spell.enableIn   = [Spell.InBattle]
                })

                ,
                (SpellID 111, Spell.Define {
                      Spell.name       = "dios"
                    , Spell.kind       = Spell.P
                    , Spell.lv         = 1
                    , Spell.attrLabels = [EffectLabel "priest"]
                    , Spell.target     = Spell.AllySingle
                    , Spell.effect     = Spell.Cure (parse' "1d8") []
                    , Spell.enableIn   = [Spell.InCamp, Spell.InBattle]
                })
                ,
                (SpellID 112, Spell.Define {
                      Spell.name       = "milwa"
                    , Spell.kind       = Spell.P
                    , Spell.lv         = 1
                    , Spell.attrLabels = [EffectLabel "priest"]
                    , Spell.target     = Spell.AllyAll
                    , Spell.effect     = Spell.AddLight 30 False
                    , Spell.enableIn   = [Spell.InCamp, Spell.InBattle]
                })
                ,
                (SpellID 113, Spell.Define {
                      Spell.name       = "diosa"
                    , Spell.kind       = Spell.P
                    , Spell.lv         = 1
                    , Spell.attrLabels = [EffectLabel "priest"]
                    , Spell.target     = Spell.AllyAll
                    , Spell.effect     = Spell.Cure (parse' "1d8") []
                    , Spell.enableIn   = [Spell.InCamp, Spell.InBattle]
                })
                ,
                (SpellID 114, Spell.Define {
                      Spell.name       = "maporfic"
                    , Spell.kind       = Spell.P
                    , Spell.lv         = 1
                    , Spell.attrLabels = [EffectLabel "priest"]
                    , Spell.target     = Spell.Party
                    , Spell.effect     = Spell.ChangeParam (AdParam {
                          adStrength = read "0" -- ^ strength
                        , adIq       = read "0" -- ^ I.Q.
                        , adPiety    = read "0" -- ^ piety
                        , adVitality = read "0" -- ^ vitality
                        , adAgility  = read "0" -- ^ agility
                        , adLuck     = read "0" -- ^ luck
                        , adAC       = read "-2" -- ^ AC
                        , adName     = "Protection" -- ^ effect name. if this name isn't empty, can't apply multiple.
                        }) OnlyInMaze "is protected."
                    , Spell.enableIn   = [Spell.InCamp, Spell.InBattle]
                })
                ,
                (SpellID 121, Spell.Define {
                      Spell.name       = "smilwa"
                    , Spell.kind       = Spell.P
                    , Spell.lv         = 2
                    , Spell.attrLabels = [EffectLabel "priest"]
                    , Spell.target     = Spell.AllyAll
                    , Spell.effect     = Spell.AddLight 30 True
                    , Spell.enableIn   = [Spell.InCamp, Spell.InBattle]
                })
                ]
            , items = Map.fromList [
                (ItemID 0, Item.Define {
                      Item.name             = "BROKEN ITEM"
                    , Item.nameUndetermined = "BROKEN ITEM?"
                    , Item.itemType         = Item.Misc
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Nothing
                    , Item.valueInShop      = 0
                    , Item.enableToEquip    = Item.All
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 1, Item.Define {
                      Item.name             = "DIOS POTION"
                    , Item.nameUndetermined = "POTION?"
                    , Item.itemType         = Item.Misc
                    , Item.usingEffect      = Just (Item.EqSpell $ SpellID 111, (100, Item.ChangeTo $ ItemInf (ItemID 0) False))
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Nothing
                    , Item.valueInShop      = 100
                    , Item.enableToEquip    = Item.All
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 2, Item.Define {
                      Item.name             = "CURSED STONE"
                    , Item.nameUndetermined = "STONE?"
                    , Item.itemType         = Item.Misc
                    , Item.usingEffect      = Just (Item.EqSpell $ SpellID 111, (100, Item.ChangeTo $ ItemInf (ItemID 0) False))
                    , Item.spEffect         = Nothing
                    , Item.attributes       = [Item.CantDrop, Item.Heal (-2) False]
                    , Item.equipType        = Nothing
                    , Item.valueInShop      = 200
                    , Item.enableToEquip    = Item.All
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 3, Item.Define {
                      Item.name             = "WATER"
                    , Item.nameUndetermined = "POTION?"
                    , Item.itemType         = Item.Misc
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Nothing
                    , Item.valueInShop      = 500
                    , Item.enableToEquip    = Item.All
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 11, Item.Define {
                      Item.name             = "SWORD OF IRON"
                    , Item.nameUndetermined = "SWORD?"
                    , Item.itemType         = Item.Equip
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Just $ Item.Weapon
                                                     Item.EquipBaseAttr {
                                                         Item.ac = read "0"
                                                       , Item.st = read "4"
                                                       , Item.at = read "0"
                                                       , Item.resistLabels   = []
                                                       , Item.resistError    = []
                                                       , Item.vsEffectLabels = []
                                                     } 
                                                     Item.WeaponAttr {
                                                         Item.targetF       = [L1, L2]
                                                       , Item.targetB       = []
                                                       , Item.damage        = read "1d8"
                                                       , Item.doubleLabels  = []
                                                       , Item.attrLabels    = [EffectLabel "fire"]
                                                       , Item.addStatusErrors = []
                                                       , Item.atackMessages = []
                                                     }
                    , Item.valueInShop      = 500
                    , Item.enableToEquip    = Item.Only ["Fighter", "Lord"]
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 12, Item.Define {
                      Item.name             = "SHILD OF IRON"
                    , Item.nameUndetermined = "SHILD?"
                    , Item.itemType         = Item.Equip
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Just $ Item.Shield
                                                     Item.EquipBaseAttr {
                                                         Item.ac = read "-2"
                                                       , Item.st = read "0"
                                                       , Item.at = read "0"
                                                       , Item.resistLabels   = []
                                                       , Item.resistError    = [(Sleep, read "min(30,lv)")]
                                                       , Item.vsEffectLabels = []
                                                     } 
                    , Item.valueInShop      = 500
                    , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 13, Item.Define {
                      Item.name             = "HELMET OF IRON"
                    , Item.nameUndetermined = "HELMET?"
                    , Item.itemType         = Item.Equip
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Just $ Item.Shield
                                                     Item.EquipBaseAttr {
                                                         Item.ac = read "-2"
                                                       , Item.st = read "0"
                                                       , Item.at = read "0"
                                                       , Item.resistLabels   = []
                                                       , Item.resistError    = []
                                                       , Item.vsEffectLabels = []
                                                     } 
                    , Item.valueInShop      = 300
                    , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 14, Item.Define {
                      Item.name             = "HELMET OF IRON"
                    , Item.nameUndetermined = "HELMET?"
                    , Item.itemType         = Item.Equip
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Just $ Item.Shield
                                                     Item.EquipBaseAttr {
                                                         Item.ac = read "-2"
                                                       , Item.st = read "0"
                                                       , Item.at = read "0"
                                                       , Item.resistLabels   = []
                                                       , Item.resistError    = []
                                                       , Item.vsEffectLabels = []
                                                     } 
                    , Item.valueInShop      = 300
                    , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 15, Item.Define {
                      Item.name             = "HELMET OF IRON"
                    , Item.nameUndetermined = "HELMET?"
                    , Item.itemType         = Item.Equip
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Just $ Item.Shield
                                                     Item.EquipBaseAttr {
                                                         Item.ac = read "-2"
                                                       , Item.st = read "0"
                                                       , Item.at = read "0"
                                                       , Item.resistLabels   = []
                                                       , Item.resistError    = []
                                                       , Item.vsEffectLabels = []
                                                     } 
                    , Item.valueInShop      = 300
                    , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 16, Item.Define {
                      Item.name             = "HELMET OF IRON"
                    , Item.nameUndetermined = "HELMET?"
                    , Item.itemType         = Item.Equip
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Just $ Item.Shield
                                                     Item.EquipBaseAttr {
                                                         Item.ac = read "-2"
                                                       , Item.st = read "0"
                                                       , Item.at = read "0"
                                                       , Item.resistLabels   = []
                                                       , Item.resistError    = []
                                                       , Item.vsEffectLabels = []
                                                     } 
                    , Item.valueInShop      = 300
                    , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 17, Item.Define {
                      Item.name             = "HELMET OF IRON"
                    , Item.nameUndetermined = "HELMET?"
                    , Item.itemType         = Item.Equip
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Just $ Item.Shield
                                                     Item.EquipBaseAttr {
                                                         Item.ac = read "-2"
                                                       , Item.st = read "0"
                                                       , Item.at = read "0"
                                                       , Item.resistLabels   = []
                                                       , Item.resistError    = []
                                                       , Item.vsEffectLabels = []
                                                     } 
                    , Item.valueInShop      = 300
                    , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 103, Item.Define {
                      Item.name             = "盗賊の弓"
                    , Item.nameUndetermined = "弓?"
                    , Item.itemType         = Item.Equip
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Just $ Item.Weapon
                                                     Item.EquipBaseAttr {
                                                         Item.ac = read "0"
                                                       , Item.st = read "0"
                                                       , Item.at = read "0"
                                                       , Item.resistLabels   = []
                                                       , Item.resistError    = []
                                                       , Item.vsEffectLabels = []
                                                     } 
                                                     Item.WeaponAttr {
                                                         Item.targetF       = [L1, L2, L3, L4]
                                                       , Item.targetB       = [L1, L2]
                                                       , Item.damage        = read "3d8"
                                                       , Item.doubleLabels  = []
                                                       , Item.attrLabels    = []
                                                       , Item.addStatusErrors = []
                                                       , Item.atackMessages = ["aimed and shot"]
                                                     }
                    , Item.valueInShop      = 4000
                    , Item.enableToEquip    = Item.Only ["Thief"]
                    , Item.enableToUse      = Item.All
                })
                ,
                (ItemID 104, Item.Define {
                      Item.name             = "火矢の弓"
                    , Item.nameUndetermined = "弓?"
                    , Item.itemType         = Item.Equip
                    , Item.usingEffect      = Nothing
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                    , Item.equipType        = Just $ Item.Weapon
                                                     Item.EquipBaseAttr {
                                                         Item.ac = read "0"
                                                       , Item.st = read "0"
                                                       , Item.at = read "0"
                                                       , Item.resistLabels   = []
                                                       , Item.resistError    = []
                                                       , Item.vsEffectLabels = []
                                                     } 
                                                     Item.WeaponAttr {
                                                         Item.targetF       = [L1, L2, L3, L4]
                                                       , Item.targetB       = [L1, L2]
                                                       , Item.damage        = read "3d8"
                                                       , Item.doubleLabels  = [EnemyLabel "beast"]
                                                       , Item.attrLabels    = [EffectLabel "fire"]
                                                       , Item.addStatusErrors = [(read "lv-o.lv", Dead, [EffectLabel "fire"])]
                                                       , Item.atackMessages = ["aimed and shot"]
                                                     }
                    , Item.valueInShop      = 4000
                    , Item.enableToEquip    = Item.Only ["Thief"]
                    , Item.enableToUse      = Item.All
                })
                ]
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
