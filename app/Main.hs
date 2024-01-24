module Main where

import System.IO (getChar, hSetBuffering, stdin, BufferMode(..), hReady)
import System.Console.ANSI (clearScreen, clearLine, hideCursor, showCursor, setCursorPosition, cursorUp)
import System.Random
import System.Directory
import qualified Data.Map as Map
import Data.Maybe (maybe, catMaybes, isJust, isNothing, fromJust)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Monad (void)

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
-- *   equip
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

        , Character.items    = [ItemInf (ItemID 1) True
                               ,ItemInf (ItemID 1) False
                               ,ItemInf (ItemID 11) True
                               ,ItemInf (ItemID 12) False
                               ,ItemInf (ItemID 13) False
                               ]
        , Character.equips   = []

        , Character.spells   = [SpellID 11, SpellID 21]
        , Character.mp       = (replicate 7 2, replicate 7 2)
        , Character.maxmp    = (replicate 7 2, replicate 7 2)
        }
        testChara2 = testChara1 {
          Character.name     = "FIG2"
        , Character.hp       = 126
        , Character.maxhp    = 148
        , Character.lv       = 15
        , Character.statusErrors = [Poison 5]
        }
        testChara3 = testChara1 {
          Character.name     = "PRI1"
        , Character.kind     = elf
        , Character.hp       = 34
        , Character.maxhp    = 48
        , Character.lv       = 5
        , Character.statusErrors = []

        , Character.job      = priest
        , Character.alignment= Character.N
        , Character.spells   = [SpellID 71, SpellID 111, SpellID 112, SpellID 113]
        , Character.items    = [ItemInf (ItemID 2) True, ItemInf (ItemID 2) False]
        }
        testChara4 = testChara1 {
          Character.name     = "THI1"
        , Character.hp       = 104
        , Character.maxhp    = 108
        , Character.lv       = 5
        , Character.statusErrors = []

        , Character.job      = thief
        , Character.alignment= Character.N
        , Character.spells   = []
        , Character.items    = [ItemInf (ItemID 2) True, ItemInf (ItemID 2) False]
        }
    --gen <- getStdGen
    let gen = mkStdGen 0 
    let w = World {
        randomGen       = gen
      , guideOn         = True
      , statusOn        = True

      , party           = []
      , place           = InCastle
      , roomBattled     = []
      , partyLight      = 0

      , visitHitory     = Map.fromList []

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
      }
    let cmd = getKey
        option = Option "Q)uit Game (for Debug!)"
        scenario = Scenario {
              scenarioOption = option
            , scenarioHome   = inCastle
            , kinds          = [human, elf]
            , jobs           = [fighter, priest, thief]
            , mazes          = [testMaze, testMaze2]
            , encountMap     = Map.fromList [
                  ((1, 1, 0), (10, [EnemyID 1, EnemyID 2]))
                , ((1, 2, 0), (10, [EnemyID 1, EnemyID 2]))
                , ((1, 3, 0), (10, [EnemyID 1, EnemyID 2]))
                , ((1, 4, 0), (10, [EnemyID 1, EnemyID 2]))
                , ((1, 5, 0), (10, [EnemyID 1, EnemyID 2]))
                , ((2, 3, 0), (10, [EnemyID 1, EnemyID 2]))
                ]
            , roomBattleMap  = Map.fromList [
                  ((2, 3, 0), (75, [EnemyID 1, EnemyID 2]))
                ]
            , roomDefine = []
            , mazeEvents = Map.fromList [
                  (GameEventID 010100, Ev.Events [
                     Ev.Select "there is climbing stairs.\n...climbing?\n\n(Y/N)" Nothing [
                       ("y", Ev.ReturnCastle), ("n", Ev.Escape)]
                   ])
                , (GameEventID 020400, Ev.Events [
                     Ev.Select "there is ladder to go down.\n...go down?\n\n(Y/N)" Nothing [
                       ("y", Ev.Events [
                           Ev.StairsToLower (2, 4, 1)
                         , Ev.End
                         ])
                     , ("n", Ev.Escape)
                     ]
                   ])
                , (GameEventID 020401, Ev.Events [
                     Ev.Select "there is ladder to go up.\n...go up?\n\n(Y/N)" Nothing [
                       ("y", Ev.Events [
                           Ev.StairsToUpper (2, 4, 0)
                         , Ev.End
                         ])
                     , ("n", Ev.Escape)
                     ]
                   ])
                , (GameEventID 010101, Ev.Events [
                     Ev.Ask "what's your name?" (Just $ PictureID 1001) [
                       ("werdna", Ev.Events [
                           Ev.Message "OH MY GOD!" (Just $ PictureID 1002)
                         ])
                     , ("", Ev.Message "who?" (Just $ PictureID 1001))
                     ]
                   ])
                ]
            , eventMap = Map.fromList [
                  ((1, 1, 0), GameEventID 010100)
                , ((2, 4, 0), GameEventID 020400)
                , ((2, 4, 1), GameEventID 020401)
                , ((1, 1, 1), GameEventID 010101)
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
                    , Enemy.kind              = "animal"
                    , Enemy.friendlyProb      = 0
                    , Enemy.numOfOccurrences  = parse' "2d2"
                    , Enemy.resistProbM       = 0
                    , Enemy.resistProbP       = 0
                    , Enemy.healPerTurn       = 2
                    , Enemy.moveFrontProb     = 20

                    , Enemy.resistError       = [(Dead, 6)]
                    , Enemy.resistAttributes  = []
                    , Enemy.weakAttributes    = []

                    , Enemy.actions           = [Enemy.Fight 1 (parse' "1d1") (parse' "1d3") []]

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
                    , Enemy.kind              = "animal"
                    , Enemy.friendlyProb      = 15
                    , Enemy.numOfOccurrences  = parse' "2d3"
                    , Enemy.resistProbM       = 0
                    , Enemy.resistProbP       = 0
                    , Enemy.healPerTurn       = 0
                    , Enemy.moveFrontProb     = 20

                    , Enemy.resistError       = [(Dead, 12)]
                    , Enemy.resistAttributes  = []
                    , Enemy.weakAttributes    = []

                    , Enemy.actions           = [Enemy.Fight 2 (parse' "1d2+1") (parse' "1d3") []
                                                ,Enemy.Spelling (parse' "11")
                                                ,Enemy.Spelling (parse' "21")
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
                      Spell.name      = "halito"
                    , Spell.kind      = Spell.M
                    , Spell.lv        = 1
                    , Spell.attribute = Spell.Fire
                    , Spell.target    = Spell.OpponentSingle
                    , Spell.effect    = Spell.Damage (parse' "1d6")
                    , Spell.enableIn  = [Spell.InBattle]
                })
                ,
                (SpellID 21, Spell.Define {
                      Spell.name      = "mahalito"
                    , Spell.kind      = Spell.M
                    , Spell.lv        = 2
                    , Spell.attribute = Spell.Fire
                    , Spell.target    = Spell.OpponentGroup
                    , Spell.effect    = Spell.Damage (parse' "2d6")
                    , Spell.enableIn  = [Spell.InBattle]
                })
                ,
                (SpellID 71, Spell.Define {
                      Spell.name      = "tiltowait"
                    , Spell.kind      = Spell.M
                    , Spell.lv        = 7
                    , Spell.attribute = Spell.None
                    , Spell.target    = Spell.OpponentAll
                    , Spell.effect    = Spell.Damage (parse' "10d10")
                    , Spell.enableIn  = [Spell.InBattle]
                })

                ,
                (SpellID 111, Spell.Define {
                      Spell.name      = "dios"
                    , Spell.kind      = Spell.P
                    , Spell.lv        = 1
                    , Spell.attribute = Spell.None
                    , Spell.target    = Spell.AllySingle
                    , Spell.effect    = Spell.Cure (parse' "1d8") []
                    , Spell.enableIn  = [Spell.InCamp, Spell.InBattle]
                })
                ,
                (SpellID 112, Spell.Define {
                      Spell.name      = "milwa"
                    , Spell.kind      = Spell.P
                    , Spell.lv        = 1
                    , Spell.attribute = Spell.None
                    , Spell.target    = Spell.AllyAll
                    , Spell.effect    = Spell.AddLight 30
                    , Spell.enableIn  = [Spell.InCamp, Spell.InBattle]
                })
                ,
                (SpellID 113, Spell.Define {
                      Spell.name      = "diosa"
                    , Spell.kind      = Spell.P
                    , Spell.lv        = 1
                    , Spell.attribute = Spell.None
                    , Spell.target    = Spell.AllyAll
                    , Spell.effect    = Spell.Cure (parse' "1d8") []
                    , Spell.enableIn  = [Spell.InCamp, Spell.InBattle]
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
                    , Item.equipType        = Just Item.Weapon
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
                    , Item.equipType        = Just $ Item.Shield (-2)
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
                    , Item.equipType        = Just $ Item.Shield (-2)
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
                    , Item.equipType        = Just $ Item.Shield (-2)
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
                    , Item.equipType        = Just $ Item.Shield (-2)
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
                    , Item.equipType        = Just $ Item.Shield (-2)
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
                    , Item.equipType        = Just $ Item.Shield (-2)
                    , Item.valueInShop      = 300
                    , Item.enableToEquip    = Item.Only ["Fighter", "Lord", "Priest"]
                    , Item.enableToUse      = Item.All
                })
                ]
            }

    let pic id | id == PictureID 1001 = himiko
               | id == PictureID 1002 = werdna
               | id == PictureID 2001 = himiko
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
             
    clearScreen
    hideCursor
    putStrLn =<< run (testRender picOf scenario) cmd scenario w inCastle
    showCursor

    appendFile saveDataPath $ show Abort ++ "\n"


saveDataPath = "save.txt"

-- ==========================================================================

getKey :: InputType -> IO Input
getKey itype = do
    i <- getKey' itype
    appendFile saveDataPath (show i ++ "\n")
    return i
  where
    getKey' SingleKey = do
        hSetBuffering stdin NoBuffering
        x <- getChar
        return $ Key [x]
    getKey' SequenceKey = do
        hSetBuffering stdin LineBuffering
        showCursor
        (Key <$> getLine) <* (cursorUp 1 >> clearLine >> hideCursor)
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

testRender :: (Maybe PictureID -> Craphic) -> Scenario -> Event -> World -> IO()
testRender picOf s (Ask m picID)           = testRender picOf s (MessagePic m picID)  
testRender picOf s (MessageTime _ m picID) = testRender picOf s (MessagePic m picID)  
testRender picOf s (Message m)             = testRender picOf s (MessagePic m Nothing)
testRender picOf s (SpellCommand m)        = testRender picOf s (BattleCommand m)     
testRender picOf s None                    = testRender picOf s (Time 0 Nothing)      

testRender picOf s (MessagePic m picID)    = rendering  picOf s m  "" Nothing  picID  
testRender picOf s (BattleCommand m)       = rendering  picOf s "" m  Nothing  Nothing
testRender picOf s (Time _ picID)          = rendering  picOf s "" "" Nothing  picID  
testRender picOf s (ShowStatus cid m _)    = rendering  picOf s m  "" (Just cid) Nothing

testRender _ _ Exit = undefined

-- --------------------------------------------------------------------------

rendering :: (Maybe PictureID -> Craphic)
          -> Scenario
          -> String -- ^ message on MessageBox
          -> String -- ^ message on CommandBox
          -> Maybe CharacterID -- ^ inspection view target.
          -> Maybe PictureID
          -> World
          -> IO()
rendering picOf s mMsg cMsg cid' picID w = do
    setCursorPosition 0 0
    render $ (if null locationText then mempty else location locationText)
          <> (if null mMsg' || isJust cid' then mempty else (msgTrans . msgBox) mMsg')
          <> (if null cMsg then mempty else cmdBox cMsg )
          <> (if statusWindow w && not hideStatus then status (catMaybes ps) else mempty)
          <> (if guideWindow w then guide else mempty)
          <> sv
          <> frame
          <> enemyScene picOf s (place w)
          <> treas
          <> picOf picID
          <> sceneTrans w (scene (place w) onLight s)
  where
    ps    = flip Map.lookup (allCharacters w) <$> party w
    cs    = allCharacters w
    ess   = case place w of InBattle _ ess' -> ess'
                            _               -> []
    inBat = case place w of InBattle _ _ -> True
                            _            -> False
    treas = case place w of FindTreasureChest _ False -> treasureChest
                            FindTreasureChest _ True  -> treasure
                            _                         -> mempty
    treasOpend = case place w of FindTreasureChest _ True  -> True
                                 _                         -> False
    onTreasure = case place w of FindTreasureChest {} -> True
                                 _                    -> False
    mMsg' | not (null mMsg) = mMsg
          | not (null ess)  = unlines $ take 4 $ fmap txtEnemy (zip [1..] ess) ++ repeat "\n"
          | onTreasure      = "you found a treasure chest."
          | otherwise       = mMsg
    sv    = case cid' of Nothing  -> mempty
                         Just cid -> statusView mMsg itemNameOf (cs Map.! cid)
    hideStatus = ((not . null) ess && null cMsg && isNothing cid')
              || (onTreasure && (not . null) cMsg || treasOpend)
              || (inBat && null ess)
    txtEnemy (l, es) = let
         e          = head es
         edef       = enemies s Map.! Enemy.id e
         determined = Enemy.determined e
         ename      = if determined then Enemy.name edef else Enemy.nameUndetermined edef
         nAll       = show $ length es
         nActive    = show $ length . filter (null . Enemy.statusErrors) $ es
      in show l ++ ") " ++ nAll ++ " " ++ ename ++ replicate (43 - length ename) ' '  ++ " (" ++ nActive ++ ")"
    itemNameOf id identified = let def = items s Map.! id in
        (if identified then Item.name else Item.nameUndetermined) def
    onLight = partyLight w > 0
    locationText = if isJust cid' then "" else
                   case place w of InCastle            -> "Castle" 
                                   Gilgamesh'sTarvern  -> "Gilgamesh's Tarvern"
                                   Adventure'sInn      -> "Adventure's Inn"
                                   Boltac'sTradingPost -> "Boltac's Trading Post"
                                   TempleOfCant        -> "Temple of Cant"
                                   InEdgeOfTown        -> "Edge of Town"
                                   TrainingGrounds     -> "Training Grounds"
                                   _ -> []
    msgTrans = if null locationText then id else translate (0, 1)

enemyScene :: (Maybe PictureID -> Craphic) -> Scenario -> Place -> Craphic
enemyScene picOf s (InBattle _ (es:_)) =
    let e    = head es
        edef = enemies s Map.! Enemy.id e
    in if Enemy.determined e then picOf (Just $ Enemy.pic edef)
                             else changeSGR 'B' $ picOf (Just $ Enemy.picUndetermined edef)
enemyScene _ _ _ = mempty


statusWindow :: World -> Bool
statusWindow w = (statusOn w && inMaze) || showStatusAlways
  where
    inMaze = case place w of InMaze _ -> True
                             _        -> False
    showStatusAlways = case place w of InMaze _        -> False
                                       TrainingGrounds -> False
                                       _               -> True

guideWindow :: World -> Bool
guideWindow w = let inMaze = case place w of InMaze _ -> True
                                             _        -> False
    in guideOn w && inMaze

-- ==========================================================================
