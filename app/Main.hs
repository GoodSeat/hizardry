module Main where

import System.IO (getChar, hSetBuffering, stdin, BufferMode(..), hReady)
import System.Console.ANSI (clearScreen)
import qualified Data.Map as Map
import Data.Maybe (maybe, catMaybes, isJust, fromJust)
import System.Random
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

-- * scenario parser, save date parser.
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
            , Character.Spell
            , Character.Run
            , Character.Parry
            , Character.UseItem
            ]
        }
        priest = Character.Job {
          Character.jobName              = "Priest"
        , Character.enableAlignments     = [Character.G, Character.N]
        , Character.enableBattleCommands = [
              Character.Fight
            , Character.Spell
            , Character.Run
            , Character.Parry
            , Character.UseItem
            ]
        }
    let testChara1 = Character.Character {
          Character.name     = "FIG1"
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

        , Character.items    = [ItemInf (ItemID 1) True, ItemInf (ItemID 1) False]
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
        , Character.hp       = 34
        , Character.maxhp    = 48
        , Character.lv       = 5
        , Character.statusErrors = []

        , Character.job      = priest
        , Character.alignment= Character.N
        , Character.spells   = [SpellID 71, SpellID 111, SpellID 112]
        , Character.items    = [ItemInf (ItemID 2) True, ItemInf (ItemID 2) False]
        }
    gen <- getStdGen
    let w = World {
        randomGen       = gen
      , guideOn         = True
      , statusOn        = True

      , party           = []
      , place           = InCastle
      , roomBattled     = []
      , visitHitory     = Map.fromList []

      , inTarvernMember = [CharacterID 1, CharacterID 2, CharacterID 3]
      , inMazeMember    = []
      , shopItems       = Map.fromList []

      , allCharacters   = Map.fromList [
                              (CharacterID 1, testChara1)
                            , (CharacterID 2, testChara2)
                            , (CharacterID 3, testChara3)
                            ]
      , sceneTrans      = id
      }
    let cmd = getKey
        option = Option "Q)uit Game (for Debug!)"
        scenario = Scenario {
              scenarioOption = option
            , scenarioHome   = inCastle
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

                    , Enemy.dropItem          = [(15, parse' "1d15+1")]
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
                                                ,Enemy.Spelling (parse' "71")
                                                ,Enemy.Run
                                                ]

                    , Enemy.dropItem          = [(15, parse' "1d15+1")]
                    , Enemy.dropGold          = parse' "2d10"

                    , Enemy.withBackProb      = 15
                    , Enemy.backEnemyID       = parse' "2"

                    , Enemy.enableRun         = True
                    , Enemy.trapCandidate     = [Enemy.NoTrap, Enemy.PoisonNeedle, Enemy.GasBomb, Enemy.CrossbowBolt, Enemy.ExplodingBox]
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
                })
                ,
                (ItemID 1, Item.Define {
                      Item.name             = "DIOS POTION"
                    , Item.nameUndetermined = "POTION?"
                    , Item.itemType         = Item.Misc
                    , Item.usingEffect      = Just (Item.EqSpell $ SpellID 111, (100, Item.ChangeTo $ ItemInf (ItemID 0) False))
                    , Item.spEffect         = Nothing
                    , Item.attributes       = []
                })
                ,
                (ItemID 2, Item.Define {
                      Item.name             = "CURSED STONE"
                    , Item.nameUndetermined = "STONE?"
                    , Item.itemType         = Item.Misc
                    , Item.usingEffect      = Just (Item.EqSpell $ SpellID 111, (100, Item.ChangeTo $ ItemInf (ItemID 0) False))
                    , Item.spEffect         = Nothing
                    , Item.attributes       = [Item.CantDrop, Item.Heal (-2) False]
                })
                ]
            }

    let pic id | id == PictureID 1001 = himiko
               | id == PictureID 1002 = werdna
               | id == PictureID 2001 = himiko
               | id == PictureID 2002 = werdna
               | otherwise            = mempty
    let picOf = maybe mempty pic

    putStrLn =<< runGame (testRender picOf scenario) cmd scenario w inCastle


-- ==========================================================================

getKey :: InputType -> IO Input
getKey SingleKey = do
    hSetBuffering stdin NoBuffering
    x <- getChar
    return $ Key [x]
getKey SequenceKey = do
    hSetBuffering stdin LineBuffering
    Key <$> getLine
getKey (WaitClock n)
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
testRender picOf s (Ask m picID)           w = testRender picOf s (MessagePic m picID)   w
testRender picOf s (MessageTime _ m picID) w = testRender picOf s (MessagePic m picID)   w
testRender picOf s (Message m)             w = testRender picOf s (MessagePic m Nothing) w
testRender picOf s (SpellCommand m)        w = testRender picOf s (BattleCommand m)      w
testRender picOf s None                    w = testRender picOf s (Time 0 Nothing)       w

testRender picOf s (MessagePic m picID)    w = rendering  picOf s m  "" Nothing  picID   w
testRender picOf s (BattleCommand m)       w = rendering  picOf s "" m  Nothing  Nothing w
testRender picOf s (Time _ picID)          w = rendering  picOf s "" "" Nothing  picID   w
testRender picOf s (ShowStatus i m _)      w = rendering  picOf s m  "" (Just i) Nothing w

testRender _ _ Exit _ = undefined

-- --------------------------------------------------------------------------

rendering :: (Maybe PictureID -> Craphic)
          -> Scenario
          -> String -- ^ message on MessageBox
          -> String -- ^ message on CommandBox
          -> Maybe PartyPos -- ^ inspection view target.
          -> Maybe PictureID
          -> World
          -> IO()
rendering picOf s mMsg cMsg i' picID w = do
    clearScreen
    render $ (if null mMsg' || isJust i' then mempty else msgBox mMsg')
          <> (if null cMsg then mempty else cmdBox cMsg )
          <> (if statusWindow w && not hideStatus then status (catMaybes ps) else mempty)
          <> (if guideWindow w then guide else mempty)
          <> sv
          <> enemyScene picOf s (place w)
          <> picOf picID
          <> frame
          <> sceneTrans w (scene (place w) s)
  where
    ps    = flip Map.lookup (allCharacters w) <$> party w
    ess   = case place w of InBattle _ ess' -> ess'
                            _               -> []
    mMsg' = if (not . null) mMsg || null ess then mMsg
            else unlines $ take 4 $ fmap txtEnemy (zip [1..] ess) ++ repeat "\n"
    sv    = case i' of Nothing -> mempty
                       Just  i -> statusView mMsg itemNameOf (ps !! (partyPosToNum i - 1))
    hideStatus = (not . null) ess && null cMsg
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

enemyScene :: (Maybe PictureID -> Craphic) -> Scenario -> Place -> Craphic
enemyScene picOf s (InBattle _ (es:_)) =
    let e    = head es
        edef = enemies s Map.! Enemy.id e
    in if Enemy.determined e then picOf (Just $ Enemy.pic edef)
                             else picOf (Just $ Enemy.picUndetermined edef)
enemyScene _ _ _ = mempty


statusWindow :: World -> Bool
statusWindow w = let inMaze = case place w of InMaze _ -> True
                                              _        -> False
    in (statusOn w && not inBattle) || not inMaze || inBattle
  where
    inBattle = case place w of InBattle _ _ -> True
                               _            -> False

guideWindow :: World -> Bool
guideWindow w = let inMaze = case place w of InMaze _ -> True
                                             _        -> False
    in guideOn w && inMaze

-- ==========================================================================
