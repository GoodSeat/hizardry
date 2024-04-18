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
import Data.Formula
import qualified Data.Characters as Character
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell

import Control.CUI
import UI.CuiRender

import qualified SampleScenario.Spells as SampleSpells
import qualified SampleScenario.Items as SampleItems
import qualified SampleScenario.Enemies as SampleEnemies
import qualified SampleScenario.Events as SampleEvents
import qualified SampleScenario.Jobs as SampleJobs
import qualified SampleScenario.Racies as SampleRacies
import qualified SampleScenario.Maze as SampleMaze


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

    let testChara1 = Character.Character {
              Character.name     = "FIG1"
            , Character.race     = SampleRacies.human
            , Character.age      = 18
            , Character.days     = 0

            , Character.lv       = 1
            , Character.exp      = 40000000
            , Character.gold     = 1000

            , Character.job      = SampleJobs.fighter
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
            , Character.race     = SampleRacies.elf
            , Character.hp       = 34
            , Character.maxhp    = 48
            , Character.lv       = 1
            , Character.statusErrors = []
            , Character.paramDelta = []

            , Character.job      = SampleJobs.priest
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

            , Character.job      = SampleJobs.thief
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

    let option = ScenarioOption {
          enableEffectDumapic = [Spell.OnlyCoord, Spell.ViewMap]
        , enableMinimapType   = [Disable, Normal, AlwaysN]
        }

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
    let scenario = Scenario {
              scenarioOption = option
            , scenarioHome   = inCastle
            , racies         = SampleRacies.racies
            , jobs           = SampleJobs.jobs
            --, mazes          = [("B1F", (14, 15), testMaze), ("B2F", (36, 35), testMaze2)]
            , mazes          = [("B1F", (4, 5), SampleMaze.maze1F), ("B2F", (26, 25), SampleMaze.maze2F)]
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
            , mazeEvents  = SampleEvents.mazeEvents
            , eventMap    = SampleEvents.eventMap
            , eventMapDir = SampleEvents.eventMapDir
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
    w' <- run (testRender renderMethod picOf scenario) cmd scenario w inCastle
    showCursor

    appendFile saveDataPath $ show Abort ++ "\n"
    void $ saveWorld w' "world.dat"


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
    msgBox' = case place w of Camping _ _ -> msgBoxCamp
                              _           -> msgBox
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
                                   Camping _ t         -> if null t then "Camp" else t
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
