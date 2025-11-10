module Main where

import System.IO (getChar, hSetBuffering, stdin, BufferMode(..), hReady)
import System.Console.ANSI (clearScreen, clearLine, hideCursor, showCursor, setCursorPosition, cursorUp)
import System.Directory
import System.Random
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
import qualified Data.Enemies as Enemy

import Control.CUI
import UI.CuiRender

import qualified SampleScenario.Home as SampleScenario


-- note
-- * game over
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
-- * config
-- *   all cure when sleep / when return castle(-> delete INN)

-- * PictureID -> PictureInf, enable to overlap, translate, etc...
-- * Event -> more universal. enable define for message of each window, PictureInf, clock Time, etc...

-- * scenario parser, save data parser.
-- *   hashable-1.4.1.0 [Data.Hashable] hash:: a -> Int
-- *   zip compression with secret keyword. using another exe? deflate?

encKey :: String
encKey = ""

main :: IO ()
main = do
    --gen <- getStdGen
    let gen = mkStdGen 0 
    (is, iw) <- SampleScenario.initScenario
    let s = initScenario is inCastle 
    let w = initWorld iw gen True

    let picOf = maybe mempty SampleScenario.pic

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
    w' <- run (testRender renderMethod picOf s) cmd s w inCastle
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
      | n > 0     = race (threadDelay $ n * 1000) ignoreKey >> return Clock
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

ignoreKey :: IO ()
ignoreKey = do
    hSetBuffering stdin NoBuffering
    buf <- hReady stdin
    when buf $ void getChar
    threadDelay 50000 >> ignoreKey

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
          <> t1 (frameTrans w $ frame)
          <> t1 (enemyTrans w $ enemyScene picOf s (place w))
          <> t1 treasureScene
          <> t1 (picOf picID)
          <> t1 (sceneTrans w $ scene (place w) (partyLight w > 0) (partyLight' w > 0) s)
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
