module Main where

import System.IO (getChar, hSetBuffering, stdin, BufferMode(..), hReady)
import System.Console.ANSI (clearScreen)
import qualified Data.Map as Map
import Data.Maybe
import System.Random
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (foldM)

import Primitive
import GameAuto
import World
import InCastle
import qualified Characters as Character
import qualified Enemies as Enemy
import qualified Spells as Spell
import Maze
import Formula

import CuiRender

-- https://kuo3.dev/2019/06/29/haskellでコンソールゲームを作りたい。共通編入力処/
-- http://hiratara.hatenadiary.jp/entry/2017/02/11/140028

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

        , Character.items    = []
        , Character.equips   = []

        , Character.spells   = []
        , Character.mp       = ([], [])
        , Character.maxmp    = ([], [])
        }
        testChara2 = testChara1 {
          Character.name     = "FIG2"
        , Character.hp       = 126
        , Character.maxhp    = 148
        , Character.lv       = 15
        , Character.statusErrors = [Poison 5]
        }
    gen <- getStdGen
    let w = World {
        randomGen       = gen
      , guideOn         = True
      , statusOn        = True

      , party           = []
      , place           = InCastle
      , roomBattled   = []

      , inTarvernMember = [Character.ID 1, Character.ID 2]
      , inMazeMember    = []
      , shopItems       = Map.fromList []

      , allCharacters   = Map.fromList [
                              (Character.ID 1, testChara1)
                            , (Character.ID 2, testChara2)
                            ]
      , sceneTrans      = id
      }
    let cmd = getKey
        option = Option "Q)uit Game (for Debug!)"
        scenario = Scenario {
              scenarioOption = option
            , scenarioHome   = inCastle
            , mazes          = [testMaze]
            , encountMap     = Map.fromList [
                  ((1, 1, 0), (30, [Enemy.ID 1]))
                , ((1, 2, 0), (30, [Enemy.ID 1]))
                , ((1, 3, 0), (30, [Enemy.ID 1]))
                , ((1, 4, 0), (30, [Enemy.ID 1]))
                , ((1, 5, 0), (30, [Enemy.ID 1]))
                ]
            , enemies        = Map.fromList [
                (Enemy.ID 1, Enemy.Define {
                      Enemy.name              = "slime"
                    , Enemy.nameUndetermined  = "moving object"
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
                ]
            , spells         = Map.fromList [
                (Spell.ID 1, Spell.Define {
                      Spell.name      = "halito"
                    , Spell.kind      = Spell.M
                    , Spell.lv        = 1
                    , Spell.attribute = Spell.Fire
                    , Spell.target    = Spell.OpponentSingle
                })
                ]
            }
    putStrLn =<< runGame (testRender scenario) cmd scenario (inCastle, w)


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
      waitTime <- async (threadDelay $ n * (-1000))
      waitKey  <- async $ do
        hSetBuffering stdin NoBuffering
        foldM (\alreadyCanceled _ -> do
          buf <- hReady stdin
          if buf && not alreadyCanceled then do
            getChar
            cancel waitTime
            return True
          else do
            threadDelay 50000 -- 50ms
            return alreadyCanceled
          ) False (repeat False)
      waitCatch waitTime
      return Clock


-- ==========================================================================
testRender :: Scenario -> Event -> World -> IO()
testRender s (MessageTime _ m) w = testRender s (Message m) w
testRender s (Message m) w = do
    clearScreen
    let ps = flip Map.lookup (allCharacters w) <$> party w
    render $ msgBox m
          <> (if statusWindow w then status (concat $ maybeToList <$> ps) else mempty)
          <> (if guideWindow w then guide else mempty)
          <> enemyPic (place w)
          <> frame
          <> sceneTrans w (scene (place w) s)
testRender s (SpellCommand m) w = testRender s (BattleCommand m) w
testRender s (BattleCommand m) w = do
    clearScreen
    let ps = flip Map.lookup (allCharacters w) <$> party w
        ess = case place w of InBattle _ ess' -> ess'
                              _               -> undefined
    render $ cmdBox m
          <> msgBox (unlines $ take 4 $ fmap txtEnemy (zip [1..] ess) ++ repeat "\n")
          <> (if statusWindow w then status (concat $ maybeToList <$> ps) else mempty)
          <> (if guideWindow w then guide else mempty)
          <> enemyPic (place w)
          <> frame
          <> sceneTrans w (scene (place w) s)
  where
    txtEnemy (l, es) = let
         e    = head es
         edef = enemies s Map.! Enemy.id e
         determined = Enemy.determined e
         ename = if determined then Enemy.name edef else Enemy.nameUndetermined edef
         nAll    = show $ length es
         nActive = show $ length . filter (null . Enemy.statusErrors) $ es
      in show l ++ ") " ++ nAll ++ " " ++ ename ++ replicate (43 - length ename) ' '  ++ " (" ++ nActive ++ ")"

testRender s (Time _) w = testRender s None w
testRender s None w = do
    clearScreen
    let ps = flip Map.lookup (allCharacters w) <$> party w
    render $ (if statusWindow w && not inBattle then status (concat $ maybeToList <$> ps) else mempty)
          <> (if guideWindow w then guide else mempty)
          <> enemyPic (place w)
          <> frame
          <> sceneTrans w (scene (place w) s)
  where inBattle = case place w of InBattle _ _ -> True
                                   _            -> False

testRender _ Exit _ = undefined


statusWindow :: World -> Bool
statusWindow w = let inMaze = case place w of InMaze _ -> True
                                              _        -> False
    in statusOn w || not inMaze

guideWindow :: World -> Bool
guideWindow w = let inMaze = case place w of InMaze _ -> True
                                             _        -> False
    in guideOn w && inMaze

-- ==========================================================================
