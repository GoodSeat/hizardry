module Main where

import System.IO.NoBufferingWorkaround
import System.Console.ANSI
import qualified Data.Map as Map
import Data.Maybe
import System.Random

import GameAuto
import World
import InCastle
import qualified Characters as Character
import qualified Enemies as Enemy
import Maze
import Formula

import CuiRender

-- https://kuo3.dev/2019/06/29/haskellでコンソールゲームを作りたい。共通編入力処/
-- http://hiratara.hatenadiary.jp/entry/2017/02/11/140028

main :: IO ()
main = do
    let param = Character.Parameter {
          Character.strength = 12
        , Character.iq       = 10
        , Character.piety    = 10
        , Character.vitality = 10
        , Character.agility  = 10
        , Character.luck     = 10
        }
    let testChara1 = Character.Character {
          Character.name     = "FIG1"
        , Character.age      = 18
        , Character.days     = 0

        , Character.lv       = 1
        , Character.exp      = 4000
        , Character.gold     = 1000

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
        , Character.hp       = 16
        , Character.maxhp    = 18
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
      }
    let cmd = getKey
        option = Option "Q)uit Game (for Debug!)"
        scenario = Scenario {
              scenarioOption = option
            , scenarioHome   = inCastle
            , mazes          = [testMaze]
            , encountMap     = Map.fromList [
                  ((1, 1, 0), (3, [Enemy.ID 1]))
                , ((1, 2, 0), (3, [Enemy.ID 1]))
                , ((1, 3, 0), (3, [Enemy.ID 1]))
                , ((1, 4, 0), (3, [Enemy.ID 1]))
                , ((1, 5, 0), (3, [Enemy.ID 1]))
                ]
            , enemies        = Map.fromList [
                (Enemy.ID 1, Enemy.Define {
                      Enemy.name              = "slime"
                    , Enemy.nameUndetermined  = "moving object"
                    , Enemy.lv                = 1
                    , Enemy.maxhp             = parse' ""

                    , Enemy.param             = Character.Parameter 5 8 8 8 8 8
                    , Enemy.ac                = 10

                    , Enemy.exp               = 55
                    , Enemy.kind              = "animal"
                    , Enemy.frendlyProb       = 0
                    , Enemy.numOfOccurrences  = parse' "2d2"
                    , Enemy.resistProbM       = 0
                    , Enemy.resistProbP       = 0
                    , Enemy.healPerTurn       = 2
                    , Enemy.moveFrontProb     = 20

                    , Enemy.resistError       = [(Character.Dead, 6)]
                    , Enemy.resistAttributes  = []
                    , Enemy.weakAttributes    = []

                    , Enemy.actions           = [Enemy.Fight 1 (parse' "1d1") (parse' "1d3") []]

                    , Enemy.dropItem          = [(15, parse' "1d15+1")]
                    , Enemy.dropGold          = parse' "2d10"

                    , Enemy.withBackProb      = 0
                    , Enemy.backEnemyID       = parse' "1"

                    , Enemy.enableRun         = True
                    , Enemy.trapCandidate     = [Enemy.NoTrap, Enemy.PoisonNeedle, Enemy.GasBomb, Enemy.CrossbowBolt, Enemy.ExplodingBox]
                })
                ]
            }
    initGetCharNoBuffering
    putStrLn =<< runGame (testRender scenario) cmd scenario (inCastle, w)


getKey :: IO Input
getKey = do
    x <- getCharNoBuffering
    return $ Key [x]


testRender :: Scenario -> Event -> World -> IO()
testRender s (Message m) w = do
    clearScreen
    let ps = flip Map.lookup (allCharacters w) <$> party w
    render $ msgBox m
          <> (if statusWindow w then status (concat $ maybeToList <$> ps) else mempty)
          <> (if guideWindow w then guide else mempty)
          <> frame
          <> scene (place w) s
testRender s None w = do
    clearScreen
    let ps = flip Map.lookup (allCharacters w) <$> party w
    render $ (if statusWindow w then status (concat $ maybeToList <$> ps) else mempty)
          <> (if guideWindow w then guide else mempty)
          <> frame
          <> scene (place w) s

statusWindow :: World -> Bool
statusWindow w = let inMaze = case place w of InMaze _ -> True
                                              _        -> False
    in statusOn w || not inMaze

guideWindow :: World -> Bool
guideWindow w = let inMaze = case place w of InMaze _ -> True
                                             _        -> False
    in guideOn w && inMaze
