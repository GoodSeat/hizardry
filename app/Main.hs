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
import Maze

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
        , Character.lv       = 1
        , Character.exp      = 4000
        , Character.gold     = 1000

        , Character.hp       = 12
        , Character.maxhp    = 20
        , Character.param    = param
        , Character.marks    = 0
        , Character.rips     = 0
        
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

      , party           = []
      , place           = InCastle

      , inTarvernMember = [Character.ID 1, Character.ID 2]
      , inMazeMember    = []
      , shopItems       = Map.fromList []

      , allCharacters   = Map.fromList [
                              (Character.ID 1, testChara1)
                            , (Character.ID 2, testChara2)
                            ]
      }
    initGetCharNoBuffering
    let cmd = getKey
        option = Option "Q)uit Game (for Debug!)"
        scenario = Scenario {
              scenarioOption = option
            , scenarioHome   = inCastle
            , mazes          = [testMaze]
            }
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
          <> status (concat $ maybeToList <$> ps)
          <> scene (place w) s
testRender s e w = print e

