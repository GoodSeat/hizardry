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
import Labyrinth

import CuiRender

-- https://kuo3.dev/2019/06/29/haskellでコンソールゲームを作りたい。共通編入力処/
-- http://hiratara.hatenadiary.jp/entry/2017/02/11/140028

main :: IO ()
main = do
    let param = Character.Parameter {
          Character.strength = 12 -- ^ 力
        , Character.iq       = 10 -- ^ 知恵
        , Character.piety    = 10 -- ^ 信仰心
        , Character.vitality = 10 -- ^ 生命力
        , Character.agility  = 10 -- ^ 素早さ
        , Character.luck     = 10 -- ^ 運の強さ
        }
    let testChara1 = Character.Character {
          Character.name     = "test1"  -- ^ 名前
        , Character.age      = 18       -- ^ 年齢
        , Character.lv       = 1        -- ^ レベル
        , Character.exp      = 4000     -- ^ 経験値
        , Character.gold     = 1000     -- ^ 所持金

        , Character.hp       = 12      -- ^ HP
        , Character.maxhp    = 20      -- ^ MaxHP
        , Character.param    = param   -- ^ ステータス
        , Character.marks    = 0       -- ^ 倒した敵の数
        , Character.rips     = 0       -- ^ 死亡数
        
        , Character.items    = []       -- ^ 所持アイテム
        , Character.equips   = []       -- ^ 所持アイテム

        , Character.spells   = []       -- ^ 習得済みの魔法
        , Character.mp       = ([], []) -- ^ MP
        , Character.maxmp    = ([], []) -- ^ MP
        }
    gen <- getStdGen
    let w = World {
        randomGen       = gen

      , party           = []
      , place           = InCastle

      , inTarvernMember = [Character.ID 1]
      , inMazeMember    = []
      , shopItems       = Map.fromList []

      , allCharacters   = Map.fromList [
                            (Character.ID 1, testChara1)
                            ]
      }
    initGetCharNoBuffering
    let cmd = getKey
        option = Option "Q)uit Game (for Debug!)"
        scenario = Scenario {
              scenarioOption  = option
            , scenarioHome    = inCastle
            , labyrinths      = [testLabyrinth]
            }
    runGame testRender cmd scenario (inCastle, w)


getKey :: IO Input
getKey = do
    x <- getCharNoBuffering
    return $ Key [x]


testRender :: Event -> World -> IO()
testRender (Message m) w = do
    clearScreen
    let ps = flip Map.lookup (allCharacters w) <$> party w
    render $ msgBox m <> status' (concat $ maybeToList <$> ps)
testRender e w = print e

