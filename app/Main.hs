module Main where

import System.IO.NoBufferingWorkaround
import System.Console.ANSI
import qualified Data.Map as Map
import System.Random
import GameAuto
import World
import InCastle
import qualified Characters as Chara

import CuiRender

-- https://kuo3.dev/2019/06/29/haskellでコンソールゲームを作りたい。共通編入力処/
-- http://hiratara.hatenadiary.jp/entry/2017/02/11/140028

main :: IO ()
main = do
    let status = Chara.Status {
          Chara.strength = 12 -- ^ 力
        , Chara.iq       = 10 -- ^ 知恵
        , Chara.piety    = 10 -- ^ 信仰心
        , Chara.vitality = 10 -- ^ 生命力
        , Chara.agility  = 10 -- ^ 素早さ
        , Chara.luck     = 10 -- ^ 運の強さ
        }
    let testChara1 = Chara.Character {
          Chara.name     = "test1"  -- ^ 名前
        , Chara.age      = 18   -- ^ 年齢
        , Chara.lv       = 1    -- ^ レベル
        , Chara.exp      = 4000 -- ^ 経験値
        , Chara.gold     = 1000 -- ^ 所持金

        , Chara.hp       = 12 -- ^ HP
        , Chara.maxhp    = 20 -- ^ MaxHP
        , Chara.status   = status  -- ^ ステータス
        , Chara.marks    = 0 -- ^ 倒した敵の数
        , Chara.rips     = 0 -- ^ 死亡数
        
        , Chara.items    = []         -- ^ 所持アイテム
        , Chara.spells   = []        -- ^ 習得済みの魔法
        , Chara.mp       = ([], []) -- ^ MP
        , Chara.maxmp    = ([], []) -- ^ MP
        }
    gen <- getStdGen
    let w = World {
        randomGen       = gen

      , party           = []
      , place           = InCastle

      , inTarvernMember = [testChara1]
      , inMazeMember    = []
      , shopItems       = Map.fromList []
      }
    initGetCharNoBuffering
    let cmd = getKey
    runGame testRender cmd (inCastle, w)


getKey :: IO Input
getKey = do
    x <- getCharNoBuffering
    return $ Key [x]


testRender :: Event -> World -> IO()
testRender (Message m) w = do
    clearScreen
    render $ msgBox m <> status' (party w)
testRender e w = print e

