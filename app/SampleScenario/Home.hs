module SampleScenario.Home where

import Engine.GameAuto
import Engine.InCastle
import Engine.Utils
import Data.Primitive
import Data.World
import Data.Maze
import Data.Formula
import Control.CUI

import qualified Data.Map as Map

import qualified Data.Characters as Character
import qualified Data.Spells     as Spell

import qualified SampleScenario.Spells  as SampleSpells
import qualified SampleScenario.Items   as SampleItems
import qualified SampleScenario.Enemies as SampleEnemies
import qualified SampleScenario.Events  as SampleEvents
import qualified SampleScenario.Jobs    as SampleJobs
import qualified SampleScenario.Racies  as SampleRacies
import qualified SampleScenario.Maze    as SampleMaze
import qualified SampleScenario.PicturesOfPrimitives          as SamplePicturesOfPrimitives
import qualified SampleScenario.PicturesOfEnemies             as SamplePicturesOfEnemies
import qualified SampleScenario.PicturesOfEnemiesUnidentified as SamplePicturesOfEnemiesUnidentified


initScenario :: IO (InitScenario, InitWorld)
initScenario = return (s, w)
  where
    w = InitWorld {
          initGuideOn         = True
        , initStatusOn        = True
        , initWorldOption     = WorldOption {
              effectDumapic = Spell.ViewMap
            , minimapType   = Normal
            }
        , initParty           = []
        , initPlace           = InCastle
        , initRoomBattled     = []
        , initPartyLight      = 0
        , initPartyLight'     = 0
        , initPartyParamDelta = []

        , initInTarvernMember = [
              CharacterID 1, CharacterID 2, CharacterID 3, CharacterID 4
            ]
        , initInMazeMember    = []
        , initShopItems       = Map.fromList [
              (ItemID 1, 10), (ItemID 2, 2), (ItemID 3, 2)
            , (ItemID 11, 3), (ItemID 12, 3), (ItemID 13, 3)
            , (ItemID 14, 3), (ItemID 15, 3), (ItemID 16, 3)
            , (ItemID 17, 3)
            ]
        , initAllCharacters   = Map.fromList [
              (CharacterID 1, testChara1)
            , (CharacterID 2, testChara2)
            , (CharacterID 3, testChara3)
            , (CharacterID 4, testChara4)
            ]
        }
    option = ScenarioOption {
          enableEffectDumapic = [Spell.OnlyCoord, Spell.ViewMap]
        , enableMinimapType   = [Disable, Normal, AlwaysN]
        }
    s = InitScenario {
          initScenarioOption = option
        , initRacies         = SampleRacies.racies
        , initJobs           = SampleJobs.jobs
        , initMazes          = [
              ("B1F", ( 4,  5), SampleMaze.maze1F)
            , ("B2F", (26, 25), SampleMaze.maze2F)
            ]
        , initEncountMap     = Map.fromList [
              ((0, 0, 0), (10, [EnemyID 1, EnemyID 2]))
            , ((0, 1, 0), (10, [EnemyID 1, EnemyID 2]))
            , ((0, 2, 0), (10, [EnemyID 1, EnemyID 2]))
            , ((0, 3, 0), (10, [EnemyID 1, EnemyID 2]))
            , ((0, 4, 0), (10, [EnemyID 1, EnemyID 2]))
            , ((1, 2, 0), (10, [EnemyID 1, EnemyID 2]))
            ]
        , initRoomBattleMap  = Map.fromList [
              ((1, 2, 0), (75, [EnemyID 1, EnemyID 2]))
            ]
        , initRoomDefine     = []
        , initMazeEvents     = SampleEvents.mazeEvents
        , initEventMap       = SampleEvents.eventMap
        , initEventMapDir    = SampleEvents.eventMapDir
        , initEventInspect   = Map.fromList [ (Position E 2 1 0, GameEventID 02010101) ]
        , initEnemies        = SampleEnemies.enemies
        , initSpells         = SampleSpells.spells
        , initItems          = SampleItems.items
        }
    param = Parameter {
          strength = 12
        , iq       = 10
        , piety    = 10
        , vitality = 10
        , agility  = 10
        , luck     = 10
        }
    testChara1 = Character.Character {
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
        , Character.spells   = [SpellID 11, SpellID 13, SpellID 14, SpellID 15, SpellID 16, 
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

modScenario :: Scenario -> Scenario
modScenario s = let org = mazes s in s {
    mazes = \z -> do
        flg2 <- evFlag 2
        if z == 0 && flg2 == 1
          then return ("B1F", ( 4,  5), SampleMaze.maze1F')
          else org z
    }


pic :: PictureInf -> Craphic
pic Null = mempty
pic (Single (PictureID id))
    |    0 <= id && id < 1000 = SamplePicturesOfPrimitives.picOfPrimitive id
    | 1001 <= id && id < 2000 = SamplePicturesOfEnemies.picOfEnemies id
    | 2001 <= id && id < 3000 = SamplePicturesOfEnemiesUnidentified.picOfEnemiesUnidentified id
    | otherwise               = mempty
pic (Trans dx dy pi) = translate (dx, dy) $ pic pi
pic (Xor  p1 p2)     = xor  (pic p1) (pic p2)
pic (Clip p1 p2)     = clip (pic p1) (pic p2)
pic (Diff p1 p2)     = diff (pic p1) (pic p2)
pic (List [])        = mempty
pic (List (pi:pis))  = pic pi <> pic (List pis)

