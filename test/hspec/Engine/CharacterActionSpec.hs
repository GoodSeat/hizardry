
{-# LANGUAGE OverloadedStrings #-}
module Engine.CharacterActionSpec (spec) where

import Test.Hspec
import Control.Monad.Reader (runReader)
import Control.Monad.State (runStateT)
import Control.Monad.Except (runExceptT)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import System.Random (mkStdGen)

import Engine.GameAuto (GameState, Scenario(..), ScenarioOption(..), InitScenario(..), initScenario, GameMachine)
import Data.World
import Data.Primitive
import Data.Formula (Formula, parse')
import qualified Data.Characters as Chara
import qualified Data.Items as Item
import qualified Data.Spells as Spell
import Engine.CharacterAction (castResurrectionSpell)

-- Helper function to run GameState in tests
runGame :: GameState a -> Scenario -> World -> (Either String a, World)
runGame state scenario initialWorld = runReader (runStateT (runExceptT state) initialWorld) scenario

-- Test Data
testRace :: Chara.Race
testRace = Chara.Race {
    Chara.raceName = "Human"
  , Chara.initialParam = Parameter { strength = 8, iq = 8, piety = 8, vitality = 8, agility = 8, luck = 8 }
  , Chara.maxParam = Parameter { strength = 18, iq = 18, piety = 18, vitality = 18, agility = 18, luck = 18 }
  , Chara.initialBonus = parse' "6"
}

testJob :: Chara.Job
testJob = Chara.Job {
      Chara.jobName              = "Fighter"
    , Chara.enableAlignments     = [Chara.G, Chara.N, Chara.E]
    , Chara.enableBattleCommands = [Chara.Fight, Chara.Parry, Chara.UseItem, Chara.Run]
    , Chara.inspectTrapAbility   = parse' "0"
    , Chara.disarmTrapAbility    = parse' "0"
    , Chara.needParameter        = Parameter { strength = 11, iq = 0, piety = 0, vitality = 0, agility = 0, luck = 0 }
    , Chara.baseWeaponAttr       = Item.WeaponAttr { Item.targetF = [], Item.targetB = [], Item.damage = parse' "2d2", Item.doubleLabels = [], Item.attrLabels = [], Item.addStatusErrors = [], Item.atackMessages = [] }
    , Chara.fightTryCount        = parse' "1"
    , Chara.fightHitBonus        = parse' "2"
    , Chara.baseAC               = parse' "10"
    , Chara.identifyItemChance   = Nothing
    , Chara.lvupExps             = [1000]
    , Chara.hpFormula            = parse' "1d8"
    , Chara.mpFormula            = ([], [])
    , Chara.learningSpells       = []
}

mockGameMachine :: GameMachine
mockGameMachine = error "mockGameMachine should not be evaluated"

testScenario :: Scenario
testScenario = initScenario (InitScenario {
      initScenarioOption = ScenarioOption [] []
    , initRacies         = [testRace]
    , initJobs           = [testJob]
    , initMazes          = []
    , initEncountMap     = Map.empty
    , initRoomBattleMap  = Map.empty
    , initRoomDefine     = []
    , initEventMap       = Map.empty
    , initEventMapDir    = Map.empty
    , initEventInspect   = Map.empty
    , initMazeEvents     = Map.empty
    , initEnemies        = Map.empty
    , initSpells         = Map.empty
    , initItems          = Map.empty
    }) mockGameMachine

initialCaster :: Chara.Character
initialCaster = Chara.Character {
      Chara.name = "Caster"
    , Chara.race = testRace, Chara.job = testJob, Chara.age = 20, Chara.days = 0
    , Chara.alignment = Chara.G
    , Chara.param = Parameter { strength = 10, iq = 10, piety = 10, vitality = 10, agility = 10, luck = 10 }
    , Chara.lv = 1, Chara.exp = 0, Chara.gold = 0, Chara.hp = 10, Chara.maxhp = 10
    , Chara.statusErrors = [], Chara.mp = ([],[]), Chara.maxmp = ([],[]), Chara.paramDelta = []
    , Chara.items = [], Chara.equips = [], Chara.spells = []
    , Chara.marks = 0, Chara.rips = 0
    }

deadTarget :: Chara.Character
deadTarget = initialCaster
    { Chara.name = "Target"
    , Chara.hp = 0
    , Chara.statusErrors = [Dead]
    }

initialWorld :: World
initialWorld = World {
      randomGen = mkStdGen 42
    , guideOn = False, statusOn = False
    , worldOption = WorldOption Spell.OnlyCoord Normal
    , allCharacters = Map.fromList [(CharacterID 1, initialCaster), (CharacterID 2, deadTarget)] -- Modified
    , party = [CharacterID 1, CharacterID 2] -- Modified
    , place = InCastle
    , roomBattled = []
    , partyLight = 0, partyLight' = 0
    , partyParamDelta = []
    , visitHitory = Map.empty
    , inTarvernMember = []
    , inMazeMember = []
    , shopItems = Map.empty
    , sceneTrans = id, enemyTrans = id, frameTrans = id
    , eventFlags = []
    , debugMode = False, debugMessage = []
    }

spec :: Spec
spec = describe "castResurrectionSpell" $ do
    let caster = Left initialCaster
        targetPartyPos = [toPartyPos 2] -- "target" is the 2nd in party

    context "with 100% success rate" $ do
        it "resurrects a dead character and restores HP" $ do
            let hpFormula = parse' "10"
                resurrectionFormulas = [(Dead, parse' "100")]
                action = castResurrectionSpell hpFormula resurrectionFormulas caster (Left targetPartyPos)

            (Right results, _) <- return $ runGame action testScenario initialWorld
            let (updateAction, msg, _) = head results
            
            (_, finalWorldAfterUpdate) <- return $ runGame updateAction testScenario initialWorld

            let finalTarget = fromJust $ Map.lookup (CharacterID 2) (allCharacters finalWorldAfterUpdate) -- Modified

            msg `shouldBe` "Target has been resurrected."
            Chara.hp finalTarget `shouldBe` 10
            Chara.statusErrors finalTarget `shouldBe` []

    context "with 0% success rate" $ do
        it "fails to resurrect a dead character" $ do
            let hpFormula = parse' "10"
                resurrectionFormulas = [(Dead, parse' "0")]
                action = castResurrectionSpell hpFormula resurrectionFormulas caster (Left targetPartyPos)

            (Right results, _) <- return $ runGame action testScenario initialWorld
            let (updateAction, msg, _) = head results
            
            (_, finalWorldAfterUpdate) <- return $ runGame updateAction testScenario initialWorld

            let finalTarget = fromJust $ Map.lookup (CharacterID 2) (allCharacters finalWorldAfterUpdate) -- Modified

            msg `shouldBe` "Target could not be resurrected."
            Chara.hp finalTarget `shouldBe` 0
            Chara.statusErrors finalTarget `shouldBe` [Dead]

