module SampleScenario.Enemies where

import PreludeL
import qualified Data.Enemies as Enemy
import qualified Data.Map as Map

import Data.Primitive
import Data.Formula

enemies :: Enemy.DB
enemies = Map.fromList [
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
        , Enemy.friendlyProb      = 0
        , Enemy.numOfOccurrences  = parse' "2d2"
        , Enemy.healPerTurn       = 2
        , Enemy.moveFrontProb     = 40

        , Enemy.resistError       = [(Dead, read "6")]
        , Enemy.vsEffectLabels    = [(EffectLabel "fire", read "value*5")]
        , Enemy.attrLabels        = [EnemyLabel "animal"]

        , Enemy.actions           = [Enemy.Fight 1 (parse' "1d1") (parse' "1d3")
                                     [(read "lv*20-o.lv", Silence,  [EffectLabel "mucus"])
                                     ,(read "lv*20-o.lv", Poison 2, [EffectLabel "mucus"])
                                     ]
                                    ,Enemy.Breath (read "hp/2") [EffectLabel "fire"]
                                    ]

        , Enemy.dropItem          = [(50, parse' "1d3")]
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
        , Enemy.friendlyProb      = 15
        , Enemy.numOfOccurrences  = parse' "2d3"
        , Enemy.healPerTurn       = 0
        , Enemy.moveFrontProb     = 20

        , Enemy.resistError       = [(Dead, read "12")]
        , Enemy.vsEffectLabels    = []
        , Enemy.attrLabels        = [EnemyLabel "beast"]

        , Enemy.actions           = [Enemy.Fight 2 (parse' "1d2+1") (parse' "1d3")
                                     [(read "lv*20-o.lv", Dead,  [EffectLabel "cutlery"])]
                                    ,Enemy.Spelling (parse' "11")
                                    --,Enemy.Spelling (parse' "21")
                                    --,Enemy.Spelling (parse' "71")
                                    ,Enemy.Run
                                    ]

        , Enemy.dropItem          = [(50, parse' "1d3")]
        , Enemy.dropGold          = parse' "2d10"

        , Enemy.withBackProb      = 15
        , Enemy.backEnemyID       = parse' "2"

        , Enemy.enableRun         = True
        , Enemy.trapCandidate     = [Enemy.Alarm, Enemy.Teleporter, Enemy.Stunner]
    })
    , (EnemyID 3, Enemy.Define {
          Enemy.name              = "zombie"
        , Enemy.nameUndetermined  = "humanoid creature"
        , Enemy.pic               = PictureID 1002
        , Enemy.picUndetermined   = PictureID 2002
        , Enemy.lv                = 2
        , Enemy.hpFormula         = parse' "2d3+1"

        , Enemy.param             = Parameter 5 8 8 8 8 8
        , Enemy.ac                = 5

        , Enemy.exp               = 715
        , Enemy.friendlyProb      = 5
        , Enemy.numOfOccurrences  = parse' "1d3"
        , Enemy.healPerTurn       = 0
        , Enemy.moveFrontProb     = 20

        , Enemy.resistError       = [(Dead, read "12")]
        , Enemy.vsEffectLabels    = [(EffectLabel "purgation", read "-1*value")]
        , Enemy.attrLabels        = [EnemyLabel "undead"]

        , Enemy.actions           = [Enemy.Fight 2 (parse' "1d2+1") (parse' "1d3")
                                     [(read "lv*20-o.lv", Fear 2,  [EffectLabel "fear"])
                                     ,(read "lv*20-o.lv", Poison 1,  [EffectLabel "poison"] )]
                                    ,Enemy.Spelling (parse' "11")
                                    --,Enemy.Spelling (parse' "21")
                                    --,Enemy.Spelling (parse' "71")
                                    ,Enemy.Run
                                    ]

        , Enemy.dropItem          = [(50, parse' "1d3")]
        , Enemy.dropGold          = parse' "2d10"

        , Enemy.withBackProb      = 15
        , Enemy.backEnemyID       = parse' "1"

        , Enemy.enableRun         = True
        , Enemy.trapCandidate     = [Enemy.Alarm, Enemy.Teleporter, Enemy.Stunner]
    })
    ]
