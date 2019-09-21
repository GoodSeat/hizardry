{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BattleAction
where


import qualified Data.Map as Map
import Data.List
import Control.Monad
import GameAuto
import World
import Utils
import Formula
import Primitive
import qualified Enemies as Enemy
import qualified Characters as Character


type ActionOfCharacter = Character.ID       -- ^ id of actor.
                      -> Int                -- ^ number that means target.
                      -> GameState [String] -- ^ text that represent result.


fightOfCharacter :: ActionOfCharacter
fightOfCharacter id l = do
    ses <- Character.statusErrors <$> characterOf id
    enm <- filter (\e -> Enemy.hp e > 0) <$> ((!!) <$> lastEnemies <*> pure (l - 1))
    if   any (`elem` Character.cantFightStatus) ses || null enm then return []
    else do
        c <- characterOf id
        e <- return $ head enm
        (h, d) <- fightDamage l c e
        let hp' = Enemy.hp e - d
            st' = Enemy.statusErrors e ++ [Dead | hp' <= 0]
            e'  = e { Enemy.hp = hp', Enemy.statusErrors = st' }
        updateEnemy l e $ const e' 
        fightMessage c e' (h, d)

fightDamage :: Int -> Character.Character -> Enemy.Instance -> GameState (Int, Int)
fightDamage l c e = do
    edef <- enemyOf $ Enemy.id e
    let tryCountF = parse' "lv/5 + 1" -- TODO:
        jobBonusF = parse' "lv/3 + 2" -- TODO:
        damageF   = parse' "2d2"      -- TODO:from wepon.
        weponAt   = 0 -- TODO:AT value of wepon.
        stBonus   = 0 -- TODO:sum of equip item's ST value.
        m         = makeMap edef
    tryCount <- max <$> (min <$> evalWith m tryCountF <*> pure 10) <*> pure weponAt
    jobBonus <- evalWith m jobBonusF
    let str      = strength . Character.param $ c
        strBonus = if str >= 16 then str - 15 else if str < 6 then str - 6 else 0
        hitSkill = jobBonus + strBonus + stBonus
        atSkill  = max (min (Enemy.ac edef + hitSkill - 3 * l) 19) 1
    rs <- replicateM tryCount $ do
        hit <- (<=) <$> randomNext 1 20 <*> pure atSkill
        dam <- (+) <$> evalWith m damageF <*> pure (max 0 strBonus)
        let dam' = if not . null $ Enemy.statusErrors e then dam * 2 else dam
        return $ if hit then (1, dam') else (0, 0)
    return $ foldl' (\(h1, d1) (h2, d2) -> (h1 + h2, d1 + d2)) (0, 0) rs
  where
    makeMap edef = Map.fromList [("ac", Enemy.ac edef)
                                ,("lv", Character.lv c)
                                ,("str", strength . Character.param $ c)
                                ]

fightMessage :: Character.Character -> Enemy.Instance -> (Int, Int) -> GameState [String]
fightMessage c e (h, d) = do
    en <- enemyNameOf e
    v  <- randomIn vs
    let m1 = Character.name c ++ " " ++ v ++ "\n " ++ en ++ ".\n"
    let m2 = if h == 0 then " and misses." else " and hits " ++ show h ++ " times for " ++ show d ++ ".\n"
    let m3 = if Enemy.hp e <= 0 then en ++ " is killed." else ""
    return $ (m1 ++ m2) : if null m3 then [] else [m1 ++ m3]
  where
    vs = ["leaps at", "attempts to slice", "thrusts violently at", "tries to ram", "tries to bash", "charges at", "tries to slash"]

enemyNameOf :: Enemy.Instance -> GameState String
enemyNameOf e = nameOf <$> enemyOf (Enemy.id e)
  where
    nameOf = if Enemy.determined e then Enemy.name else Enemy.nameUndetermined

