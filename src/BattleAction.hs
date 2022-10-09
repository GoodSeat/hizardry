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
import qualified Spells as Spell


type ActionOfCharacter = Character.ID -- ^ id of actor.
                      -> Int          -- ^ number that means target.
                      -> GameAuto     -- ^ next game auto.
                      -> GameAuto     -- ^ game auto.


fightOfCharacter :: ActionOfCharacter
fightOfCharacter id l next = GameAuto $ do
    e1 <- aliveEnemyLineHead l
    case e1 of
      Nothing -> run next
      Just e  -> do
        c <- characterOf id
        (h, d) <- fightDamage l c e
        let hp' = Enemy.hp e - d
            st' = Enemy.statusErrors e ++ [Dead | hp' <= 0]
            e'  = e { Enemy.hp = hp', Enemy.statusErrors = st' }
        updateEnemy l e $ const e' 
        es <- fmap Message <$> fightMessage c e' (h, d)
        run $ events es next

fightDamage :: Int -> Character.Character -> Enemy.Instance -> GameState (Int, Int)
fightDamage l c e = do
    edef <- enemyOf $ Enemy.id e
    let tryCountF = parse' "lv/5 + 1" -- TODO:
        jobBonusF = parse' "lv/3 + 2" -- TODO:
        damageF   = parse' "2d2"      -- TODO:from wepon.
        weponAt   = 0 -- TODO:AT value of wepon.
        stBonus   = 0 -- TODO:sum of equip item's ST value.
        m         = formulaMap c (e, edef)
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

-- ================================================================================

-- spellOfCharacter :: ActionOfCharacter
-- spellOfCharacter id l = do
    

type SpellEffect  = Either Character.ID Enemy.Instance
                 -> Int
                 -> GameAuto
                 -> GameAuto



damageSpell :: Object s => Object o => Formula -> s -> o -> GameState (o, Int)
damageSpell f s o = do
   d <- evalWith (formulaMap s o) f
   let hp' = hpOf o - d
       st' = statusErrorsOf o ++ [Dead | hp' <= 0]
       o'  = setHp hp' . setStatusErrors st' $ o
   return (o', d)

halito :: Object s => Object o => s -> o -> GameState (o, Int)
halito = damageSpell $ parse' "1d8"

spellHalito :: SpellEffect
spellHalito (Left id) l next = GameAuto $ do
    e1 <- aliveEnemyLineHead l
    case e1 of
      Nothing -> run next
      Just e  -> do
        c            <- characterOf id
        edef         <- enemyOf $ Enemy.id e
        ((e', _), d) <- halito c (e, edef)
        updateEnemy l e $ const e'
        let ts  = ["", Enemy.name edef ++ " takes " ++ show d ++ "."]
            ts' = ts ++ (if Enemy.hp e' <= 0 then [last ts ++ "\n" ++ Enemy.name edef ++ " is killed."] else [])
            toMsg t = Message $ (Character.name c ++ " spells halito.\n") ++ t
        run $ events (toMsg <$> ts') next

spellUnkown :: String -> SpellEffect
spellUnkown n (Left id) _ next = GameAuto $ do
    c <- characterOf id
    let ts  = ["", "no happens."]
        toMsg t = Message $ (Character.name c ++ " spells " ++ n ++ ".\n") ++ t 
    run $ events (toMsg <$> ts) next

spell :: String -> SpellEffect
spell s = if s == "halito" then spellHalito else spellUnkown s


-- ==========================================================================
aliveEnemiesLine :: Int -> GameState [Enemy.Instance]
aliveEnemiesLine l = filter (\e -> Enemy.hp e > 0) <$> ((!!) <$> lastEnemies <*> pure (l - 1))

aliveEnemyLineHead :: Int -> GameState (Maybe Enemy.Instance)
aliveEnemyLineHead l = do
    es <- aliveEnemiesLine l
    return $ if null es then Nothing else Just $ head es


-- ================================================================================
enemyNameOf :: Enemy.Instance -> GameState String
enemyNameOf e = nameOf <$> enemyOf (Enemy.id e)
  where
    nameOf = if Enemy.determined e then Enemy.name else Enemy.nameUndetermined


formulaMap :: Object s => Object o => s -> o -> Map.Map String Int
formulaMap s o = Map.fromList [
     ("ac"      , acOf s)
    ,("lv"      , lvOf s)
    ,("hp"      , hpOf s)
    ,("maxhp"   , maxhpOf s)
    ,("str"     , strength.paramOf $ s)
    ,("iq"      , iq      .paramOf $ s)
    ,("pie"     , piety   .paramOf $ s)
    ,("vit"     , vitality.paramOf $ s)
    ,("agi"     , agility .paramOf $ s)
    ,("luc"     , luck    .paramOf $ s)
    ,("o.ac"    , acOf o)
    ,("o.lv"    , lvOf o)
    ,("o.hp"    , hpOf o)
    ,("o.maxhp" , maxhpOf o)
    ,("o.str"   , strength.paramOf $ o)
    ,("o.iq"    , iq      .paramOf $ o)
    ,("o.pie"   , piety   .paramOf $ o)
    ,("o.vit"   , vitality.paramOf $ o)
    ,("o.agi"   , agility .paramOf $ o)
    ,("o.luc"   , luck    .paramOf $ o)
    ]


