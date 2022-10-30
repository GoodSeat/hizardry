module Engine.BattleAction
where


import qualified Data.Map as Map
import Data.List
import Control.Monad

import Engine.GameAuto
import Engine.Utils
import Data.World
import Data.Formula
import Data.Primitive
import qualified Data.Enemies as Enemy
import qualified Data.Characters as Character
import qualified Data.Spells as Spell


type ActionOfCharacter = CharacterID  -- ^ id of actor.
                      -> Int          -- ^ number that means target.
                      -> GameMachine  -- ^ next game auto.
                      -> GameMachine  -- ^ game auto.


fightOfCharacter :: ActionOfCharacter
fightOfCharacter id l next = GameAuto $ do
    e1 <- aliveEnemyLineHead l
    case e1 of
      Nothing -> run next
      Just e  -> do
        edef   <- enemyOf $ Enemy.id e
        c      <- characterOf id
        (h, d) <- fightDamage l c e
        let (e', _) = setHp (hpOf (e, edef) - d) (e, edef)
        updateEnemy e $ const e'
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
        strBonus
          | str >= 16 = str - 15
          | str < 6   = str - 6
          | otherwise = 0
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
    return $ (m1 ++ m2) : [m1 ++ m3 | not (null m3)]
  where
    vs = ["leaps at", "attempts to slice", "thrusts violently at", "tries to ram", "tries to bash", "charges at", "tries to slash"]

-- ================================================================================

fightOfEnemy :: Enemy.Instance       -- ^ attacker enemy.
             -> Int                  -- ^ count of attack.
             -> Formula              -- ^ damage per hit.
             -> Formula              -- ^ target number. 1~3 are front member, 4~6 are back member.
             -> [(Int, StatusError)] -- ^ additinal effect, and it's probablity.
             -> GameMachine          -- ^ next game auto.
             -> GameMachine          -- ^ game auto.
fightOfEnemy e n dmg tgt sts next = GameAuto $ do
    edef <- enemyOf $ Enemy.id e
    ps   <- party <$> world
    idc  <- flip mod (length ps) <$> eval tgt
    c    <- characterOf (ps !! idc)
    if hpOf c == 0 then run next
    else do
      (h, d) <- fightDamageE n e c dmg
      let c' = setHp (hpOf c - d) c
         -- TODO:lv drain, poison, critical ...etc
      updateCharacter (ps !! idc) c'
      es <- fmap Message <$> fightMessageE e c' (h, d)
      run $ events es next

fightDamageE :: Int                 -- ^ count of attack.
             -> Enemy.Instance      -- ^ attacker enemy.
             -> Character.Character -- ^ target character.
             -> Formula             -- ^ damage per hit.
             -> GameState (Int, Int)
fightDamageE n e c dmg = do
    edef <- enemyOf $ Enemy.id e
    let p  = -2 -- TODO!:parry bonus of c.
        a  = 19 + p - acOf c - lvOf (e, edef)
        b  = a - acOf (e, edef)
        m  = formulaMap (e, edef) c
        hv |  19 <= b  = 19 
           |   0 <= b  = b
           | -36 <= b  = 0
           |   a < 0   = 0
           | otherwise = 19
    rs <- replicateM n $ do
        hit <- (<=) <$> randomNext 1 20 <*> pure (19 - hv)
        dam <- evalWith m dmg
        let dam' = if not . null $ statusErrorsOf c then dam * 2 else dam
        return $ if hit then (1, dam') else (0, 0)
    return $ foldl' (\(h1, d1) (h2, d2) -> (h1 + h2, d1 + d2)) (0, 0) rs

fightMessageE :: Enemy.Instance -> Character.Character -> (Int, Int) -> GameState [String]
fightMessageE e c (h, d) = do
    en <- enemyNameOf e
    v  <- randomIn vs
    let m1 = en ++ " " ++ v ++ "\n " ++ Character.name c ++ ".\n"
    let m2 = if h == 0 then " and misses." else " and hits " ++ show h ++ " times for " ++ show d ++ ".\n"
    let m3 = if hpOf c <= 0 then Character.name c  ++ " is killed." else ""
    return $ (m1 ++ m2) : [m1 ++ m3 | not (null m3)]
  where
    vs = ["charges at", "claws at"]


-- ================================================================================

-- spellOfCharacter :: ActionOfCharacter
-- spellOfCharacter id l = do


type SpellEffect  = Either CharacterID Enemy.Instance
                 -> Int -- ^ target line or character no.
                 -> GameMachine
                 -> GameMachine

spell :: String -> SpellEffect
spell s tgt l next = GameAuto $ do
    spellDef <- spellByName s
    case spellDef of Just def -> run $ spell' def tgt l next
                     Nothing  -> run $ spellUnknown s tgt l next

spell' :: Spell.Define -> SpellEffect
spell' def = case Spell.effect def of
    Spell.Damage f  -> case Spell.target def of
      Spell.OpponentSingle -> castDamageSpellSingle (Spell.name def) f
      Spell.OpponentGroup  -> castDamageSpellGroup  (Spell.name def) f
      Spell.OpponentAll    -> castDamageSpellAll    (Spell.name def) f
      _                    -> undefined
    Spell.Cure f ss -> undefined


castDamageSpellSingle :: String -> Formula -> SpellEffect
castDamageSpellSingle n f (Left id) l next = GameAuto $ do
    e1 <- aliveEnemyLineHead l
    case e1 of Nothing -> run next
               Just e  -> run $ castDamageSpell n f (Right [e]) (Left id) next
castDamageSpellSingle n f (Right e) l next = castDamageSpell n f (Left [l]) (Right e) next

castDamageSpellGroup :: String -> Formula -> SpellEffect
castDamageSpellGroup n f (Left id) l next = GameAuto $ do
    es <- aliveEnemiesLine l
    run $ castDamageSpell n f (Right es) (Left id) next
castDamageSpellGroup n f (Right e) _ next = GameAuto $ do
    ps <- party <$> world
    run $ castDamageSpell n f (Left [1..length ps]) (Right e) next

castDamageSpellAll :: String -> Formula -> SpellEffect
castDamageSpellAll n f (Left id) l next = GameAuto $ do
    es <- sequence $ aliveEnemiesLine <$> [1..4]
    run $ castDamageSpell n f (Right $ concat es) (Left id) next
castDamageSpellAll n f (Right e) l next = castDamageSpellGroup n f (Right e) l next


castDamageSpell :: String -> Formula
                -> Either [Int] [Enemy.Instance]
                -> Either CharacterID Enemy.Instance -> GameMachine -> GameMachine
castDamageSpell n f (Right es) (Left id) next = GameAuto $ do
    c  <- characterOf id
    ts <- forM es $ \e -> do
      edef <- enemyOf $ Enemy.id e
      if Enemy.hp e <= 0 then return []
      else do
        d <- evalWith (formulaMap c (e, edef)) f
        let (e', _) = setHp (hpOf (e, edef) - d) (e, edef) 
        updateEnemy e $ const e'
        let msg = nameOf (e, edef) ++ " takes " ++ show d ++ "."
        return $ msg : [msg ++ "\n" ++ nameOf (e, edef) ++ " is killed." | Enemy.hp e' <= 0]
    let toMsg t = Message $ (nameOf c ++ " spells " ++ n ++ ".\n") ++ t
    run $ events (toMsg <$> "" : concat ts) next

castDamageSpell n f (Left is) (Right e) next = GameAuto $ do
    edef <- enemyOf $ Enemy.id e
    ps   <- party <$> world
    ts   <- forM is $ \i -> do
      let idc = (i - 1) `mod` length ps
      c  <- characterOf (ps !! idc)
      if hpOf c == 0 then return []
      else do
        d <- evalWith (formulaMap (e, edef) c) f
        let c' = setHp (hpOf c - d) c
        updateCharacter (ps !! idc) c'
        let msg = nameOf c ++ " takes " ++ show d ++ "."
        return $ msg : [msg ++ "\n" ++ nameOf c ++ " is killed." | hpOf c' <= 0]
    if null ts then run next
    else do
      let toMsg t = Message $ (nameOf (e, edef) ++ " spells " ++ n ++ ".\n") ++ t
      run $ events (toMsg <$> "" : concat ts) next
    
castDamageSpell _ _ _ _ _ = undefined



spellUnknown :: String -> SpellEffect
spellUnknown n (Left id) _ next = GameAuto $ do
    c <- characterOf id
    let ts  = ["", "no happens."]
        toMsg t = Message $ (Character.name c ++ " spells " ++ n ++ ".\n") ++ t
    run $ events (toMsg <$> ts) next
spellUnknown n (Right e) _ next = GameAuto $ do
    edef <- enemyOf $ Enemy.id e
    let ts  = ["", "no happens."]
        toMsg t = Message $ (Enemy.name edef ++ " spells " ++ n ++ ".\n") ++ t
    run $ events (toMsg <$> ts) next


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


