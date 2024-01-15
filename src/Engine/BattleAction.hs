module Engine.BattleAction
where


import qualified Data.Map as Map
import Data.List
import Data.Function ((&))
import Control.Monad
import Control.Monad.Reader (asks)

import Engine.GameAuto
import Engine.Utils
import Engine.CharacterAction (castCureSpell)
import Engine.InEvent (setLightValue)
import Data.World
import Data.Formula
import Data.Primitive
import qualified Data.Enemies as Enemy
import qualified Data.Characters as Chara
import qualified Data.Spells as Spell


type ActionOfCharacter = CharacterID  -- ^ id of actor.
                      -> EnemyLine    -- ^ number that means target.
                      -> GameMachine  -- ^ next game auto.
                      -> GameMachine  -- ^ game auto.


fightOfCharacter :: ActionOfCharacter
fightOfCharacter id el next = GameAuto $ do
    e1 <- aliveEnemyLineHead el
    case e1 of
      Nothing -> run next
      Just e  -> do
        edef   <- enemyOf $ Enemy.id e
        c      <- characterOf id
        (h, d) <- fightDamage el c e
        let (e', _) = damageHp d (e, edef)
        updateEnemy e $ const e'
        es <- fmap Message <$> fightMessage c e' (h, d)
        run $ events es next

fightDamage :: EnemyLine -> Chara.Character -> Enemy.Instance -> GameState (Int, Int)
fightDamage el c e = do
    edef <- enemyOf $ Enemy.id e
    let tryCountF = parse' "lv/5 + 1" -- TODO:
        jobBonusF = parse' "lv/3 + 2" -- TODO:
        damageF   = parse' "2d2"      -- TODO:from wepon.
        weponAt   = 0 -- TODO:AT value of wepon.
        stBonus   = 0 -- TODO:sum of equip item's ST value.
        m         = formulaMapSO c (e, edef)
    tryCount <- max <$> (min <$> evalWith m tryCountF <*> pure 10) <*> pure weponAt
    jobBonus <- evalWith m jobBonusF
    let str      = strength . Chara.param $ c
        strBonus
          | str >= 16 = str - 15
          | str < 6   = str - 6
          | otherwise = 0
        hitSkill = jobBonus + strBonus + stBonus
        atSkill  = max (min (Enemy.ac edef + hitSkill - 3 * enemyLineToNum el) 19) 1
    rs <- replicateM tryCount $ do
        hit <- (<=) <$> randomNext 1 20 <*> pure atSkill
        dam <- (+) <$> evalWith m damageF <*> pure (max 0 strBonus)
        let dam' = if not . null $ Enemy.statusErrors e then dam * 2 else dam
        return $ if hit then (1, dam') else (0, 0)
    return $ foldl' (\(h1, d1) (h2, d2) -> (h1 + h2, d1 + d2)) (0, 0) rs

fightMessage :: Chara.Character -> Enemy.Instance -> (Int, Int) -> GameState [String]
fightMessage c e (h, d) = do
    en <- enemyNameOf e
    v  <- randomIn vs
    let m1 = Chara.name c ++ " " ++ v ++ "\n " ++ en ++ ".\n"
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
      let c' = damageHp d c
         -- TODO:lv drain, poison, critical ...etc
      es <- fmap Message <$> fightMessageE e c' (h, d)
      run $ events es (with [updateCharacter (ps !! idc) c'] next)

fightDamageE :: Int             -- ^ count of attack.
             -> Enemy.Instance  -- ^ attacker enemy.
             -> Chara.Character -- ^ target character.
             -> Formula         -- ^ damage per hit.
             -> GameState (Int, Int)
fightDamageE n e c dmg = do
    edef <- enemyOf $ Enemy.id e
    let p  = -2 -- TODO!:parry bonus of c.
        a  = 19 + p - acOf c - lvOf (e, edef)
        b  = a - acOf (e, edef)
        m  = formulaMapSO (e, edef) c
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

fightMessageE :: Enemy.Instance -> Chara.Character -> (Int, Int) -> GameState [String]
fightMessageE e c (h, d) = do
    en <- enemyNameOf e
    v  <- randomIn vs
    let m1 = en ++ " " ++ v ++ "\n " ++ Chara.name c ++ ".\n"
    let m2 = if h == 0 then " and misses." else " and hits " ++ show h ++ " times for " ++ show d ++ ".\n"
    let m3 = if hpOf c <= 0 then Chara.name c  ++ " is killed." else ""
    return $ (m1 ++ m2) : [m1 ++ m3 | not (null m3)]
  where
    vs = ["charges at", "claws at"]


-- ================================================================================


type SpellEffect  = Either CharacterID Enemy.Instance
                 -> SpellTarget -- ^ target line or character no.
                 -> GameMachine
                 -> GameMachine

spell :: Spell.Name -> SpellEffect
spell s src dst next = GameAuto $ do
    spellDef <- spellByName s
    case spellDef of
      Nothing  -> run $ spellUnknown s src dst next
      Just def ->
        if Spell.InBattle `elem` Spell.enableIn def then case src of
          Left idc -> do
            c    <- characterOf idc
            know <- knowSpell' c def
            can  <- canSpell'  c def
            if      not know then run $ spellUnknown s src dst next
            else if not can  then run $ spellNoMP    s src dst next
            else                  run $ with [updateCharacter idc =<< costSpell' c def] (spell' def src dst next)
          Right _  -> run $ spell' def src dst next
        else
          run $ spellUnknown s src dst next

spell' :: Spell.Define -> SpellEffect
spell' def = case Spell.effect def of
    Spell.Damage f  -> case Spell.target def of
      Spell.OpponentSingle -> castDamageSpellSingle (Spell.name def) f
      Spell.OpponentGroup  -> castDamageSpellGroup  (Spell.name def) f
      Spell.OpponentAll    -> castDamageSpellAll    (Spell.name def) f
      _                    -> undefined
    Spell.Cure f ss -> case Spell.target def of
      Spell.AllySingle     -> castCureSpellSingle (Spell.name def) f ss
      Spell.AllyAll        -> castCureSpellAll    (Spell.name def) f ss
      _                    -> undefined
    Spell.AddLight n -> \(Left id) _ next -> GameAuto $ do 
        c  <- characterOf id
        setLightValue n
        run $ events [Message $ nameOf c ++ " spells " ++ Spell.name def ++ "."] next

-- --------------------------------------------------------------------------------

castDamageSpellSingle :: Spell.Name -> Formula -> SpellEffect
castDamageSpellSingle n f (Left id) (Right el) next = GameAuto $ do
    e1 <- aliveEnemyLineHead el
    case e1 of Nothing -> run next
               Just e  -> run $ castDamageSpell n f (Right [e]) (Left id) next
castDamageSpellSingle n f (Right e) (Left l) next = castDamageSpell n f (Left [l]) (Right e) next
castDamageSpellSingle _ _ _ _ _ = error "invalid castDamageSpellSingle"

castDamageSpellGroup :: Spell.Name -> Formula -> SpellEffect
castDamageSpellGroup n f (Left id) (Right el) next = GameAuto $ do
    es <- aliveEnemiesLine el
    run $ castDamageSpell n f (Right es) (Left id) next
castDamageSpellGroup n f (Right e) _ next = GameAuto $ do
    ps <- party <$> world
    run $ castDamageSpell n f (Left $ toPartyPos <$> [1..length ps]) (Right e) next
castDamageSpellGroup _ _ _ _ _ = error "invalid castDamageSpellGroup"

castDamageSpellAll :: Spell.Name -> Formula -> SpellEffect
castDamageSpellAll n f (Left id) l next = GameAuto $ do
    es <- sequence $ aliveEnemiesLine . toEnemyLine <$> [1..4]
    run $ castDamageSpell n f (Right $ concat es) (Left id) next
castDamageSpellAll n f (Right e) l next = castDamageSpellGroup n f (Right e) l next


castDamageSpell :: Spell.Name -> Formula
                -> Either [PartyPos] [Enemy.Instance] -- ^ dst
                -> Either CharacterID Enemy.Instance  -- ^ src
                -> GameMachine -> GameMachine
castDamageSpell n f (Right es) (Left id) next = GameAuto $ do
    c  <- characterOf id
    ts <- forM es $ \e -> do
      edef <- enemyOf $ Enemy.id e
      if Enemy.hp e <= 0 then return []
      else do
        d <- evalWith (formulaMapSO c (e, edef)) f
        let (e', _) = damageHp d (e, edef)
        updateEnemy e $ const e'
        let msg = nameOf (e, edef) ++ " takes " ++ show d ++ "."
        return $ msg : [msg ++ "\n" ++ nameOf (e, edef) ++ " is killed." | Enemy.hp e' <= 0]
    let toMsg t = Message $ (nameOf c ++ " spells " ++ n ++ ".\n") ++ t
    run $ events (toMsg <$> "" : concat ts) next

castDamageSpell n f (Left is) (Right e) next = GameAuto $ do
    edef <- enemyOf $ Enemy.id e
    ts   <- forM is $ \i -> do
      c <- partyAt' i
      if hpOf c == 0 then return []
      else do
        d <- evalWith (formulaMapSO (e, edef) c) f
        let c' = damageHp d c
        let msg = nameOf c ++ " takes " ++ show d ++ "."
        return $ (join $ updateCharacter <$> partyAt i <*> pure c', msg)
               : [(return (), msg ++ "\n" ++ nameOf c ++ " is killed.") | hpOf c' <= 0]
    if null ts then run next
    else do
      let toMsg t = Message $ (nameOf (e, edef) ++ " spells " ++ n ++ ".\n") ++ t
      run $ events (toMsg <$> "" : (snd <$> concat ts)) (with (fst <$> concat ts) next)
    
castDamageSpell _ _ _ _ _ = error "castDamageSpell"

-- --------------------------------------------------------------------------------

castCureSpellSingle :: Spell.Name -> Formula -> [StatusError] -> SpellEffect
castCureSpellSingle n f ss (Left id) (Left l) next = GameAuto $ do
    ps <- party <$> world
    run $ castCureSpellInBattle n f ss (Left [l]) (Left id) next
castCureSpellSingle _ _ _ _ _ _ = error "castCureSpellSingle"

castCureSpellAll n f ss (Left id) _ next = GameAuto $ do
    ps <- party <$> world
    run $ castCureSpellInBattle n f ss (Left $ toPartyPos <$> [1..length ps]) (Left id) next
castCureSpellAll _ _ _ _ _ _ = error "castCureSpellAll"

castCureSpellInBattle :: Spell.Name -> Formula -> [StatusError]
              -> Either [PartyPos] [Enemy.Instance]
              -> Either CharacterID Enemy.Instance -> GameMachine -> GameMachine
castCureSpellInBattle n f ss dst (Left cid) next = GameAuto $ do
    wiz <- characterOf cid
    ts  <- castCureSpell n f ss (Left wiz) dst
    let toMsg t = Message $ (nameOf wiz ++ " spells " ++ n ++ ".\n") ++ t
    run $ events (toMsg <$> "" : (snd <$> ts)) (with (fst <$> ts) next)
castCureSpellInBattle _ _ _ _ _ _ = error "castCureSpellInBattle"

-- --------------------------------------------------------------------------------

spellUnknown :: Spell.Name -> SpellEffect
spellUnknown = spellNoEffect "no happens."

spellNoMP :: Spell.Name -> SpellEffect
spellNoMP = spellNoEffect "no more MP."

spellNoEffect :: String -> Spell.Name -> SpellEffect
spellNoEffect msg n src _ next = GameAuto $ do
    name <- case src of Left id -> Chara.name <$> characterOf id
                        Right e -> Enemy.name <$> enemyOf (Enemy.id e)
    let ts      = ["", msg]
        toMsg t = Message $ (name ++ " spells " ++ n ++ ".\n") ++ t
    run $ events (toMsg <$> ts) next

-- ==========================================================================
aliveEnemiesLine :: EnemyLine -> GameState [Enemy.Instance]
aliveEnemiesLine el = do
  ess <- lastEnemies
  if length ess < enemyLineToNum el then return []
  else return $ filter (\e -> Enemy.hp e > 0) (ess !! (enemyLineToNum el - 1))

aliveEnemyLineHead :: EnemyLine -> GameState (Maybe Enemy.Instance)
aliveEnemyLineHead el = do
    es <- aliveEnemiesLine el
    return $ if null es then Nothing else Just $ head es


-- ================================================================================
enemyNameOf :: Enemy.Instance -> GameState String
enemyNameOf e = nameOf <$> enemyOf (Enemy.id e)
  where
    nameOf = if Enemy.determined e then Enemy.name else Enemy.nameUndetermined

