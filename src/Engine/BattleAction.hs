module Engine.BattleAction
where

import qualified Data.Map as Map
import Data.List
import Data.Maybe (fromJust)
import Data.Function ((&))
import Control.Monad
import Control.Monad.Reader (asks)

import Engine.GameAuto
import Engine.Utils
import Engine.CharacterAction (CastAction, castCureSpell, castParamChangeSpell, castDamageSpell)
import Engine.InEvent (setLightValue)
import Data.World
import Data.Formula
import Data.Primitive
import qualified Data.Enemies as Enemy
import qualified Data.Characters as Chara
import qualified Data.Spells as Spell
import qualified Data.Items as Item


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
        c      <- characterByID id
        (h, d) <- fightDamage el c e
        let e' = damageHp d e
        updateEnemy e $ const e'
        es <- fmap Message <$> fightMessage c e' (h, d)
        run $ events es next

fightDamage :: EnemyLine -> Chara.Character -> Enemy.Instance -> GameState (Int, Int)
fightDamage el c e = do
    wattr <- weaponAttrOf c
    m     <- formulaMapSO (Left c) (Right e)
    eats  <- allValidEquipAttrs c
    weponAt  <- sum <$> mapM (evalWith m . Item.at) eats
    stBonus  <- sum <$> mapM (evalWith m . Item.st) eats
    tryCount <- max <$> evalWith m (Chara.fightTryCount $ Chara.job c) <*> pure weponAt
    jobBonus <- evalWith m (Chara.fightHitBonus $ Chara.job c)
    prm      <- paramOf (Left c)
    acE      <- acOf (Right e)
    let str      = strength prm
        strBonus | str >= 16 = str - 15
                 | str < 6   = str - 6
                 | otherwise = 0
        hitSkill = jobBonus + strBonus + stBonus
        atSkill  = max (min (acE + hitSkill - 3 * enemyLineToNum el) 19) 1
        damageF  = Item.damage wattr
    -- TODO:add statusError, critiacl.
    rs <- replicateM tryCount $ do
        hit <- (<=) <$> randomNext 1 20 <*> pure atSkill
        dam <- (+) <$> evalWith m damageF <*> pure (max 0 strBonus)
        let edef = Enemy.define e
        let dam' = if      not . null $ Enemy.statusErrors e                            then dam * 2
                   else if any (`elem` Item.doubleLabels wattr) (Enemy.attrLabels edef) then dam * 2
                   else dam
        return $ if hit then (1, dam') else (0, 0)
    return $ foldl' (\(h1, d1) (h2, d2) -> (h1 + h2, d1 + d2)) (0, 0) rs

fightMessage :: Chara.Character -> Enemy.Instance -> (Int, Int) -> GameState [String]
fightMessage c e (h, d) = do
    en  <- enemyNameOf e
    vs' <- Item.atackMessages <$> weaponAttrOf c
    v   <- randomIn $ if null vs' then vs else vs'
    let m1 = Chara.name c ++ " " ++ v ++ "\n " ++ en ++ ".\n"
    let m2 = if h == 0 then " and misses." else " and hits " ++ show h ++ " times for " ++ show d ++ ".\n"
    let m3 = if Enemy.hp e <= 0 then en ++ " is killed." else ""
    return $ (m1 ++ m2) : [m1 ++ m3 | not (null m3)]
  where
    vs = ["leaps at", "attempts to slice", "thrusts violently at", "tries to ram", "tries to bash", "charges at", "tries to slash"]

weaponAttrOf :: Chara.Character -> GameState Item.WeaponAttr
weaponAttrOf c = do
    wep <- equipOf c Item.isWeapon
    case wep of
      Nothing  -> return $ Chara.baseWeaponAttr (Chara.job c)
      Just def -> do
        case Item.equipType def of Just (Item.Weapon _ w) -> return w
                                   _                      -> err $ "invalid weaponAttrOf for " ++ show c ++ "."


-- ================================================================================

fightOfEnemy :: Enemy.Instance                        -- ^ attacker enemy.
             -> Int                                   -- ^ count of attack.
             -> Formula                               -- ^ damage per hit.
             -> Formula                               -- ^ target number. 1~3 are front member, 4~6 are back member.
             -> [(Formula, StatusError, EffectLabel)] -- ^ additinal effect, and it's probablity.
             -> GameMachine                           -- ^ next game auto.
             -> GameMachine                           -- ^ game auto.
fightOfEnemy e n dmg tgt sts next = GameAuto $ do
    ps   <- party <$> world
    idc  <- flip mod (length ps) <$> eval tgt
    c    <- characterByID (ps !! idc)
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
    acC  <- acOf (Left c)
    acE  <- acOf (Right e)
    m    <- formulaMapSO (Right e) (Left c)
    let p  = -2 -- TODO!:parry bonus of c.
        a  = 19 + p - acC - lvOf e
        b  = a - acE
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
            c    <- characterByID idc
            know <- knowSpell' c def
            can  <- canSpell'  c def
            let isSilence = c `hasStatusError` Silence
                isFear    = c `hasStatusError` Fear
            run $ if      not know  then spellUnknown s src dst next
                  else if not can   then spellNoMP    s src dst next
                  else if isSilence then spellButSilent s src dst next
                  else if isFear    then spellButFear   s src dst next
                  else                   with [updateCharacter idc =<< costSpell' c def] (spell' def src dst next)
          Right e -> do
            let isSilence = e `hasStatusError` Silence
                isFear    = e `hasStatusError` Fear
            run $ if      isSilence then spellButSilent s src dst next
                  else if isFear    then spellButFear   s src dst next
                  else                   spell' def src dst next
        else
          run $ spellUnknown s src dst next

spell' :: Spell.Define -> SpellEffect
spell' def = case Spell.effect def of
    Spell.Damage f  -> case Spell.target def of
      Spell.OpponentSingle -> castSpellSingle (Spell.name def) (castDamageSpell f)
      Spell.OpponentGroup  -> castSpellGroup  (Spell.name def) (castDamageSpell f)
      Spell.OpponentAll    -> castSpellAll    (Spell.name def) (castDamageSpell f)
      _                    -> undefined
    Spell.Cure f ss -> case Spell.target def of
      Spell.AllySingle     -> castSpellSingle (Spell.name def) (castCureSpell f ss)
      Spell.AllyAll        -> castSpellAll    (Spell.name def) (castCureSpell f ss)
      Spell.Party          -> castSpellAll    (Spell.name def) (castCureSpell f ss)
      _                    -> undefined
    Spell.ChangeParam ad term etxt -> case Spell.target def of
      Spell.AllySingle     -> castSpellSingle (Spell.name def) (castParamChangeSpell ad term etxt)
      Spell.AllyAll        -> castSpellAll    (Spell.name def) (castParamChangeSpell ad term etxt)
      Spell.Party          -> castSpellNull   (Spell.name def) (castParamChangeSpell ad term etxt)
      _                    -> undefined
    Spell.AddLight n s -> \(Left id) _ next -> GameAuto $ do
      c <- characterByID id
      setLightValue s n
      run $ events [Message $ nameOf c ++ " spells " ++ Spell.name def ++ "."] next

-- --------------------------------------------------------------------------------

castSpellNull :: Spell.Name -> CastAction -> SpellEffect
castSpellNull n ca src (Left l)  next = castSpellInBattle n ca src (Left []) next
castSpellNull n ca src (Right _) next = GameAuto $ do
    es <- mapM (aliveEnemiesLine . toEnemyLine) [1..4]
    run $ castSpellInBattle n ca src (Right $ concat es) next

castSpellSingle :: Spell.Name -> CastAction -> SpellEffect
castSpellSingle n ca (Left id) (Left l) next = castSpellInBattle n ca (Left id) (Left [l]) next
castSpellSingle n ca (Left id) (Right el) next = GameAuto $ do
    e1 <- aliveEnemyLineRandom el
    case e1 of Nothing -> run next
               Just e  -> run $ castSpellInBattle n ca (Left id) (Right [e]) next
castSpellSingle n ca (Right e) (Left l) next = castSpellInBattle n ca (Right e) (Left [l]) next
castSpellSingle n ca (Right se) (Right el) next = GameAuto $ do
    e1 <- aliveEnemyLineRandom el
    case e1 of Nothing -> run next
               Just e  -> run $ castSpellInBattle n ca (Right se) (Right [e]) next

castSpellGroup :: Spell.Name -> CastAction -> SpellEffect
castSpellGroup n ca src (Right el) next = GameAuto $ do
    es <- aliveEnemiesLine el
    run $ castSpellInBattle n ca src (Right es) next
castSpellGroup n ca src (Left _) next = GameAuto $ do
    ps <- party <$> world
    run $ castSpellInBattle n ca src (Left $ toPartyPos <$> [1..length ps]) next

castSpellAll :: Spell.Name -> CastAction -> SpellEffect
castSpellAll n ca src (Left _) next = GameAuto $ do
    ps <- party <$> world
    run $ castSpellInBattle n ca src (Left $ toPartyPos <$> [1..length ps]) next
castSpellAll n ca src (Right _) next = GameAuto $ do
    es <- mapM (aliveEnemiesLine . toEnemyLine) [1..4]
    run $ castSpellInBattle n ca src (Right $ concat es) next


castSpellInBattle :: Spell.Name
                  -> CastAction
                  -> Either CharacterID Enemy.Instance  -- src
                  -> Either [PartyPos] [Enemy.Instance] -- dst
                  -> GameMachine -> GameMachine
castSpellInBattle n ca (Left cid) dst next = GameAuto $ do
    src <- characterByID cid
    ts  <- ca (Left src) dst
    let toMsg t = Message $ (nameOf src ++ " spells " ++ n ++ ".\n") ++ t
    run $ events (toMsg <$> "" : (snd <$> ts)) (with (fst <$> ts) next)
castSpellInBattle n ca (Right e) dst next = GameAuto $ do
    ts <- ca (Right e) dst
    let toMsg t = Message $ (nameOf e ++ " spells " ++ n ++ ".\n") ++ t
    run $ events (toMsg <$> "" : (snd <$> ts)) (with (fst <$> ts) next)

-- --------------------------------------------------------------------------------

spellUnknown :: Spell.Name -> SpellEffect
spellUnknown = spellNoEffect "no happens."

spellNoMP :: Spell.Name -> SpellEffect
spellNoMP = spellNoEffect "no more MP."

spellButSilent :: Spell.Name -> SpellEffect
spellButSilent = spellNoEffect "but it wasn't voiced."

spellButFear :: Spell.Name -> SpellEffect
spellButFear = spellNoEffect "but couldn't voice well by fear."

spellNoEffect :: String -> Spell.Name -> SpellEffect
spellNoEffect msg n src _ next = GameAuto $ do
    name <- case src of Left id -> Chara.name <$> characterByID id
                        Right e -> Enemy.name <$> enemyDefineByID (Enemy.id e)
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

aliveEnemyLineRandom :: EnemyLine -> GameState (Maybe Enemy.Instance)
aliveEnemyLineRandom el = do
    es <- aliveEnemiesLine el
    if null es then return Nothing
               else Just <$> randomIn es

-- ================================================================================
enemyNameOf :: Enemy.Instance -> GameState String
enemyNameOf e = nameOf <$> enemyDefineByID (Enemy.id e)
  where
    nameOf = if Enemy.determined e then Enemy.name else Enemy.nameUndetermined

