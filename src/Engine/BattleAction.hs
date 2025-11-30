{-# LANGUAGE TupleSections #-}
module Engine.BattleAction
where

import Prelude hiding (lookup)
import qualified Data.Map as Map
import Data.List hiding (lookup)
import Data.Maybe (fromJust)
import Data.Function ((&))
import Data.Map hiding (filter, null, foldl, foldl', foldr, take, drop)
import Control.Monad
import Control.Monad.Reader (asks)
import Control.Monad.State (modify)

import Engine.GameAuto
import Engine.Utils
import Engine.CharacterAction (
      CastAction
    , castCureSpell
    , castParamChangeSpell
    , castDamageSpell
    , castAddLight
    , castResurrectionSpell
    , castAddStatusErrorSpell
    , breakItem
    )
import Engine.InEvent (setLightValue, doEvent)
import Data.World
import Data.Formula
import Data.Primitive
import qualified Data.Enemies as Enemy
import qualified Data.Characters as Chara
import qualified Data.Spells as Spell
import qualified Data.Items as Item

import Control.CUI (translate)

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
        c           <- characterByID id
        (h, d, ses) <- fightDamage el c e
        let e' = damageHp d e
        updateEnemy e $ const e'
        ms <- fightMessage c e' (h, d, ses)
        run $ if d == 0 || el /= L1 then events (message <$> ms) next
                                    else toEffect False (head ms) (events (message <$> tail ms) next)

fightDamage :: EnemyLine
            -> Chara.Character
            -> Enemy.Instance
            -> GameState (Int, Int, [StatusError])
fightDamage el c e = do
    wattr <- weaponAttrOf c
    m     <- formulaMapSO (Left c) (Right e)
    eats  <- allValidEquipAttrs c
    vs    <- vsEffectLabelsOf (Right e)
    weponAt  <- sum <$> mapM (evalWith m . Item.at) eats
    stBonus  <- sum <$> mapM (evalWith m . Item.st) eats
    tryCount <- max <$> evalWith m (Chara.fightTryCount $ Chara.job c) <*> pure weponAt
    jobBonus <- evalWith m (Chara.fightHitBonus $ Chara.job c)
    prm      <- paramOf (Left c)
    acE      <- acOf (Right e)
    let edef     = Enemy.define e
        str      = strength prm
        strBonus | str >= 16 = str - 15
                 | str < 6   = str - 6
                 | otherwise = 0
        hitSkill = jobBonus + strBonus + stBonus
        atSkill  = max (min (acE + hitSkill - 3 * enemyLineToNum el) 19) 1
        damageF  = Item.damage wattr
    rs <- replicateM tryCount $ do
        hit <- (<=) <$> randomNext 1 20 <*> pure atSkill
        dam <- (+) <$> evalWith m damageF <*> pure (max 0 strBonus)
        let dam' | not . null $ Enemy.statusErrors e                            = dam * 2
                 | any (`elem` Item.doubleLabels wattr) (Enemy.attrLabels edef) = dam * 2
                 | otherwise                                                    = dam
        dam'' <- applyVsEffect (Item.attrLabels wattr) vs (Left c) (Right e) dam'
        return $ if hit then (1, dam'') else (0, 0)
    let dh = foldl' (\(h1, d1) (h2, d2) -> (h1 + h2, d1 + d2)) (0, 0) rs
    ses <- if snd dh == 0 then return []
           else flip filterM (Item.addStatusErrors wattr) $ \(prob, se, attrs) -> do
             m  <- formulaMapSO (Left c) (Right e)
             p  <- evalWith m prob
             p' <- applyVsEffect attrs vs (Left c) (Right e) p
             resist <- resistStatusError m se (Enemy.resistError edef)
             (&&) <$> happens p' <*> pure (not resist)
    return (fst dh, snd dh, (\(_,s,_) -> s) <$> ses)

fightMessage :: Chara.Character -> Enemy.Instance -> (Int, Int, [StatusError]) -> GameState [String]
fightMessage c e (h, d, ses) = do
    en  <- enemyNameOf e
    vs' <- Item.atackMessages <$> weaponAttrOf c
    v   <- randomIn $ if null vs' then vs else vs'
    let m1 = Chara.name c ++ " " ++ v ++ "\n " ++ en ++ ".\n"
    let m2 = if h == 0 then " and misses." else " and hits " ++ show h ++ " times for " ++ show d ++ ".\n"
    let m3 = if Enemy.hp e <= 0 then [en ++ " is killed."]
             else (en ++) . statusErrorMessage <$> sort ses
    return $ (m1 ++ m2) : [m1 ++ x | x <- m3]
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

fightOfEnemy :: Enemy.Instance                          -- ^ attacker enemy.
             -> Int                                     -- ^ count of attack.
             -> Formula                                 -- ^ damage per hit.
             -> Formula                                 -- ^ target number. 1~3 are front member, 4~6 are back member.
             -> [(Formula, StatusError, [EffectLabel])] -- ^ additinal effect, and it's probablity.
             -> GameMachine                             -- ^ next game auto.
             -> GameMachine                             -- ^ game auto.
fightOfEnemy e n dmg tgt sts next = GameAuto $ do
    ps   <- party <$> world
    idc  <- flip mod (length ps) <$> eval tgt
    c    <- characterByID (ps !! idc)
    if hpOf c == 0 then run next
    else do
      (h, d, ses) <- fightDamageE n e c dmg sts
      let c' = foldl (&) (damageHp d c) (addStatusError <$> ses)
         -- TODO:lv drain
      ms <- fightMessageE e c' (h, d, ses)
      let next' = with [updateCharacter (ps !! idc) c'] next
      run $ if d == 0 then events (message <$> ms) next'
                      else toEffect True (head ms) (events (message <$> tail ms) next')

fightDamageE :: Int             -- ^ count of attack.
             -> Enemy.Instance  -- ^ attacker enemy.
             -> Chara.Character -- ^ target character.
             -> Formula         -- ^ damage per hit.
             -> [(Formula, StatusError, [EffectLabel])] -- ^ additinal effect, and it's probablity.
             -> GameState (Int, Int, [StatusError])
fightDamageE n e c dmg sts = do
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
    let dh = foldl' (\(h1, d1) (h2, d2) -> (h1 + h2, d1 + d2)) (0, 0) rs
    ses <- if snd dh == 0 then return []
           else flip filterM sts $ \(prob, se, attrs) -> do
             m  <- formulaMapSO (Right e) (Left c)
             p  <- evalWith m prob
             vs <- vsEffectLabelsOf (Left c)
             p' <- applyVsEffect attrs vs (Right e) (Left c) p
             eats <- allValidEquipAttrs c
             resist <- resistStatusError m se (concatMap Item.resistError eats)
             (&&) <$> happens p' <*> pure (not resist)
    return (fst dh, snd dh, (\(_,s,_) -> s) <$> ses)

fightMessageE :: Enemy.Instance -> Chara.Character -> (Int, Int, [StatusError]) -> GameState [String]
fightMessageE e c (h, d, ses) = do
    en <- enemyNameOf e
    v  <- randomIn vs
    let m1 = en ++ " " ++ v ++ "\n " ++ Chara.name c ++ ".\n"
    let m2 = if h == 0 then " and misses." else " and hits " ++ show h ++ " times for " ++ show d ++ ".\n"
    let m3 = if hpOf c <= 0 then [Chara.name c  ++ " is killed."]
             else (Chara.name c ++) . statusErrorMessage <$> sort ses
    return $ (m1 ++ m2) : [m1 ++ x | x <- m3]
  where
    vs = ["charges at", "claws at"]


-- ================================================================================

verbForItem  = "uses"

useItemInBattle :: Chara.ItemPos -> SpellEffect
useItemInBattle i (Left cid) dst next = GameAuto $ do
    c   <- characterByID cid
    def <- itemByID $ Chara.itemAt c i
    let n = Item.name def
    case Item.usingEffect def of
      Nothing                     -> run $ asItem castUnknown n (Left cid) dst next
      Just (Item.EqSpell ids, bp) -> do
         sdef' <- spellByID ids
         case sdef' of
           Just sdef -> run $ use n sdef (Left cid) dst (with [breakItem bp cid i] next)
           Nothing   -> error "invalid spellId in useItemInBattle"
      Just (Item.Happens eid, bp) -> do
         let next' = with [breakItem bp cid i] next
         edef' <- asks (lookup eid . mazeEvents)
         case edef' of Nothing   -> run next'
                       Just edef -> run $ doEvent edef (const next') (const next') (\sdef n -> spell' sdef (Left cid) dst n)

useItemInBattle i (Right ei) dst next = undefined -- TODO!:considering possible using item by ememy, first argument must change to item id.

use :: String -> Spell.Define -> SpellEffect
use name def = if Spell.InBattle `elem` Spell.enableIn def then cast verbForItem name def 
                                                           else asItem castUnknown name 

-- ================================================================================

verbForSpell = "spells"


type SpellEffect  = Either CharacterID Enemy.Instance
                 -> SpellTarget -- ^ target line or character no.
                 -> GameMachine
                 -> GameMachine

spell :: Spell.Name -> SpellEffect
spell s src dst next = GameAuto $ do
    spellDef <- spellByName s
    case spellDef of
      Nothing  -> run $ asSpell castUnknown s src dst next
      Just def ->
        if Spell.InBattle `elem` Spell.enableIn def then case src of
          Left idc -> do
            c    <- characterByID idc
            know <- knowSpell' c def
            can  <- canSpell'  c def
            let isSilence = c `hasStatusError` Silence
                isFear    = c `hasStatusError` Fear
            run $ if      not know  then asSpell castUnknown s src dst next
                  else if not can   then asSpell castNoMP    s src dst next
                  else if isSilence then asSpell castButSilent s src dst next
                  else if isFear    then asSpell castButFear   s src dst next
                  else                   with [updateCharacter idc =<< costSpell' c def] (spell' def src dst next)
          Right e -> do
            let isSilence = e `hasStatusError` Silence
                isFear    = e `hasStatusError` Fear
            run $ if      isSilence then asSpell castButSilent s src dst next
                  else if isFear    then asSpell castButFear   s src dst next
                  else                   spell' def src dst next
        else
          run $ asSpell castUnknown s src dst next

spell' :: Spell.Define -> SpellEffect
spell' def = cast verbForSpell (Spell.name def) def

cast :: Verb -> String -> Spell.Define -> SpellEffect
cast v name def = let as cast = cast v in case Spell.effect def of
    Spell.Damage f  -> case Spell.target def of
      Spell.OpponentSingle -> castToSingle as name (castDamageSpell f $ Spell.attrLabels def)
      Spell.OpponentGroup  -> castToGroup  as name (castDamageSpell f $ Spell.attrLabels def)
      Spell.OpponentAll    -> castToAll    as name (castDamageSpell f $ Spell.attrLabels def)
      _                    -> undefined
    Spell.Cure f ss -> case Spell.target def of
      Spell.AllySingle     -> castToSingle as name (castCureSpell f ss)
      Spell.AllyAll        -> castToAll    as name (castCureSpell f ss)
      Spell.Party          -> castToAll    as name (castCureSpell f ss)
      _                    -> undefined
    Spell.Resurrection hp ts -> case Spell.target def of
      Spell.AllySingle     -> castToSingle as name (castResurrectionSpell hp ts)
      Spell.AllyAll        -> castToAll    as name (castResurrectionSpell hp ts)
      Spell.Party          -> castToAll    as name (castResurrectionSpell hp ts)
      _                    -> undefined
    Spell.ChangeParam ad term etxt -> case Spell.target def of
      Spell.AllySingle     -> castToSingle as name (castParamChangeSpell ad term etxt)
      Spell.AllyAll        -> castToAll    as name (castParamChangeSpell ad term etxt)
      Spell.Party          -> castToNull   as name (castParamChangeSpell ad term etxt)
      _                    -> undefined
    Spell.AddStatusError ts -> case Spell.target def of
      Spell.OpponentSingle -> castToSingle as name (castAddStatusErrorSpell ts)
      Spell.OpponentGroup  -> castToGroup  as name (castAddStatusErrorSpell ts)
      Spell.OpponentAll    -> castToAll    as name (castAddStatusErrorSpell ts)
      _                    -> undefined
    Spell.AddLight n s     -> castToNull as name (castAddLight n s)
    Spell.CheckLocation _  -> as castUnknown name
    Spell.Event eid        -> eventSpell eid

eventSpell :: GameEventID -> SpellEffect
eventSpell eid s o next = GameAuto $ do
    evDB  <- asks mazeEvents
    let e = Map.lookup eid evDB
    run $ case e of Nothing   -> next
                    Just edef -> doEvent edef (const next) (const next) (\sdef n -> spell' sdef s o n)

-- --------------------------------------------------------------------------------

castToNull :: CastAs -> String -> CastAction -> SpellEffect
castToNull as n ca src (Left l)  next = as castInBattle n ca src (Left []) next
castToNull as n ca src (Right _) next = GameAuto $ do
    es <- mapM (aliveEnemiesLine . toEnemyLine) [1..4]
    run $ as castInBattle n ca src (Right $ concat es) next

castToSingle :: CastAs -> String -> CastAction -> SpellEffect
castToSingle as n ca (Left id) (Left l) next = as castInBattle n ca (Left id) (Left [l]) next
castToSingle as n ca (Left id) (Right el) next = GameAuto $ do
    e1 <- aliveEnemyLineRandom el
    case e1 of Nothing -> run next
               Just e  -> run $ as castInBattle n ca (Left id) (Right [e]) next
castToSingle as n ca (Right e) (Left l) next = as castInBattle n ca (Right e) (Left [l]) next
castToSingle as n ca (Right se) (Right el) next = GameAuto $ do
    e1 <- aliveEnemyLineRandom el
    case e1 of Nothing -> run next
               Just e  -> run $ as castInBattle n ca (Right se) (Right [e]) next

castToGroup :: CastAs -> String -> CastAction -> SpellEffect
castToGroup as n ca src (Right el) next = GameAuto $ do
    es <- aliveEnemiesLine el
    run $ as castInBattle n ca src (Right es) next
castToGroup as n ca src (Left _) next = GameAuto $ do
    ps <- party <$> world
    run $ as castInBattle n ca src (Left $ toPartyPos <$> [1..length ps]) next

castToAll :: CastAs -> String -> CastAction -> SpellEffect
castToAll as n ca src (Left _) next = GameAuto $ do
    ps <- party <$> world
    run $ as castInBattle n ca src (Left $ toPartyPos <$> [1..length ps]) next
castToAll as n ca src (Right _) next = GameAuto $ do
    es <- mapM (aliveEnemiesLine . toEnemyLine) [1..4]
    run $ as castInBattle n ca src (Right $ concat es) next

type Verb = String

type Cast = String -- object (spell name or item name).
         -> CastAction
         -> Either CharacterID Enemy.Instance  -- src
         -> Either [PartyPos] [Enemy.Instance] -- dst
         -> GameMachine -> GameMachine

castInBattle :: Verb -> Cast
castInBattle v n ca (Left cid) dst next = GameAuto $ do
    src <- characterByID cid
    ts  <- ca (Left src) dst
    let acc (_, t, d) = let msg = (nameOf src ++ " " ++ v ++ " " ++ n ++ ".\n") ++ t
                        in if d then toEffect False msg else events [message msg] 
    run $ foldr acc (with (fst3 <$> ts) next) ((undefined, "", False) : ts)

castInBattle v n ca (Right e) dst next = GameAuto $ do
    ts <- ca (Right e) dst
    let acc (_, t, d) = let msg = (nameOf e ++ " " ++ v ++ " " ++ n ++ ".\n") ++ t
                        in if d then toEffect True msg else events [message msg] 
    run $ foldr acc (with (fst3 <$> ts) next) ((undefined, "", False) : ts)


type CastAs = (Verb -> Cast) -> Cast

asSpell cast = cast verbForSpell
asItem  cast = cast verbForItem

-- --------------------------------------------------------------------------------

castUnknown :: Verb -> String -> SpellEffect
castUnknown v = castNoEffect v "no happens."

castNoMP :: Verb -> String -> SpellEffect
castNoMP v = castNoEffect v "no more MP."

castButSilent :: Verb -> String -> SpellEffect
castButSilent v = castNoEffect v "but it wasn't voiced."

castButFear :: Verb -> String -> SpellEffect
castButFear v = castNoEffect v "but couldn't voice well by fear."

castNoEffect :: Verb -> String -> String -> SpellEffect
castNoEffect v msg n src _ next = GameAuto $ do
    name <- case src of Left id -> Chara.name <$> characterByID id
                        Right e -> Enemy.name <$> enemyDefineByID (Enemy.id e)
    let ts      = ["", msg]
        toMsg t = message $ (name ++ " " ++ v ++ " " ++ n ++ ".\n") ++ t
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


toEffect :: Bool -> String -> GameMachine -> GameMachine
toEffect fromEnemy msg next = e1
  where
    d1  = modify $ \w -> if fromEnemy then w { frameTrans = frameTrans w . translate ( 0,  1)
                                             , sceneTrans = sceneTrans w . translate ( 0,  1) }
                                      else w { enemyTrans = enemyTrans w . translate ( 0,  1) }
    d2  = modify $ \w -> if fromEnemy then w { frameTrans = frameTrans w . translate (-1,  0)
                                             , sceneTrans = sceneTrans w . translate (-1,  0) }
                                      else w { enemyTrans = enemyTrans w . translate (-1,  0) }
    d3  = modify $ \w -> if fromEnemy then w { frameTrans = frameTrans w . translate ( 2, -1)
                                             , sceneTrans = sceneTrans w . translate ( 2, -1) }
                                      else w { enemyTrans = enemyTrans w . translate ( 2, -1) }
    d4  = modify $ \w -> if fromEnemy then w { frameTrans = id 
                                             , sceneTrans = id }
                                      else w { enemyTrans = id }
    e1  = with [d1] $ select (messageTime (-40) msg Nothing) [(Clock, e2), (AnyKey, with [d4] next)]
    e2  = with [d2] $ select (messageTime (-30) msg Nothing) [(Clock, e3), (AnyKey, with [d4] next)]
    e3  = with [d3] $ select (messageTime (-40) msg Nothing) [(Clock, e4), (AnyKey, with [d4] next)]
    e4  = with [d4] $ events [message msg] next
