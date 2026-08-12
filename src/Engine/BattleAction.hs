{-# LANGUAGE TupleSections #-}
module Engine.BattleAction
where

import PreludeL hiding (lookup)
import qualified Data.Map as Map
import Data.List hiding (lookup, (!!))
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
    , checkLocation
    , breakItem
    )
import Engine.InEvent (doEvent)
import Data.World
import Data.Formula
import Data.Primitive
import qualified Data.Enemies as Enemy
import qualified Data.Characters as Chara
import qualified Data.Spells as Spell
import qualified Data.Items as Item
import qualified Data.Maze as Maze

import Control.CUI (translate)

type ActionOfCharacter = CharacterID  -- ^ id of actor.
                      -> EnemyLine    -- ^ number that means target.
                      -> GameMachine  -- ^ next game auto.
                      -> GameMachine  -- ^ game auto.

messageBlink :: GameState ([String] -> [Event])
messageBlink = do
    msgF <- messageF
    return $ \ms -> messageBlink' msgF ms
  where
    messageBlink' :: (String -> Event) -> [String] -> [Event]
    messageBlink' msgF [] = []
    messageBlink' msgF (m:ms) = messageTime 2 (unlines $ head ls : ("" <$ tail ls)) Nothing : (msgF <$> m : ms)
      where ls = lines m

messageF :: GameState (String -> Event)
messageF = do
    tw <- waitTimeInBattle . worldOption <$> world
    return $ if tw == 0 then message else flip (messageTime (-tw)) Nothing

fightOfCharacter :: ActionOfCharacter
fightOfCharacter id el next = GameAuto $ do
    msgBlink <- messageBlink
    msgF     <- messageF
    es <- aliveEnemiesLine el
    ea <- filter (\e -> Enemy.hp e > 0) . join <$> lastEnemies
    c     <- characterByID id
    wattr <- weaponAttrOf c
    let range = Item.targetRange wattr
    if null ea || (range /= Item.ToAll && null es) then run next
    else do
      let es' | range == Item.ToSingle = [head es]
              | range == Item.ToGroup  = es
              | range == Item.ToAll    = ea
          toM = if length es' <= 1 then fmap msgF else msgBlink
      nexts <- forM es' $ \e -> do
        (h, d, ses) <- fightDamage el c e 0
        let e' = damageHp d e
        ms <- fightMessage c e' (h, d, ses)
        let update = updateEnemy e (const e') >> when (Enemy.hp e' <= 0) (addMarks id)
        return $ if d == 0 || el /= L1 then with [update] . events (toM ms)
                                       else with [update] . toEffect False (head ms) . events (msgF <$> tail ms)
      run $ foldr ($) next nexts

fightDamage :: EnemyLine
            -> Chara.Character
            -> Enemy.Instance
            -> Int
            -> GameState (Int, Int, [StatusError]) -- hit count, damage, status errors.
fightDamage el c e hitBonus = do
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
        hitSkill = jobBonus + strBonus + stBonus + hitBonus
        atSkill  = max (min (acE + hitSkill - 3 * enemyLineToNum el) 19) 1
        damageF  = Item.damage wattr
    rs <- replicateM tryCount $ do
        hit <- (<=) <$> randomNext 1 20 <*> pure atSkill
        dam <- (+) <$> evalWith m damageF <*> pure (max 0 strBonus)
        let dam'  | e `hasStatusError` Sleep || e `hasStatusError` Paralysis     = dam * 2
                  | otherwise                                                    = dam
        let dam'' | any (`elem` Item.doubleLabels wattr) (Enemy.attrLabels edef) = dam' * 2
                  | otherwise                                                    = dam'
        dam''' <- applyVsEffect (Item.attrLabels wattr) vs (Left c) (Right e) dam''
        return $ if hit then (1, dam''') else (0, 0)
    let dh = foldl' (\(h1, d1) (h2, d2) -> (h1 + h2, d1 + d2)) (0, 0) rs
    ses <- if snd dh == 0 then return []
           else flip filterM (Item.addStatusErrors wattr) $ \(prob, se, attrs) -> do
             m  <- formulaMapSO (Left c) (Right e)
             p  <- evalWith m prob
             p' <- applyVsEffect attrs vs (Left c) (Right e) p
             resist <- resistStatusError m se (Enemy.resistError edef)
             (&&) <$> happens p' <*> pure (not resist)
    return (fst dh, snd dh, snd3 <$> ses)

fightMessage :: Chara.Character -> Enemy.Instance -> (Int, Int, [StatusError]) -> GameState [String]
fightMessage c e (h, d, ses) = do
    wa  <- weaponAttrOf c
    vs' <- switchL (Item.atackMessages wa)
    vs  <- switchL (EnJp vsENG vsJPN)
    v   <- randomIn $ if null vs' then vs else vs'
    en  <- switchL $ nameOf e
    m1 <- switchL (EnJp (Chara.name c ++ " " ++ v ++ "\n " ++ en ++ "\n")
                        (Chara.name c ++ " は " ++ en ++ " に\n" ++ v ++ "。\n"))
    m2 <- switchL (EnJp (if h == 0 then " and misses.\n" else " and hits " ++ show h ++ " times for " ++ show d ++ ".\n")
                        (if h == 0 then "しかし外れた。" else show h ++ " 回当たって、" ++ show d ++ " のダメージを与えた。\n"))
    m3 <- if Enemy.hp e <= 0 then switchL (EnJp [en ++ " is killed."] [en ++ " は死んだ。"])
                             else mapM (fmap (en ++) . statusErrorMessage) (sort ses)
    return $ (m1 ++ m2) : [m1 ++ x | x <- m3]
  where
    vsENG = ["leaps at", "tries to ram", "tries to bash", "charges at"]
    vsJPN = ["飛びかかった", "体当たりした", "殴りつけた", "突進した"]

weaponAttrOf :: Chara.Character -> GameState Item.WeaponAttr
weaponAttrOf c = do
    wep <- equipOf c Item.isWeapon
    case wep of
      Nothing  -> return $ Chara.baseWeaponAttr (Chara.job c)
      Just def -> do
        case Item.equipType def of Just (Item.Weapon _ w) -> return w
                                   _                      -> err $ "invalid weaponAttrOf for " ++ show c ++ "."

dispellOfCharacter :: Formula -> ActionOfCharacter
dispellOfCharacter f id el next = GameAuto $ do
    msgF <- messageF
    es   <- aliveEnemiesLine el
    c    <- characterByID id
    if null es then run next
    else do
      n <- length . filter (==True) <$> forM es (\e -> do
        let isUndead = EnemyLabel "undead" `elem` (Enemy.attrLabels . Enemy.define $ e)
        s <- happens =<< flip evalWith f =<< formulaMapSO (Left c) (Right e)
        if not isUndead || not s then return False
                                 else updateEnemy e (const e { Enemy.hp = 0 }) >> return True)
      en <- switchL $ nameOf (head es)
      bm <- switchL (EnJp (Chara.name c ++ " attempted to dispel " ++ en ++ ".\n")
                          (Chara.name c ++ " は " ++ en ++ " を解呪しようとした。\n"))
      rm <- switchL (EnJp (if n == 0 then "but failed." else show n ++ " " ++ en ++ " have been purified.")
                          (if n == 0 then "しかし失敗した。" else show n ++ " 体の " ++ en ++ " を解呪した。"))
      let ts = [bm, bm ++ rm]
      run $ events (msgF <$> ts) next

hideOfCharacter :: CharacterID -> GameMachine -> GameMachine
hideOfCharacter cid next = GameAuto $ do
    msgF  <- messageF
    c     <- characterByID cid
    param <- paramOf (Left c)
    couldHide <- happens $ 50 + agility param
    txt1 <- switchL (EnJp " has hidden in the shadows."   " は影に潜んだ。")
    txt2 <- switchL (EnJp " tried to hide, but couldn't." " は隠れようとしたが、失敗した。")
    if couldHide then do
        updateCharacter cid (addStatusError Hidden c)
        run $ events [msgF $ Chara.name c ++ txt1] next
    else
        run $ events [msgF $ Chara.name c ++ txt2] next

ambushOfCharacter :: ActionOfCharacter
ambushOfCharacter id el next = GameAuto $ do
    msgBlink <- messageBlink
    msgF     <- messageF
    es <- aliveEnemiesLine el
    ea <- filter (\e -> Enemy.hp e > 0) . join <$> lastEnemies
    c     <- characterByID id
    wattr <- weaponAttrOf c
    let range = Item.targetRange wattr
    if null ea || (range /= Item.ToAll && null es) then run next else do
      let es' | range == Item.ToSingle = [head es]
              | range == Item.ToGroup  = es
              | range == Item.ToAll    = ea
          toM = if length es' <= 1 then fmap msgF else msgBlink
      nexts <- forM es' $ \e -> do
        (h, d, ses) <- fightDamage el c e 2
        let e' = damageHp d e
        ms <- ambushMessage c e' (h, d, ses)
        let update = updateEnemy e (const e') >> when (Enemy.hp e' <= 0) (addMarks id)
        return $ if d == 0 || el /= L1 then with [update] . events (toM ms)
                                       else with [update] . toEffect False (head ms) . events (msgF <$> tail ms)
      run $ foldr ($) next nexts

ambushMessage :: Chara.Character -> Enemy.Instance -> (Int, Int, [StatusError]) -> GameState [String]
ambushMessage c e (h, d, ses) = do
    v  <- randomIn =<< switchL (EnJp vsENG vsJPN)
    en <- switchL $ nameOf e
    m1 <- switchL (EnJp (Chara.name c ++ " " ++ v ++ "\n " ++ en ++ "\n")
                        (Chara.name c ++ " は " ++ en ++ " に\n" ++ v ++ "。\n"))
    m2 <- switchL (EnJp (if h == 0 then " and misses." else " and hits " ++ show h ++ " times for " ++ show d ++ ".\n")
                        (if h == 0 then "しかし外れた。" else show h ++ " 回当たって、" ++ show d ++ " のダメージを与えた。\n"))
    m3 <- if Enemy.hp e <= 0 then switchL (EnJp [en ++ " is killed."] [en ++ " は死んだ。"])
                             else mapM (fmap (en ++) . statusErrorMessage) (sort ses)
    return $ (m1 ++ m2) : [m1 ++ x | x <- m3]
  where
    vsENG = ["tries to ambush"]
    vsJPN = ["飛びかかった", "体当たりした", "殴りつけた", "突進した"]

-- ================================================================================

fightOfEnemy :: Enemy.Instance                          -- ^ attacker enemy.
             -> Int                                     -- ^ count of attack.
             -> Formula                                 -- ^ damage per hit.
             -> Formula                                 -- ^ target number. 1~3 are front member, 4~6 are back member.
             -> [(Formula, StatusError, [EffectLabel])] -- ^ additinal effect, and it's probablity.
             -> GameMachine                             -- ^ next game auto.
             -> GameMachine                             -- ^ game auto.
fightOfEnemy e n dmg tgt sts next = GameAuto $ do
    msgF <- messageF
    ps   <- party <$> world
    vcids <- filterM (\cid -> do
        c <- characterByID cid
        return $ hpOf c > 0 && not (c `hasStatusError` Hidden)
      ) ps

    if null vcids then run next
    else do
      tind <- eval tgt
      let idx  = tind `mod` length ps - 1
          cid' = ps !! (if idx >= 0 then idx else length ps - 1)
      cid <- if cid' `elem` vcids then return cid' else randomIn (take tind vcids)
      c   <- characterByID cid
      if hpOf c == 0 then run next
      else do
        (h, d, ses) <- fightDamageE n e c dmg sts
        let c' = foldl (&) (damageHp d c) (addStatusError <$> ses)
        ms <- fightMessageE e c' (h, d, ses)
        let next' = with [updateCharacter cid c'] next
        run $ if d == 0 then events (msgF <$> ms) next'
                        else toEffect True (head ms) (events (msgF <$> tail ms) next')

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
    let a  = 19 - acC - lvOf e
        b  = a - acE
        hv |  19 <= b  = 19
           |   0 <= b  = b
           | -36 <= b  = 0
           |   a < 0   = 0
           | otherwise = 19
    rs <- replicateM n $ do
        hit <- (<=) <$> randomNext 1 20 <*> pure (19 - hv)
        dam <- evalWith m dmg
        let dam' = if c `hasStatusError` Sleep || c `hasStatusError` Paralysis then dam * 2 else dam
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
    return (fst dh, snd dh, snd3 <$> ses)

fightMessageE :: Enemy.Instance -> Chara.Character -> (Int, Int, [StatusError]) -> GameState [String]
fightMessageE e c (h, d, ses) = do
    let cn = Chara.name c
    v  <- randomIn =<< switchL (EnJp vsENG vsJPN)
    en <- switchL $ nameOf e
    m1 <- switchL (EnJp (en ++ " " ++ v ++ "\n " ++ cn ++ "\n")
                        (en ++ " は " ++ cn ++ " に\n" ++ v ++ "。\n"))
    m2 <- switchL (EnJp (if h == 0 then " and misses.\n" else " and hits " ++ show h ++ " times for " ++ show d ++ ".\n")
                        (if h == 0 then "しかし外れた。" else show h ++ " 回当たって、" ++ show d ++ " のダメージを与えた。\n"))
    m3 <- if hpOf c <= 0 then switchL (EnJp [cn  ++ " is killed."] [cn ++ " は死んだ。"])
                         else mapM (fmap (cn ++) . statusErrorMessage) (sort ses)
    return $ (m1 ++ m2) : [m1 ++ x | x <- m3]
  where
    vsENG = ["charges at", "claws at"]
    vsJPN = ["引っかいた", "突進した"]


-- ================================================================================

verbForItem  :: Verb
verbForItem  = EnJp "uses" "を使った"

useItemInBattle :: GameMachine -> Chara.ItemPos -> SpellEffect
useItemInBattle escape i (Left cid) dst next = GameAuto $ do
    c   <- characterByID cid
    def <- itemByID $ Chara.itemAt c i
    let n = Item.name def
    case Item.usingEffect def of
      Nothing                     -> run $ asItem castUnknown n (Left cid) dst next
      Just (Item.EqSpell ids, bp) -> do
         sdef' <- spellByID ids
         case sdef' of
           Just sdef -> run $ use escape n sdef (Left cid) dst (with [breakItem bp cid i] next)
           Nothing   -> error "invalid spellId in useItemInBattle"
      Just (Item.Happens eid, bp) -> do
         let next' = with [breakItem bp cid i] next
         edef' <- asks (lookup eid . mazeEvents)
         case edef' of Nothing   -> run next'
                       Just edef -> run $ doEvent (Just cid) edef (const next') (const next')
                                          (\sdef n -> spell' escape sdef (Left cid) dst n)

useItemInBattle escape i (Right ei) dst next = undefined -- TODO!:considering possible using item by ememy, first argument must change to item id.

use :: GameMachine -> String -> Spell.Define -> SpellEffect
use escape name def = if Spell.InBattle `elem` Spell.enableIn def
                      then cast escape verbForItem name def 
                      else asItem castUnknown name 

-- ================================================================================

verbForSpell :: Verb
verbForSpell = EnJp "casts" "を唱えた"


type SpellEffect  = Either CharacterID Enemy.Instance
                 -> SpellTarget -- ^ target line or character no.
                 -> GameMachine
                 -> GameMachine

spell :: GameMachine -> Spell.Name -> SpellEffect
spell escape s src dst next = GameAuto $ do
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
                isFear    = c `hasStatusError` Fear 0
            run $ if      not know  then asSpell castUnknown s src dst next
                  else if not can   then asSpell castNoMP    s src dst next
                  else if isSilence then asSpell castButSilent s src dst next
                  else if isFear    then asSpell castButFear   s src dst next
                  else                   with [updateCharacter idc =<< costSpell' c def] (spell' escape def src dst next)
          Right e -> do
            let isSilence = e `hasStatusError` Silence
                isFear    = e `hasStatusError` Fear 0
            run $ if      isSilence then asSpell castButSilent s src dst next
                  else if isFear    then asSpell castButFear   s src dst next
                  else                   spell' escape def src dst next
        else
          run $ asSpell castUnknown s src dst next

spell' :: GameMachine -> Spell.Define -> SpellEffect
spell' escape def = cast escape verbForSpell (Spell.name def) def

cast :: GameMachine -> Verb -> String -> Spell.Define -> SpellEffect
cast escape v name def = let as cast = cast v in case Spell.effect def of
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
    Spell.CheckLocation t  -> as (castCheckLocation t) name
    Spell.MoveLocation  _  -> as (castMalor escape) name
    Spell.Event eid        -> eventSpell escape eid
    Spell.IdentifyTrap _   -> castUnknown v name

eventSpell :: GameMachine -> GameEventID -> SpellEffect
eventSpell escape eid s o next = addEff (withSE Spelled) $ GameAuto $ do
    evDB  <- asks mazeEvents
    let e = Map.lookup eid evDB
        cid = case s of Left cid' -> Just cid'
                        Right _   -> Nothing
    run $ case e of Nothing   -> next
                    Just edef -> doEvent cid edef (const next) (const next) (\sdef n -> spell' escape sdef s o n)

-- --------------------------------------------------------------------------------

castToNull :: CastAs -> String -> CastAction -> SpellEffect
castToNull as n ca src (Left l)  next = addEff (withSE Spelled) $ as castInBattle n ca src (Left []) next
castToNull as n ca src (Right _) next = addEff (withSE Spelled) $ GameAuto $ do
    es <- mapM (aliveEnemiesLine . toEnemyLine) [1..4]
    run $ as castInBattle n ca src (Right $ concat es) next

castToSingle :: CastAs -> String -> CastAction -> SpellEffect
castToSingle as n ca (Left id) (Left l) next = addEff (withSE Spelled) $ as castInBattle n ca (Left id) (Left [l]) next
castToSingle as n ca (Left id) (Right el) next = addEff (withSE Spelled) $ GameAuto $ do
    e1 <- aliveEnemyLineRandom el
    case e1 of Nothing -> run next
               Just e  -> run $ as castInBattle n ca (Left id) (Right [e]) next
castToSingle as n ca (Right e) (Left l) next = addEff (withSE Spelled) $ as castInBattle n ca (Right e) (Left [l]) next
castToSingle as n ca (Right se) (Right el) next = addEff (withSE Spelled) $ GameAuto $ do
    e1 <- aliveEnemyLineRandom el
    case e1 of Nothing -> run next
               Just e  -> run $ as castInBattle n ca (Right se) (Right [e]) next

castToGroup :: CastAs -> String -> CastAction -> SpellEffect
castToGroup as n ca src (Right el) next = addEff (withSE Spelled) $ GameAuto $ do
    es <- aliveEnemiesLine el
    run $ as castInBattle n ca src (Right es) next
castToGroup as n ca src (Left _) next = addEff (withSE Spelled) $ GameAuto $ do
    ps <- party <$> world
    run $ as castInBattle n ca src (Left $ toPartyPos <$> [1..length ps]) next

castToAll :: CastAs -> String -> CastAction -> SpellEffect
castToAll as n ca src (Left _) next = addEff (withSE Spelled) $ GameAuto $ do
    ps <- party <$> world
    run $ as castInBattle n ca src (Left $ toPartyPos <$> [1..length ps]) next
castToAll as n ca src (Right _) next = addEff (withSE Spelled) $ GameAuto $ do
    es <- mapM (aliveEnemiesLine . toEnemyLine) [1..4]
    run $ as castInBattle n ca src (Right $ concat es) next

type Verb = LanguageSet String

type Cast = String -- object (spell name or item name).
         -> CastAction
         -> Either CharacterID Enemy.Instance  -- src
         -> Either [PartyPos] [Enemy.Instance] -- dst
         -> GameMachine -> GameMachine

castInBattle :: Verb -> Cast
castInBattle vs n ca (Left cid) dst next = GameAuto $ do
    msgF <- messageF
    src <- characterByID cid
    ts  <- ca (Left src) dst
    n   <- switchL $ nameOf src
    v   <- switchL vs
    let acc (_, t, d, k) = let msg = (n ++ " " ++ v ++ " " ++ n ++ ".\n") ++ t
                           in with [when k (addMarks cid)] . (if d then toEffect False msg else events [msgF msg])
    run $ foldr acc (with (fst4 <$> ts) next) ((undefined, "", False, False) : ts)

castInBattle vs n ca (Right e) dst next = GameAuto $ do
    msgF <- messageF
    ts <- ca (Right e) dst
    v  <- switchL vs
    n  <- switchL $ nameOf e
    let acc (_, t, d, _) = let msg = (n ++ " " ++ v ++ " " ++ n ++ ".\n") ++ t
                           in if d then toEffect True msg else events [msgF msg] 
    run $ foldr acc (with (fst4 <$> ts) next) ((undefined, "", False, False) : ts)


type CastAs = (Verb -> Cast) -> Cast

asSpell cast = cast verbForSpell
asItem  cast = cast verbForItem

-- --------------------------------------------------------------------------------

castUnknown :: Verb -> String -> SpellEffect
castUnknown vs = castNoEffect vs ("nothing happened.", "何も起こらなかった。")

castNoMP :: Verb -> String -> SpellEffect
castNoMP vs = castNoEffect vs ("not enough MP.", "MPが足りなかった。")

castButSilent :: Verb -> String -> SpellEffect
castButSilent vs = castNoEffect vs ("but is silenced.", "しかし声にならなかった。")

castButFear :: Verb -> String -> SpellEffect
castButFear vs = castNoEffect vs ("but was too frightened to cast.", "しかし恐怖でうまく声を出せなかった。")

castNoEffect :: Verb -> (String, String) -> String -> SpellEffect
castNoEffect vs (msgENG, msgJPN) n src _ next = GameAuto $ do
    msgF <- messageF
    msg  <- switchL (EnJp msgENG msgJPN)
    v    <- switchL vs
    name <- case src of Left id -> Chara.name <$> characterByID id
                        Right e -> switchL . Enemy.name =<< enemyDefineByID (Enemy.id e)
    bm   <- switchL (EnJp (name ++ " " ++ v ++ " " ++ n ++ ".\n")
                          (name ++ " は " ++ n ++ " に " ++ v ++ "。\n"))
    let ts      = ["", msg]
        toMsg t = msgF $ bm ++ t
    run $ events (toMsg <$> ts) next

castCheckLocation :: Spell.CheckLocationType -> Verb -> String -> SpellEffect
castCheckLocation t vs n src _ next = GameAuto $ do
    msgF <- messageF
    v    <- switchL vs
    name <- case src of Left id -> Chara.name <$> characterByID id
                        Right e -> switchL . Enemy.name =<< enemyDefineByID (Enemy.id e)
    run $ events [msgF $ name ++ " " ++ v ++ " " ++ n ++ ".\n"] (checkLocation t next)

castMalor :: GameMachine -> Verb -> String -> SpellEffect
castMalor escape vs n src _ next = GameAuto $ do
    msgF <- messageF
    txt1 <- switchL (EnJp " has disappeared." " は消え失せた。")
    v    <- switchL vs
    case src of
        Right e -> do
            name <- switchL . Enemy.name =<< enemyDefineByID (Enemy.id e)
            bm   <- switchL (EnJp (name ++ " " ++ v ++ " " ++ n ++ ".\n")
                                  (name ++ " は " ++ n ++ " に " ++ v ++ "。\n"))
            let ts      = ["", name ++ txt1]
                toMsg t = msgF $ bm ++ t
            updateEnemy e $ const e { Enemy.hp = 0 }
            run $ events (toMsg <$> ts) next
        Left id -> do
            name    <- Chara.name <$> characterByID id
            p       <- currentPosition
            size    <- mazeSizeAt $ Maze.z p
            (x',y') <- case size of Just ((x0, y0), (w, h)) -> (,) <$> randomIn [x0..(x0+w-1)] <*> randomIn [y0..(y0+h-1)]
                                    Nothing                 -> (,) <$> randomIn [(Maze.x p-100)..(Maze.x p+100)] <*> randomIn [(Maze.y p-100)..(Maze.y p+100)]
            bm   <- switchL (EnJp (name ++ " " ++ v ++ " " ++ n ++ ".\n")
                                  (name ++ " は " ++ n ++ " に " ++ v ++ "。\n"))
            run $ events [msgF bm] $ with [movePlace $ InBattle (p { Maze.x = x', Maze.y = y' }) []] escape

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

toEffect :: Bool -> String -> GameMachine -> GameMachine
toEffect fromEnemy msg next = GameAuto $ do
    msgBlink <- messageBlink
    msgF     <- messageF
    let d1  = modify $ \w -> if fromEnemy then w { frameTrans = frameTrans w . translate ( 0,  1)
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
        e0  =             select (withSE se $ head $ msgBlink [msg]) [(Clock, e1), (AnyKey, with [d4] next)]
        e1  = with [d1] $ select (messageTime (-40) msg Nothing)     [(Clock, e2), (AnyKey, with [d4] next)]
        e2  = with [d2] $ select (messageTime (-30) msg Nothing)     [(Clock, e3), (AnyKey, with [d4] next)]
        e3  = with [d3] $ select (messageTime (-40) msg Nothing)     [(Clock, e4), (AnyKey, with [d4] next)]
        e4  = with [d4] $ events [msgF msg] next
        se  = if fromEnemy then FightHitToP else FightHitToE
    run e0
