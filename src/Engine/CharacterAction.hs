{-# LANGUAGE TupleSections #-}
module Engine.CharacterAction
where

import Prelude hiding (lookup)
import Control.Monad (when, join, forM, forM_)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.Map hiding (filter, null, foldl, take, drop)
import Data.Function ((&), on)
import Data.List (isInfixOf, unlines, sortBy, groupBy, intercalate)
import Data.Maybe (catMaybes)

import Engine.GameAuto
import Engine.Utils
import Engine.InEvent
import Data.Primitive
import Data.World
import Data.Maze
import Data.Formula
import Data.Bifunctor (bimap)
import Data.Char (toLower)
import qualified Data.Characters as Chara
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell
import qualified Data.Items as Item


-- =================================================================================
-- character inspection.
-- ---------------------------------------------------------------------------------

inspectCharacter :: GameMachine -> Bool -> PartyPos -> GameMachine
inspectCharacter h canSpell i = GameAuto $ do
    cid <- characterIDInPartyAt i
    c   <- characterInPartyAt i
    cmdsInspect <- cmdNumPartiesWhen $ bimap (inspectCharacter h canSpell) (const True)
    let cancel = inspectCharacter h canSpell i
        iCast  = flip (ShowStatus cid) SequenceKey
        sCast  = flip (ShowStatus cid) SingleKey
        sItem  = const (sCast $ "Select item(" ++ textItemCandidate c ++ ").  ^L)eave")
        dItem  = sCast
    run $ selectWhenEsc (ShowStatus cid msg SingleKey)
                      $ (Key "l", h, True)
                      : (Key "s", inputSpell c iCast sCast (spellInCamp i cancel) cancel, canSpell)
                      : (Key "u", selectItem sItem identified (useItem sCast (useItemInCamp i cancel)) c cancel, True)
                      : (Key "d", selectDropItem dItem i c cancel, True)
                      : (Key "t", selectTradeItem dItem i cancel, True)
                      : (Key "e", equip           dItem i c cancel, True)
-- TODO               : (Key "i", identifyItem    dItem i c cancel, canIdentify)
                      : (Key "r", readSpell cancel cid, True)
                      : (Key "g", with [msgDebug $ show (Chara.spells c)] cancel, True)
                      : (Key "p", GameAuto (poolGoldTo cid >> run cancel), True)
                      : cmdsInspect
  where
    msg = if canSpell then
            "^U)se Item     ^D)rop Item    ^T)rade Item    ^E)qiup  \n" ++
            "^R)ead Spell   ^S)pell        ^P)ool Money            \n" ++
            "^#)Inspect     ^L)eave `[`E`S`C`]                         "
          else
            "^U)se Item     ^D)rop Item    ^T)rade Item   ^E)qiup       \n" ++
            "^R)ead Spell   ^P)ool Money   ^#)Inspect     ^L)eave `[`E`S`C`]"

readSpell :: GameMachine -> CharacterID -> GameMachine
readSpell cancel cid = GameAuto $ do
    c <- characterByID cid
    ss <- catMaybes <$> mapM spellByID (Chara.spells c)
    mb <- msgInReadSpell cid Nothing
    let hasMageSpell   = any (\s -> Spell.kind s == Spell.M) ss
        hasPriestSpell = any (\s -> Spell.kind s == Spell.P) ss
        msg            = "\nSelect spell type: ^M)age Spells ^P)riest Spells ^L)eave `[`E`S`C`]\n" ++ mb
    run $ selectWhenEsc (ShowStatus cid msg SingleKey)
                        [ (Key "l", cancel, True)
                        , (Key "m", selectSpellLevel (readSpell cancel cid) cid Spell.M, hasMageSpell)
                        , (Key "p", selectSpellLevel (readSpell cancel cid) cid Spell.P, hasPriestSpell)
                        ]

selectSpellLevel :: GameMachine -> CharacterID -> Spell.Kind -> GameMachine
selectSpellLevel cancel cid kind = GameAuto $ do
    mb <- msgInReadSpell cid Nothing
    let msg = "\nSelect spell level (1-7)  ^L)eave `[`E`S`C`]\n" ++ mb
        -- Generate keybindings for levels 1-7
        levelCmds = [(Key (show lv), showSpellListForInfo cancel cid kind lv) | lv <- [1..7]]
    run $ selectEsc (ShowStatus cid msg SingleKey)
                    ( (Key "l", cancel) : levelCmds )

showSpellListForInfo :: GameMachine -> CharacterID -> Spell.Kind -> Int -> GameMachine
showSpellListForInfo cancel cid kind level = GameAuto $ do
    c <- characterByID cid
    spellDB <- asks spells
    let sids = Chara.spells c
        allLearned = catMaybes $ flip lookup spellDB <$> sids
        -- Filter spells by the selected kind and level
        filteredSpells = filter (\s -> Spell.kind s == kind && Spell.lv s == level) allLearned

    if null filteredSpells
        then run $ events [message "\nNo spells learned at this level."] (selectSpellLevel cancel cid kind)
        else do
            mb <- msgInReadSpell cid (Just (kind, level))
            let indexedSpells = zip ['A'..'Z'] filteredSpells

                infoCmds = flip fmap indexedSpells $ \(key, spellDef) ->
                  let info = Spell.information spellDef
                      -- After showing info, return to this same spell list
                      nextMachine = if null info
                                      then showSpellListForInfo cancel cid kind level
                                      else events [flashMessage (-400000) info] (showSpellListForInfo cancel cid kind level)
                  in (Key [toLower key], nextMachine)

                kindName = if kind == Spell.M then "MAGE" else "PRIEST"
                msg = "\n ++ ^A~Z) Show Info  ^L)eave `[`E`S`C`]\n" ++ mb
            
            -- The 'back' action returns to the level selection screen
            let backToLevelSelect = selectSpellLevel cancel cid kind
            run $ selectEsc (ShowStatus cid msg SingleKey) ( (Key "l", backToLevelSelect) : infoCmds )

msgInReadSpell :: CharacterID -> Maybe (Spell.Kind, Int) -> GameState String
msgInReadSpell cid tgt = do
    c       <- characterByID cid
    spellDB <- asks spells
    let learnedSpells = catMaybes $ flip lookup spellDB <$> Chara.spells c
        (kind, lvl) = case tgt of Nothing     -> (Spell.M, 0)
                                  Just (k, l) -> (k, l)
    return $ if null learnedSpells then "\nNo spells learned." else let
      -- Sort by kind (Mage/Priest) then by level
      sortedSpells = sortBy (compare `on` (\s -> (Spell.kind s, Spell.lv s))) learnedSpells
      -- Group by kind
      groupedByKind = groupBy ((==) `on` Spell.kind) sortedSpells

      formatGroup group =
        let kindName = if Spell.kind (head group) == Spell.M then "MAGE SPELLS" else "PRIEST SPELLS"
            -- Group by level within the kind
            groupedByLv = groupBy ((==) `on` Spell.lv) group
            snames = fmap Spell.name
            names lvGroup = if Spell.lv (head lvGroup) == lvl && Spell.kind (head group) == kind then
                foldl (\acc (a, b) -> acc ++ " ^" ++ (a : ")" ++ b)) "" (zip ['A'..'Z'] (snames lvGroup))
              else
                intercalate "    " (snames lvGroup)
            formatLvGroup lvGroup = "  LV " ++ show (Spell.lv (head lvGroup)) ++ ": " ++ names lvGroup
        in kindName : fmap formatLvGroup groupedByLv

      in "\n" ++ unlines (intercalate [""] (fmap formatGroup groupedByKind))

-- =================================================================================
-- for item.
-- ---------------------------------------------------------------------------------

useItemInCamp :: PartyPos -> GameMachine -> Chara.ItemPos -> SpellTarget -> GameMachine
useItemInCamp src next i (Left dst) = GameAuto $ do
    cid <- characterIDInPartyAt src
    c   <- characterInPartyAt src
    def <- itemByID $ Chara.itemAt c i
    case Item.usingEffect def of
      Nothing                     -> run $ events [ShowStatus cid "no happens." SingleKey] next
      Just (Item.EqSpell ids, bp) -> do
         sdef' <- spellByID ids
         case sdef' of
           Just sdef -> if Spell.InCamp `elem` Spell.enableIn sdef then
                          run $ spellInCampNoCost sdef src dst (with [breakItem bp cid i] next)
                        else
                          run $ events [ShowStatus cid "can't use it here." SingleKey] next
           Nothing   -> error "invalid spellId in useItemInCamp"
      Just (Item.Happens eid, bp) -> do
         let next' = with [breakItem bp cid i] next
         edef' <- asks (lookup eid . mazeEvents)
         case edef' of Nothing   -> run next'
                       Just edef -> run $ doEvent edef (const next') (const next')
                                                  (\sdef n -> if Spell.InCamp `elem` Spell.enableIn sdef then
                                                                spellInCampNoCost sdef src dst (with [breakItem bp cid i] n)
                                                              else
                                                                events [ShowStatus cid "can't use it here." SingleKey] n)
useItemInCamp _ _ _ _ = error "invalid useItemInCamp"


selectItem :: (String -> Event)
           -> (ItemInf -> Bool)
           -> (Chara.Character -> Chara.ItemPos -> GameMachine -> GameMachine)
           -> Chara.Character
           -> GameMachine
           -> GameMachine
selectItem = selectItem' "l"

selectItem' :: String
            -> (String -> Event)
            -> (ItemInf -> Bool)
            -> (Chara.Character -> Chara.ItemPos -> GameMachine -> GameMachine)
            -> Chara.Character
            -> GameMachine
            -> GameMachine
selectItem' cancelKey msgForSelect isTarget next c cancel = GameAuto $ do
    is <- asks items
    let nameOf id = Item.name (is ! id)
        its = Chara.items c
        cs  = filter (isTarget . snd) (zip (Chara.numToItemPos <$> [0..]) its)
        msg = (\(t, inf) -> Chara.itemPosToText t ++ ")" ++ nameOf (itemID inf)) <$> cs
    return (msgForSelect $
              "Select item(" ++ textItemCandidate c ++ ").\n^L)eave `[`E`S`C`]\n\n"
              ++ unlines msg,
            \(Key s) -> if s == cancelKey || s == "\ESC" then cancel
                        else case Chara.itemPosByChar s of
                          Nothing -> selectItem' cancelKey msgForSelect isTarget next c cancel
                          Just i  -> if i `elem` (fst <$> cs) then next c i cancel
                                     else selectItem' cancelKey msgForSelect isTarget next c cancel
           )

useItem :: (String -> Event)
        -> (Chara.ItemPos -> SpellTarget -> GameMachine)
        -> Chara.Character
        -> Chara.ItemPos
        -> GameMachine
        -> GameMachine
useItem msgForTargeting next c i cancel = GameAuto $ do
    ps  <- party <$> world
    def <- itemByID $ Chara.itemAt c i
    case Item.usingEffect def of
      Nothing                    -> run $ next i (Left F1) -- MEMO:target should be ignored...
      Just (Item.EqSpell ids, _) -> do
         sdef' <- spellByID ids
         case sdef' of
           Just sdef -> run $ selectSpellTarget sdef undefined False (next i) msgForTargeting cancel
           Nothing   -> error "invalid spellId in useItem"


selectDropItem :: (String -> Event)
               -> PartyPos
               -> Chara.Character
               -> GameMachine
               -> GameMachine
selectDropItem msgForSelect src c =
    selectItem (const $ msgForSelect $ "Select drop item(" ++ textItemCandidate c ++ ").\n^L)eave `[`E`S`C`]") (const True) drop c
  where
    drop :: Chara.Character -> Chara.ItemPos -> GameMachine -> GameMachine
    drop c i cancel = GameAuto $ do
        let inf     = Chara.itemInfAt c i
            isEquip = inf `elem` Chara.equips c && (length . filter (==inf)) (Chara.items c) <= 1
        def <- itemByID $ Chara.itemAt c i
        run $ if Item.CantDrop `elem` Item.attributes def then
                events [msgForSelect "you cannot drop it."] (selectDropItem msgForSelect src c cancel)
              else if isEquip then
                events [msgForSelect "you are equippped with it."] (selectDropItem msgForSelect src c cancel)
              else
                with [dropItem src i] cancel




selectTradeItem :: (String -> Event)
                -> PartyPos
                -> GameMachine
                -> GameMachine
selectTradeItem msgForSelect src cancel = GameAuto $ do
    ps   <- party <$> world
    c    <- characterByID =<< characterIDInPartyAt src
    cmds <- cmdNumPartiesID (\(i, cid) -> tradeTo cid)
    run $ if null (Chara.items c) then cancel
          else selectEsc (msgForSelect $ "Select target character(^1~^" ++ show (length ps) ++ ").\n^L)eave `[`E`S`C`]")
                         ((Key "l", cancel) : cmds)
  where
    tradeTo dst = GameAuto $ do
        c'   <- characterByID =<< characterIDInPartyAt src
        cdst <- characterByID dst
        let msg = const $ msgForSelect $ "Select item to trade to " ++ Chara.name cdst ++ "(" ++ textItemCandidate c' ++ ").\n^L)eave `[`E`S`C`]"
        run $ if      null (Chara.items c')      then cancel
              else if Chara.hasMaxCountItem cdst then selectTradeItem msgForSelect src cancel
                                                 else selectItem msg (const True) (trade dst) c' cancel
    trade :: CharacterID -> Chara.Character -> Chara.ItemPos -> GameMachine -> GameMachine
    trade dst c i cancel = GameAuto $ do
        let inf = Chara.itemInfAt c i
        dropItem src i
        gainItem dst inf
        run $ tradeTo dst
        



breakItem :: (Int, Item.WhenBroken) -> CharacterID -> Chara.ItemPos -> GameState ()
breakItem (prob, to) cid i = do
    broken <- happens prob
    when broken $ do
      p <- characterByID cid
      let is = Chara.items p
          ix = Chara.itemPosToNum i
          is' = case to of Item.Lost        -> take ix is ++ drop (ix + 1) is
                           Item.ChangeTo i' -> take ix is ++ [i'] ++ drop (ix + 1) is
      updateCharacter cid (p { Chara.items = is' })

dropItem :: PartyPos -> Chara.ItemPos -> GameState ()
dropItem src pos = do
  cid <- characterIDInPartyAt src
  breakItem (100, Item.Lost) cid pos

gainItem :: CharacterID -> ItemInf -> GameState ()
gainItem cid inf = do
    c' <- characterByID cid
    let itms = Chara.items c'
    updateCharacter cid (c' { Chara.items = itms ++ [inf] })


textItemCandidate :: Chara.Character -> String
textItemCandidate c = "^A~^" ++ (Chara.itemPosToText . Chara.numToItemPos) (length (Chara.items c) - 1)


-- =================================================================================
-- for equipment.
-- ---------------------------------------------------------------------------------
equip :: (String -> Event)
      -> PartyPos
      -> Chara.Character
      -> GameMachine
      -> GameMachine
equip msgForSelect src c = equip' msgForSelect src c [(Item.isWeapon, "weapon")
                                                     ,(Item.isShield, "shield")
                                                     ,(Item.isHelmet, "helmet")
                                                     ,(Item.isArmour, "armour")
                                                     ,(Item.isGauntlet, "gauntlet")
                                                     ,(Item.isAccessory, "accessory")
                                                     ]
-- TODO:cursed item.
-- TODO:sp item.
equip' :: (String -> Event)
       -> PartyPos
       -> Chara.Character
       -> [(Item.Define -> Bool, String)]
       -> GameMachine
       -> GameMachine
equip' _ _ _ [] next = next
equip' msgForSelect src c ((isTarget, typeText):rest) next = GameAuto $ do
    let ids = itemID <$> filter identified (Chara.items c)
    items <- mapM itemByID ids
    let idset = zip ids items
        tgts  = filter (Chara.canEquip c . snd) . filter (isTarget . snd) $ idset
    run $ if null tgts then equip' msgForSelect src c rest next
          else selectItem' "n" (const $ msgForSelect $ "Select equip " ++ typeText ++ "(" ++ textItemCandidate c ++ ").\n  N)o equip. `[`E`S`C`]")
                          ((`elem` (fst <$> tgts)) . itemID) selectEq c (eq Nothing)
  where
    selectEq :: Chara.Character -> Chara.ItemPos -> GameMachine -> GameMachine
    selectEq c pos next = eq $ Just (Chara.itemInfAt c pos)

    eq :: Maybe ItemInf -> GameMachine
    eq item = GameAuto $ do
      cid <- characterIDInPartyAt src
      let es  = Chara.equips c
      items <- mapM itemByID (itemID <$> es)
      let es'  = snd <$> filter (not . isTarget. fst) (zip items es)
          es'' = case item of Nothing -> es'
                              Just i  -> i : es'
      updateCharacter cid $ c { Chara.equips = es'' }
      run $ equip' msgForSelect src c rest next


-- =================================================================================
-- for spelling.
-- ---------------------------------------------------------------------------------

inputSpell :: Chara.Character
           -> (String -> Event)
           -> (String -> Event)
           -> (Spell.Name -> SpellTarget -> GameMachine)
           -> GameMachine
           -> GameMachine
inputSpell c msgForCasting msgForSelecting next cancel = GameAuto $
    return (msgForCasting "Input spell.\n(Empty to cancel.)",
            \(Key s) -> if null s then cancel else selectCastTarget s next)
  where
    selectCastTarget :: String -> (String -> SpellTarget -> GameMachine) -> GameMachine
    selectCastTarget s next = GameAuto $ do
        ps   <- party <$> world
        def' <- spellByName s
        case def' of
          Nothing  -> run $ next s (Left F1) -- MEMO:target should be ignored...
          Just def -> run $ selectSpellTarget def c True (next s) msgForSelecting cancel

selectSpellTarget :: Spell.Define -> Chara.Character -> Bool
                  -> (SpellTarget -> GameMachine)
                  -> (String -> Event)
                  -> GameMachine
                  -> GameMachine
selectSpellTarget def c checkKnow next msgForSelecting cancel = GameAuto $ do
    know <- if checkKnow then knowSpell' c def else return True
    ps   <- party <$> world
    (toEnemy, mx) <- case Spell.target def of
      Spell.OpponentSingle -> (True,) . length <$> lastEnemies
      Spell.OpponentGroup  -> (True,) . length <$> lastEnemies
      Spell.OpponentAll    -> return (True , 1) -- MEMO:target should be ignored...
      Spell.AllySingle     -> return (False, length ps)
      _                    -> return (False, 1) -- MEMO:target should be ignored...
    select toEnemy (if know then mx else 1) next
  where
    select toEnemy mx nextWith =
        let toDst = if toEnemy then Right . toEnemyLine else Left . toPartyPos in
        if mx <= 1 then
          run (nextWith $ toDst 1)
        else
          run $ selectEsc (msgForSelecting $
                  if toEnemy then "Target group? (^1~^"     ++ show mx ++ ")\n\n^C)ancel `[`E`S`C`]"
                             else "Target character? (^1~^" ++ show mx ++ ")\n\n^C)ancel `[`E`S`C`]")
                  $ (Key "c", cancel)
                  : cmdNums mx (nextWith.toDst)


spellInCamp :: PartyPos -> GameMachine -> Spell.Name -> SpellTarget -> GameMachine
spellInCamp src next s (Left dst) = GameAuto $ do
    cid <- characterIDInPartyAt src
    spellDef <- spellByName s
    case spellDef of
        Just def -> if Spell.InCamp `elem` Spell.enableIn def then
                      run $ spellInCamp' def src dst next
                    else
                      run $ events [ShowStatus cid "can't cast it here." SingleKey] next
        Nothing  -> run $ events [ShowStatus cid "what?" SingleKey] next
spellInCamp src next s (Right dst) = GameAuto $ do
    cid <- characterIDInPartyAt src
    run $ events [ShowStatus cid "can't cast it here." SingleKey] next

spellInCamp' :: Spell.Define -> PartyPos -> PartyPos -> GameMachine -> GameMachine
spellInCamp' def src dst next = GameAuto $ do
    cid  <- characterIDInPartyAt src
    c    <- characterInPartyAt src
    know <- knowSpell' c def
    can  <- canSpell'  c def
    if      not know then
      run $ events [ShowStatus cid "you can't casting it." SingleKey] next
    else if not can  then
      run $ events [ShowStatus cid "no more MP." SingleKey] next
    else do
      join $ updateCharacter <$> characterIDInPartyAt src <*> costSpell' c def
      run $ spellInCampNoCost def src dst next

spellInCampNoCost :: Spell.Define -> PartyPos -> PartyPos -> GameMachine -> GameMachine
spellInCampNoCost def src dst next = GameAuto $ do
    pn  <- length . party <$> world
    cid <- characterIDInPartyAt src
    c   <- characterInPartyAt src
    let tgt = case Spell.target def of
                Spell.AllySingle -> [dst]
                Spell.AllyAll    -> toPartyPos <$> [1..pn]
                _                -> []
    case Spell.effect def of
      Spell.Damage _  -> undefined
      Spell.Cure f ss -> do
        efs <- castCureSpell f ss (Left c) (Left tgt)
        run $ with (fst3 <$> efs) (events [ShowStatus cid "done" SingleKey] next)
      Spell.ChangeParam ad term etxt -> do
        efs <- castParamChangeSpell ad term etxt (Left c) (Left tgt)
        run $ with (fst3 <$> efs) (events [ShowStatus cid "done" SingleKey] next)
      Spell.AddLight n s -> do
        efs <- castAddLight n s (Left c) (Left tgt)
        run $ with (fst3 <$> efs) (events [ShowStatus cid "done" SingleKey] next)
      Spell.Event eid -> do
         edef' <- asks (lookup eid . mazeEvents)
         run $ case edef' of Nothing   -> next
                             Just edef -> doEvent edef (const next) (const next)
                                                  (\sdef n -> if Spell.InCamp `elem` Spell.enableIn sdef then
                                                                spellInCampNoCost sdef src dst n
                                                              else
                                                                events [ShowStatus cid "can't use it here." SingleKey] n)
      Spell.CheckLocation t -> do
        p              <- currentPosition
        (fn, (w,h), m) <- mazeInfAt $ z p
        let msg = "you are at " ++ fn ++ "(" ++ show (x p) ++ ", " ++ show (y p) ++ ": " ++ show (direction p) ++ ")."
        run $ case t of
          Spell.OnlyCoord -> events [ ShowStatus cid msg SingleKey
                                    , ShowStatus cid "done" SingleKey] next
          Spell.ViewMap   -> showMap msg (0, 0) $ events [ShowStatus cid "done" SingleKey] next

showMap :: String -> (Int, Int) -> GameMachine -> GameMachine
showMap msg (x,y) next = selectEsc (ShowMap (msg ++ "\n ^A-^W-^S-^D  ^L)eave `[`E`S`C`]") (x,y))
                                   [(Key "l", next)
                                   ,(Key "a", showMap msg (x+1,y) next)
                                   ,(Key "s", showMap msg (x,y-1) next)
                                   ,(Key "d", showMap msg (x-1,y) next)
                                   ,(Key "w", showMap msg (x,y+1) next)
                                   ]

-- --------------------------------------------------------------------------------

type CastAction = (Either Chara.Character Enemy.Instance     -- ^ src
                -> Either [PartyPos] [Enemy.Instance]        -- ^ dst (if it is empty, target is party)
                -> GameState [(GameState (), String, Bool)]) -- ^ effect (effect, message, damage occured)

castCureSpell :: Formula -> [StatusError] -> CastAction
castCureSpell f ss (Left src) (Left is) = do
    ps <- party <$> world
    ts <- forM is $ \i -> do
      dst <- characterInPartyAt i
      id  <- characterIDInPartyAt i
      let ssc = statusErrorsOf dst
      if hpOf dst == 0 && all (`notElem` ssc) ss then return []
      else do
        m <- formulaMapSO (Left src) (Left dst)
        d <- evalWith m f
        let dst' = foldl (&) (setHp (hpOf dst + d) dst) (removeStatusError <$> ss)
        let msg = if hpOf dst /= hpOf dst' then
                    nameOf dst ++ " heal " ++ show (hpOf dst' - hpOf dst) ++ "."
                  else
                    nameOf dst ++ " cured."
        return [(updateCharacter id dst', msg, False)]
    return $ concat ts
castCureSpell f ss (Right src) (Right is) = do
    ts <- forM is $ \dst -> do
      let ssc = statusErrorsOf dst
      if hpOf dst == 0 && all (`notElem` ssc) ss then return []
      else do
        m <- formulaMapSO (Right src) (Right dst)
        d <- evalWith m f
        let dst' = foldl (&) (setHp (hpOf dst + d) dst) (removeStatusError <$> ss)
        let msg = if hpOf dst /= hpOf dst' then
                    nameOf dst ++ " heal " ++ show (hpOf dst' - hpOf dst) ++ "."
                  else
                    nameOf dst ++ " cured."
        return [(updateEnemy dst (const dst'), msg, False)]
    return $ concat ts
castCureSpell _ _ _ _ = undefined


castParamChangeSpell :: AdParam -> Term -> String -> CastAction
castParamChangeSpell ad term etxt (Left src) (Left is)
    | null is = do
          prmc <- toParamChange (Left src) (Left src) ad
          return [(modify $ \w -> w { partyParamDelta = Spell.applyChangeParam term prmc (partyParamDelta w) }
                  , "party " ++ etxt ++ ".", False)]
    | otherwise = concat <$> forM is (\i -> do
          dst  <- characterIDInPartyAt i
          cdst <- characterInPartyAt i
          prmc <- toParamChange (Left src) (Left cdst) ad
          if hpOf cdst == 0 then return []
          else return [(updateCharacter dst $ cdst { Chara.paramDelta = Spell.applyChangeParam term prmc (Chara.paramDelta cdst) }
                      , nameOf cdst ++ " " ++ etxt ++ ".", False)]
          )
castParamChangeSpell ad term etxt (Right src) (Right is) = concat <$> forM is (\dst -> do
    prmc <- toParamChange (Right src) (Right dst) ad
    let org = Enemy.modParam dst
        p'  = if effectName prmc `isInfixOf` effectName org then org
              else ParamChange { deltaParam = deltaParam org <> deltaParam prmc
                               , deltaAC    = deltaAC org + deltaAC prmc
                               , effectName = effectName org ++ effectName prmc ++ "\n" }
    if hpOf dst == 0 then return []
    else return [(updateEnemy dst $ const dst { Enemy.modParam = p' }
                , nameOf dst ++ " " ++ etxt ++ ".", False)]
    )

castDamageSpell :: Formula -> [EffectLabel] -> CastAction
castDamageSpell f attrs (Left c) (Right es) = do
    ts <- forM es $ \e -> do
      m <- formulaMapSO (Left c) (Right e)
      if Enemy.hp e <= 0 then return []
      else do
        vs <- vsEffectLabelsOf (Right e)
        d  <- evalWith m f
        d' <- applyVsEffect attrs vs (Left c) (Right e) d
        let e' = damageHp d' e
            noDamage = d /= 0 && d' == 0
        let msg = if noDamage then nameOf e ++ " resisted."
                              else nameOf e ++ " takes " ++ show d' ++ "."
        return $ (updateEnemy e (const e'), msg, not noDamage)
               : [(return (), msg ++ "\n" ++ nameOf e ++ " is killed.", False) | Enemy.hp e' <= 0]
    return $ concat ts
castDamageSpell f attrs s@(Right e) (Left is) = do
    ts <- forM is $ \i -> do
      c <- characterInPartyAt i
      m <- formulaMapSO (Right e) (Left c)
      if hpOf c == 0 then return []
      else do
        vs <- vsEffectLabelsOf (Left c)
        d  <- evalWith m f
        d' <- applyVsEffect attrs vs s (Left c)  d
        let c' = damageHp d' c
            noDamage = d /= 0 && d' == 0
        let msg = if noDamage then nameOf c ++ " resisted."
                              else nameOf c ++ " takes " ++ show d' ++ "."
        cid <- characterIDInPartyAt i
        return $ (updateCharacter cid c', msg, not noDamage)
               : [(return (), msg ++ "\n" ++ nameOf c ++ " is killed.", False) | hpOf c' <= 0]
    return $ concat ts
castDamageSpell f attrs src dst = error $ "castDamageSpell:" ++ show f ++ ", src=" ++ show src ++ ", dst=" ++ show dst


castAddLight :: Int -> Bool -> CastAction
castAddLight n s _ _ = return [(setLightValue s n, "it is brightly lit.", False)]


