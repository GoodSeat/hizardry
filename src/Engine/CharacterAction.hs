{-# LANGUAGE TupleSections #-}
module Engine.CharacterAction
where

import Prelude hiding (lookup)
import Control.Monad (when, join, forM, forM_)
import Control.Monad.Reader (asks)
import Control.Monad.State (gets, modify)
import Data.Map hiding (filter, null, foldl, take, drop)
import Data.Function ((&), on)
import Data.List (isInfixOf, unlines, sortBy, groupBy, intercalate, find)
import Data.Maybe (catMaybes, isJust)

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
    let job = Chara.job c
        canIdentify = isJust (Chara.identifyItemChance (Chara.job c)) && not (c `hasStatusError` Fear 0) && hpOf c > 0
        canSpell' = canSpell && hpOf c > 0 && not (any (c `hasStatusError`) cantSpellStatus)
        cancel = inspectCharacter h canSpell i
        iCast  = askInStatus cid
        sCast  = showStatus cid
        sItem  = const (sCast $ "Select item(" ++ textItemCandidate c ++ ").  ^L)eave")
        dItem  = sCast
        eItem  = showStatusEquip cid
        msg   
         | canSpell' && not canIdentify =
                   "^U)se Item     ^D)rop Item    ^T)rade Item    ^E)qiup  \n" ++
                   "^R)ead Spell   ^S)pell        ^P)ool Money             \n" ++
                   "^#)Inspect     ^L)eave `[`E`S`C`]                         "
         | not canSpell' && not canIdentify =
                   "^U)se Item     ^D)rop Item    ^T)rade Item   ^E)qiup   \n" ++
                   "^R)ead Spell   ^P)ool Money   ^#)Inspect     ^L)eave `[`E`S`C`]"
         | canSpell' && canIdentify =
                   "^U)se Item     ^D)rop Item    ^T)rade Item   ^E)qiup   \n" ++
                   "^R)ead Spell   ^S)pell        ^P)ool Money   ^I)dentify\n" ++
                   "^#)Inspect     ^L)eave `[`E`S`C`]                         "
         | otherwise =
                   "^U)se Item     ^D)rop Item    ^T)rade Item   ^E)qiup   \n" ++
                   "^R)ead Spell   ^P)ool Money   ^I)dentify               \n" ++
                   "^#)Inspect     ^L)eave `[`E`S`C`]"
    run $ selectWhenEsc (showStatus cid msg)
                      $ (Key "l", h, True)
                      : (Key "s", inputSpell c iCast sCast (spellInCamp i cancel) cancel, canSpell')
                      : (Key "u", selectItem sItem identified (useItem sCast (useItemInCamp i cancel)) c cancel, True)
                      : (Key "d", selectDropItem dItem i c cancel, True)
                      : (Key "t", selectTradeItem eItem i cancel, True)
                      : (Key "e", equip           eItem i c cancel, True)
                      : (Key "i", identifyItem    dItem i c cancel, canIdentify)
                      : (Key "r", readSpell cancel cid, True)
                      : (Key "g", with [msgDebug $ show (Chara.spells c)] cancel, True)
                      : (Key "p", GameAuto (poolGoldTo cid >> run cancel), True)
                      : cmdsInspect

identifyItem :: (String -> Event)
             -> PartyPos
             -> Chara.Character
             -> GameMachine
             -> GameMachine
identifyItem msgForSelect src c cancel = GameAuto $ do
    cid <- characterIDInPartyAt src
    let unidentifiedItems = filter (not . identified) (Chara.items c)
    run $ if null unidentifiedItems then
            events [msgForSelect "No unidentified items."] cancel
          else
            selectItem (const $ msgForSelect $ "Select target item(" ++ textItemCandidate c ++ ").\n^L)eave `[`E`S`C`]")
                       (not . identified) (doIdentifyItem cid cancel) c cancel

doIdentifyItem :: CharacterID
               -> GameMachine
               -> Chara.Character
               -> Chara.ItemPos
               -> GameMachine
               -> GameMachine
doIdentifyItem cid cancel _ i _ = GameAuto $ do
    c   <- characterByID cid
    job <- Chara.job <$> characterByID cid
    let inf = Chara.itemInfAt c i
    itemDef <- itemByID (itemID inf)
    let charLv = Chara.lv c
    let ilv = Item.itemLv itemDef
    case Chara.identifyItemChance job of
        Nothing -> run $ events [showStatus cid "You cannot identify items."] cancel
        Just formula -> do
            m <- formulaMapSO (Left c) (Left c) -- self-targeting for formula evaluation
            successChance <- evalWith (insert "itemLv" ilv m) formula
            roll <- happens successChance
            if roll then do
                let newInf = inf { identified = True }
                updateCharacter cid (c { Chara.items = replaceItemAt (Chara.items c) i newInf })
                run $ events [showStatus cid "Identified."] cancel
            else do
                beFear <- happens 20
                if not beFear then
                    run $ events [showStatus cid "Identification failed."] cancel
                else do
                    updateCharacterWith cid $ addStatusError (Fear 30)
                    run $ events [showStatus cid "Ooops! you touch item!"] cancel


replaceItemAt :: [ItemInf] -> Chara.ItemPos -> ItemInf -> [ItemInf]
replaceItemAt items pos newInf =
    let idx = fromEnum pos
    in take idx items ++ [newInf] ++ drop (idx + 1) items


readSpell :: GameMachine -> CharacterID -> GameMachine
readSpell cancel cid = GameAuto $ do
    c  <- characterByID cid
    ss <- catMaybes <$> mapM spellByID (Chara.spells c)
    ac <- msgInReadSpell cid Nothing
    let hasMageSpell   = any (\s -> Spell.kind s == Spell.M) ss
        hasPriestSpell = any (\s -> Spell.kind s == Spell.P) ss
        msg            = "Select spell type:\n  ^M)age Spells  ^P)riest Spells  ^L)eave `[`E`S`C`]"
    run $ selectWhenEsc (showStatusAlt cid msg ac)
                        [ (Key "l", cancel, True)
                        , (Key "m", selectSpellLevel (readSpell cancel cid) cid Spell.M, hasMageSpell)
                        , (Key "p", selectSpellLevel (readSpell cancel cid) cid Spell.P, hasPriestSpell)
                        ]

selectSpellLevel :: GameMachine -> CharacterID -> Spell.Kind -> GameMachine
selectSpellLevel cancel cid kind = GameAuto $ do
    ac <- msgInReadSpell cid Nothing
    let msg = "Select spell level:\n  ^1~^7)Select Lv  ^L)eave `[`E`S`C`]"
        -- Generate keybindings for levels 1-7
        levelCmds = [(Key (show lv), showSpellListForInfo (selectSpellLevel cancel cid kind) cid kind lv) | lv <- [1..7]]
    run $ selectEsc (showStatusAlt cid msg ac) ((Key "l", cancel) : levelCmds)

showSpellListForInfo :: GameMachine -> CharacterID -> Spell.Kind -> Int -> GameMachine
showSpellListForInfo cancel cid kind level = GameAuto $ do
    c <- characterByID cid
    spellDB <- asks spells
    let sids = Chara.spells c
        allLearned = catMaybes $ flip lookup spellDB <$> sids
        -- Filter spells by the selected kind and level
        filteredSpells = filter (\s -> Spell.kind s == kind && Spell.lv s == level) allLearned

    if null filteredSpells then run cancel else do
        ac <- msgInReadSpell cid (Just (kind, level))
        let indexedSpells = zip ['A'..'Z'] filteredSpells
            infoCmds = flip fmap indexedSpells $ \(key, spellDef) ->
              let info = unlines . fmap ((++" ") . (" "++)) . lines $ Spell.information spellDef
                  -- After showing info, return to this same spell list
                  nextMachine = if null info
                                  then showSpellListForInfo cancel cid kind level
                                  else events [showStatusAlt' cid msg ac info] (showSpellListForInfo cancel cid kind level)
              in (Key [toLower key], nextMachine)

            kindName = if kind == Spell.M then "MAGE" else "PRIEST"
            msg = "Select spell:\n  ^A~^Z)Show Info  ^L)eave `[`E`S`C`]"
        run $ selectEsc (showStatusAlt cid msg ac) ((Key "l", cancel) : infoCmds)

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
                foldl (\acc (a, b) -> acc ++ " ^" ++ (a : "`)" ++ b)) "" (zip ['A'..'Z'] (snames lvGroup))
              else
                "   " ++ intercalate "   " (snames lvGroup)
            formatLvGroup lvGroup = " LV" ++ show (Spell.lv (head lvGroup)) ++ ": " ++ names lvGroup
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
      Nothing                     -> run $ events [showStatus cid "no happens."] next
      Just (Item.EqSpell ids, bp) -> do
         sdef' <- spellByID ids
         case sdef' of
           Just sdef -> if Spell.InCamp `elem` Spell.enableIn sdef then
                          run $ spellInCampNoCost sdef src dst (with [breakItem bp cid i] next)
                        else
                          run $ events [showStatus cid "can't use it here."] next
           Nothing   -> error "invalid spellId in useItemInCamp"
      Just (Item.Happens eid, bp) -> do
         let next' = with [breakItem bp cid i] next
         edef' <- asks (lookup eid . mazeEvents)
         case edef' of Nothing   -> run next'
                       Just edef -> run $ doEvent edef (const next') (const next')
                                                  (\sdef n -> if Spell.InCamp `elem` Spell.enableIn sdef then
                                                                spellInCampNoCost sdef src dst (with [breakItem bp cid i] n)
                                                              else
                                                                events [showStatus cid "can't use it here."] n)
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
        cs  = filter (isTarget . snd) (zip (toEnum <$> [0..]) its)
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




selectTradeItem :: ([Chara.ItemPos] -> String -> Event)
                -> PartyPos
                -> GameMachine
                -> GameMachine
selectTradeItem msgForSelect src cancel = GameAuto $ do
    ps   <- party <$> world
    c    <- characterByID =<< characterIDInPartyAt src
    cmds <- cmdNumPartiesID (\(i, cid) -> tradeTo cid)
    run $ if null (Chara.items c) then cancel
          else selectEsc (msgForSelect [(Chara.ItemA)..(Chara.ItemJ)] $ "Select target character(^1~^" ++ show (length ps) ++ ").\n^L)eave `[`E`S`C`]")
                         ((Key "l", cancel) : cmds)
  where
    canPoss c = filter (`notElem` Chara.equipPoss c) [(Chara.ItemA)..(Chara.ItemJ)]
    tradeTo dst = GameAuto $ do
        c'   <- characterByID =<< characterIDInPartyAt src
        cdst <- characterByID dst
        let msg = const $ msgForSelect (canPoss c') $ "Select item to trade to " ++ Chara.name cdst ++ "(" ++ textItemCandidate c' ++ ").\n^L)eave `[`E`S`C`]"
        run $ if      null (Chara.items c')      then cancel
              else if Chara.hasMaxCountItem cdst then selectTradeItem msgForSelect src cancel
                                                 else selectItem msg (const True) (trade dst) c' cancel
    trade :: CharacterID -> Chara.Character -> Chara.ItemPos -> GameMachine -> GameMachine
    trade dst c i cancel = GameAuto $ do
        let inf = Chara.itemInfAt c i
        when (i `notElem` Chara.equipPoss c) $ dropItem src i >> gainItem dst inf
        run $ tradeTo dst
        



breakItem :: (Int, Item.WhenBroken) -> CharacterID -> Chara.ItemPos -> GameState ()
breakItem (prob, to) cid i = do
    broken <- happens prob
    when broken $ do
      p <- characterByID cid
      let is = Chara.items p
          ix = fromEnum i
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
textItemCandidate c = "^A~^" ++ (Chara.itemPosToText . toEnum) (length (Chara.items c) - 1)


-- =================================================================================
-- for equipment.
-- ---------------------------------------------------------------------------------
equip :: ([Chara.ItemPos] -> String -> Event)
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
-- TODO:sp item.
equip' :: ([Chara.ItemPos] -> String -> Event)
       -> PartyPos
       -> Chara.Character
       -> [(Item.Define -> Bool, String)]
       -> GameMachine
       -> GameMachine
equip' _ _ _ [] next = next
equip' msgForSelect src c ((isTarget, typeText):rest) next = GameAuto $ do
    let ids = (\(a, b) -> (a, itemID b)) <$> filter (identified . snd) (zip (toEnum <$> [0..]) $ Chara.items c)
    items <- mapM itemByID (snd <$> ids)
    let idset = zipWith (\(a, b) c -> (a, b, c)) ids items
        tgts  = filter (Chara.canEquip c . thd3) . filter (isTarget . thd3) $ idset
        eps   = Chara.equipPoss c
        isCursed = any (\(p, _, def) -> p `elem` eps && Item.Cursed `elem` Item.attributes def) tgts
    run $ if null tgts then equip' msgForSelect src c rest next
          else if isCursed then events [msgForSelect [] ("* Your " ++ typeText ++ " is cursed and cannot be removed. *")] $ equip' msgForSelect src c rest next
          else selectItem' "n" (const $ msgForSelect (fst3 <$> tgts) $ msgBase)
                          ((`elem` (snd3 <$> tgts)) . itemID) selectEq c (eq Nothing)
  where
    msgBase = "Select equip " ++ typeText ++ "(" ++ textItemCandidate c ++ ").\n  N)o equip. `[`E`S`C`]"
    selectEq :: Chara.Character -> Chara.ItemPos -> GameMachine -> GameMachine
    selectEq c pos next = eq $ Just (Chara.itemInfAt c pos)

    eq :: Maybe ItemInf -> GameMachine
    eq item = GameAuto $ do
      cid <- characterIDInPartyAt src
      let iid = itemID <$> item
      isC  <- case iid of Nothing   -> return False
                          Just iid' -> (Item.Cursed `elem`) . Item.attributes <$> itemByID iid'
      let es = Chara.equips c
      items <- mapM itemByID (itemID <$> es)
      let es'  = snd <$> filter (not . isTarget. fst) (zip items es)
          es'' = case item of Nothing -> es'
                              Just i  -> i : es'
          c'   = c { Chara.equips = es'' }
      updateCharacter cid c'
      run $ events [showStatusFlash cid msgBase " \n  * The item is cursed. *  \n  " | isC] (equip' msgForSelect src c' rest next)


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
    return (msgForCasting msgForInputSpellInCamp, \(Key s) -> if null s then cancel else selectCastTarget s next)
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


showStatusSpellingCamp :: CharacterID -> String -> Event
showStatusSpellingCamp cid = showStatusFlash cid msgForInputSpellInCamp . (" \n  "++) .  (++"  \n ") 

msgForInputSpellInCamp = "Input spell.\n(Empty to cancel.)"

spellInCamp :: PartyPos -> GameMachine -> Spell.Name -> SpellTarget -> GameMachine
spellInCamp src next s (Left dst) = GameAuto $ do
    cid      <- characterIDInPartyAt src
    spellDef <- spellByName s
    run $ if s == "\n" then next else case spellDef of
      Just def -> if Spell.InCamp `elem` Spell.enableIn def then
                    spellInCamp' def src dst next
                  else
                    events [showStatusSpellingCamp cid "can't cast it here."] next
      Nothing  -> events [showStatusSpellingCamp cid "what?"] next
spellInCamp src next s (Right dst) = GameAuto $ do
    cid <- characterIDInPartyAt src
    run $ events [showStatusSpellingCamp cid "can't cast it here."] next

spellInCamp' :: Spell.Define -> PartyPos -> PartyPos -> GameMachine -> GameMachine
spellInCamp' def src dst next = GameAuto $ do
    cid  <- characterIDInPartyAt src
    c    <- characterInPartyAt src
    know <- knowSpell' c def
    can  <- canSpell'  c def
    if      not know then
      run $ events [showStatusSpellingCamp cid "you can't casting it."] next
    else if not can  then
      run $ events [showStatusSpellingCamp cid "no more MP."] next
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
        run $ with (fst4 <$> efs) (events [showStatus cid "done"] next)
      Spell.Resurrection hp ts -> do
        efs <- castResurrectionSpell hp ts (Left c) (Left tgt)
        let msgs = snd4 <$> efs
        run $ with (fst4 <$> efs) (events [showStatus cid (unlines msgs)] next)
      Spell.ChangeParam ad term etxt -> do
        efs <- castParamChangeSpell ad term etxt (Left c) (Left tgt)
        run $ with (fst4 <$> efs) (events [showStatus cid "done"] next)
      Spell.AddLight n s -> do
        efs <- castAddLight n s (Left c) (Left tgt)
        run $ with (fst4 <$> efs) (events [showStatus cid "done"] next)
      Spell.Event eid -> do
         edef' <- asks (lookup eid . mazeEvents)
         run $ case edef' of Nothing   -> next
                             Just edef -> doEvent edef (const next) (const next)
                                                  (\sdef n -> if Spell.InCamp `elem` Spell.enableIn sdef then
                                                                spellInCampNoCost sdef src dst n
                                                              else
                                                                events [showStatus cid "can't use it here."] n)
      Spell.CheckLocation t -> do
        p              <- currentPosition
        (fn, (w,h), m) <- mazeInfAt $ z p
        let msg = "you are at " ++ fn ++ "(" ++ show (x p) ++ ", " ++ show (y p) ++ ": " ++ show (direction p) ++ ")."
        run $ case t of
          Spell.OnlyCoord -> events [ showStatus cid msg
                                    , showStatus cid "done"] next
          Spell.ViewMap   -> showMap msg (0, 0) $ events [showStatus cid "done"] next

showMap :: String -> (Int, Int) -> GameMachine -> GameMachine
showMap msg (x,y) next = selectEsc (ShowMap (msg ++ "\n ^A-^W-^S-^D  ^L)eave `[`E`S`C`]") (x,y))
                                   [(Key "l", next)
                                   ,(Key "a", showMap msg (x+1,y) next)
                                   ,(Key "s", showMap msg (x,y-1) next)
                                   ,(Key "d", showMap msg (x-1,y) next)
                                   ,(Key "w", showMap msg (x,y+1) next)
                                   ]

-- --------------------------------------------------------------------------------

type CastAction = (Either Chara.Character Enemy.Instance           -- ^ src
                -> Either [PartyPos] [Enemy.Instance]              -- ^ dst (if it is empty, target is party)
                -> GameState [(GameState (), String, Bool, Bool)]) -- ^ effect (effect, message, damage occured, killed)

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
        return [(updateCharacter id dst', msg, False, False)]
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
        return [(updateEnemy dst (const dst'), msg, False, False)]
    return $ concat ts
castCureSpell _ _ _ _ = undefined


castResurrectionSpell :: Formula -> [(StatusError, Formula)] -> CastAction
castResurrectionSpell hpF sesF (Left src) (Left is) = do
    ts <- forM is $ \i -> do
      dst <- characterInPartyAt i
      id  <- characterIDInPartyAt i
      let ssc = statusErrorsOf dst
          targetSes = filter ((`elem` ssc) . fst) sesF
      if      Lost `elem` ssc then return [(return (), nameOf dst ++ " has been lost.", False, False)]
      else if null targetSes then return [(return (), "no happens.", False, False)]
      else do
        let (_, probF) = head targetSes
        m       <- formulaMapSO (Left src) (Left dst)
        prob    <- evalWith m probF
        success <- happens prob
        if success then do
            hp <- evalWith m hpF
            let dst' = setHp hp (removeStatusError Dead $ removeStatusError Ash dst)
            return [(updateCharacter id dst', nameOf dst ++ " has been resurrected.", False, False)]
        else
            return [(return (), nameOf dst ++ " could not be resurrected.", False, False)]
    return $ concat ts
castResurrectionSpell _ _ _ _ = undefined


castAddStatusErrorSpell :: [(StatusError, Formula, String)] -> CastAction
castAddStatusErrorSpell ses (Left src) (Right es) = concat <$> forM es (\e -> do
    if Enemy.hp e <= 0 then return []
    else do
        m <- formulaMapSO (Left src) (Right e)
        results <- forM ses $ \(se, prob, msg) -> do
            p <- evalWith m prob
            resist <- resistStatusError m se (Enemy.resistError $ Enemy.define e)
            success <- (&&) <$> happens p <*> pure (not resist)
            if success then do
                let e' = addStatusError se e
                let message = if msg == ""
                                then nameOf e ++ statusErrorMessage se
                                else nameOf e ++ " " ++ msg
                return [(updateEnemy e (const e'), message, False, se >= Dead)]
            else
                return [(return (), nameOf e ++ " resisted.", False, False)]
        return $ concat results
    )
castAddStatusErrorSpell ses (Right src) (Left cs) = concat <$> forM cs (\i -> do
    c <- characterInPartyAt i
    if Chara.hp c <= 0 then return []
    else do
        cid <- characterIDInPartyAt i
        m <- formulaMapSO (Right src) (Left c)
        results <- forM ses $ \(se, prob, msg) -> do
            eats <- allValidEquipAttrs c
            p <- evalWith m prob
            resist <- resistStatusError m se (concatMap Item.resistError eats)
            success <- (&&) <$> happens p <*> pure (not resist)
            if success then do
                let c' = addStatusError se c
                let message = if msg == ""
                                then nameOf c ++ statusErrorMessage se
                                else nameOf c ++ " " ++ msg
                return [(updateCharacter cid c', message, False, se >= Dead)]
            else
                return [(return (), nameOf c ++ " resisted.", False, False)]
        return $ concat results
    )
castAddStatusErrorSpell _ _ _ = undefined



castParamChangeSpell :: AdParam -> Term -> String -> CastAction
castParamChangeSpell ad term etxt (Left src) (Left is)
    | null is = do
          prmc <- toParamChange (Left src) (Left src) ad
          return [(modify $ \w -> w { partyParamDelta = Spell.applyChangeParam term prmc (partyParamDelta w) }
                  , "party " ++ etxt ++ ".", False, False)]
    | otherwise = concat <$> forM is (\i -> do
          dst  <- characterIDInPartyAt i
          cdst <- characterInPartyAt i
          prmc <- toParamChange (Left src) (Left cdst) ad
          if hpOf cdst == 0 then return []
          else return [(updateCharacter dst $ cdst { Chara.paramDelta = Spell.applyChangeParam term prmc (Chara.paramDelta cdst) }
                      , nameOf cdst ++ " " ++ etxt ++ ".", False, False)]
          )
castParamChangeSpell ad term etxt (Right src) (Right is) = concat <$> forM is (\dst -> do
    prmc <- toParamChange (Right src) (Right dst) ad
    if hpOf dst == 0 then return []
    else return [(updateEnemy dst $ const dst { Enemy.modParams = Spell.applyChangeParam term prmc (Enemy.modParams dst) }
                , nameOf dst ++ " " ++ etxt ++ ".", False, False)]
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
        return $ (updateEnemy e (const e'), msg, not noDamage, False)
               : [(return (), msg ++ "\n" ++ nameOf e ++ " is killed.", False, True) | Enemy.hp e' <= 0]
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
        return $ (updateCharacter cid c', msg, not noDamage, False)
               : [(return (), msg ++ "\n" ++ nameOf c ++ " is killed.", False, True) | hpOf c' <= 0]
    return $ concat ts
castDamageSpell f attrs src dst = error $ "castDamageSpell:" ++ show f ++ ", src=" ++ show src ++ ", dst=" ++ show dst


castAddLight :: Int -> Bool -> CastAction
castAddLight n s _ _ = return [(setLightValue s n, "it is brightly lit.", False, False)]


