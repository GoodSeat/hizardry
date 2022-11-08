{-# LANGUAGE TupleSections #-}
module Engine.CharacterAction
where

import Prelude hiding (lookup)
import Control.Monad (when, join, forM)
import Control.Monad.Reader (asks)
import Data.Map hiding (filter, null, foldl, take, drop)
import Data.Function ((&))

import Engine.GameAuto
import Engine.Utils
import Engine.InEvent
import Data.Primitive
import Data.World
import Data.Maze
import Data.Formula
import qualified Data.Characters as Chara
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell
import qualified Data.Items as Item


-- =================================================================================
-- character inspection.
-- ---------------------------------------------------------------------------------

inspectCharacter :: GameMachine -> Bool -> PartyPos -> GameMachine
inspectCharacter h canSpell i = GameAuto $ do
    pn <- length . party <$> world
    c  <- partyAt' i
    let cancel = inspectCharacter h canSpell i
    run $ selectWhen (ShowStatus i msg SingleKey)
                     [(Key "l", h, True)
                     ,(Key "s", inputSpell c iCast sCast (spellInCamp i cancel) cancel, canSpell)
                     ,(Key "u", selectItem sItem identified (selectUseTarget sCast (useItemInCamp i cancel)) c cancel, True)
                     ,(Key "d", selectDropItem dItem i c cancel, True)
                     ,(Key "1", inspectCharacter h canSpell F1, pn >= 1)
                     ,(Key "2", inspectCharacter h canSpell F2, pn >= 2)
                     ,(Key "3", inspectCharacter h canSpell F3, pn >= 3)
                     ,(Key "4", inspectCharacter h canSpell B4, pn >= 4)
                     ,(Key "5", inspectCharacter h canSpell B5, pn >= 5)
                     ,(Key "6", inspectCharacter h canSpell B6, pn >= 6)]
  where
    msg = if canSpell then
            "U)se Item     D)rop Item    T)rade Item    E)qiup  \n" ++
            "R)ead Spell   S)pell        P)ool Money            \n" ++
            "#)Inspect     L)eave                               "
          else
            "U)se Item     D)rop Item    T)rade Item    E)qiup  \n" ++
            "R)ead Spell   P)ool Money   #)Inspect      L)eave  "
    iCast = flip (ShowStatus i) SequenceKey
    sCast = flip (ShowStatus i) SingleKey
    sItem = const (sCast "Select item.  L)eave")
    dItem = sCast

-- =================================================================================
-- for item.
-- ---------------------------------------------------------------------------------

useItemInCamp :: PartyPos -> GameMachine -> Chara.ItemPos -> SpellTarget -> GameMachine
useItemInCamp src next i (Left dst) = GameAuto $ do
    c   <- partyAt' src
    def <- itemByID $ Chara.itemAt c i
    case Item.usingEffect def of
      Nothing                     -> run $ events [ShowStatus src "no happens." SingleKey] next
      Just (Item.EqSpell ids, bp) -> do
         sdef' <- spellByID ids
         case sdef' of
           Just sdef -> if Spell.InCamp `elem` Spell.enableIn sdef then
                          run $ spellInCampNoCost sdef src dst (with [breakItem bp src i] next)
                        else
                          run $ events [ShowStatus src "can't use it here." SingleKey] next
           Nothing   -> error "invalid spellId in useItemInCamp"
      Just (Item.Happens eid, bp) -> do
         let next' = with [breakItem bp src i] next
         edef' <- asks (lookup eid . mazeEvents)
         case edef' of Nothing   -> run next'
                       Just edef -> run $ doEvent edef next' next'
useItemInCamp _ _ _ _ = error "invalid useItemInCamp"


breakItem :: (Int, Item.WhenBroken) -> PartyPos -> Chara.ItemPos -> GameState ()
breakItem (prob, to) src i = do
    broken <- happens prob
    when broken $ do
      idc <- partyAt  src
      p   <- partyAt' src
      let is = Chara.items p
          ix = Chara.itemPosToNum i
          is' = case to of Item.Lost        -> take ix is ++ drop (ix + 1) is
                           Item.ChangeTo i' -> take ix is ++ [i'] ++ drop (ix + 1) is
      updateCharacter idc (p { Chara.items = is' }) 

selectItem :: (String -> Event)
           -> (ItemInf -> Bool)
           -> (Chara.Character -> Chara.ItemPos -> GameMachine -> GameMachine)
           -> Chara.Character
           -> GameMachine
           -> GameMachine
selectItem msgForSelect isTarget next c cancel = GameAuto $ do
    is <- asks items
    let nameOf id = Item.name (is ! id)
        its = Chara.items c
        cs  = filter (isTarget . snd) (zip (Chara.numToItemPos <$> [0..]) its)
        msg = (\(t, inf) -> Chara.itemPosToText t ++ ")" ++ nameOf (itemID inf)) <$> cs
    return (msgForSelect $ "Select item.\nL)eave\n\n" ++ unlines msg,
            \(Key s) -> if s == "l" then cancel
                        else case Chara.itemPosByChar s of
                          Nothing -> selectItem msgForSelect isTarget next c cancel
                          Just i  -> if i `elem` (fst <$> cs) then next c i cancel
                                     else selectItem msgForSelect isTarget next c cancel
           )

selectUseTarget :: (String -> Event)
                -> (Chara.ItemPos -> SpellTarget -> GameMachine)
                -> Chara.Character
                -> Chara.ItemPos
                -> GameMachine 
                -> GameMachine
selectUseTarget msgForTargeting next c i cancel = GameAuto $ do
    let id = Chara.itemAt c i
    ps  <- party <$> world
    def <- itemByID id
    case Item.usingEffect def of
      Nothing                    -> run $ next i (Left F1) -- MEMO:target should be ignored...
      Just (Item.EqSpell ids, _) -> do
         sdef' <- spellByID ids
         case sdef' of
           Just sdef -> run $ selectSpellTarget sdef undefined False (next i) msgForTargeting cancel
           Nothing   -> error "invalid spellId in selectUseTarget"

selectDropItem :: (String -> Event)
               -> PartyPos
               -> Chara.Character
               -> GameMachine
               -> GameMachine
selectDropItem msgForSelect src =
    selectItem (const $ msgForSelect "Select drop item.\nL)eave") (const True) drop
  where
    drop :: Chara.Character -> Chara.ItemPos -> GameMachine -> GameMachine
    drop c i cancel = GameAuto $ do
                        def <- itemByID $ Chara.itemAt c i
                        if Item.CantDrop `elem` Item.attributes def then
                          run $ events [msgForSelect "you cannot drop it."] (selectDropItem msgForSelect src c cancel)
                        else
                          run $ with [dropItem src i] cancel

dropItem :: PartyPos -> Chara.ItemPos -> GameState ()
dropItem = breakItem (100, Item.Lost)

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
      Spell.AllySingle     -> return (False, length ps)
      _                    -> return (False, 1) -- MEMO:target should be ignored...
    select toEnemy (if know then mx else 1) next
  where
    select toEnemy mx nextWith =
        let toDst = if toEnemy then Right . toEnemyLine else Left . toPartyPos in
        if mx <= 1 then
          run (nextWith $ toDst 1)
        else
          run $ selectWhen (msgForSelecting $
                  if toEnemy then "Target group? (1~"     ++ show mx ++ ")\n\nC)ancel"
                             else "Target character? (1~" ++ show mx ++ ")\n\nC)ancel")
                  [(Key "1", nextWith (toDst 1), mx > 0)
                  ,(Key "2", nextWith (toDst 2), mx > 1)
                  ,(Key "3", nextWith (toDst 3), mx > 2)
                  ,(Key "4", nextWith (toDst 4), mx > 3)
                  ,(Key "5", nextWith (toDst 5), mx > 4)
                  ,(Key "6", nextWith (toDst 6), mx > 5)
                  ,(Key "c", cancel, True)]


spellInCamp :: PartyPos -> GameMachine -> Spell.Name -> SpellTarget -> GameMachine
spellInCamp src next s (Left dst) = GameAuto $ do
    spellDef <- spellByName s
    case spellDef of
        Just def -> if Spell.InCamp `elem` Spell.enableIn def then
                      run $ spellInCamp' def src dst next
                    else
                      run $ events [ShowStatus src "can't cast it hear." SingleKey] next
        Nothing  -> run $ events [ShowStatus src "what?" SingleKey] next
spellInCamp src next s (Right dst) = error "can't target enemy in spellInCamp"

spellInCamp' :: Spell.Define -> PartyPos -> PartyPos -> GameMachine -> GameMachine
spellInCamp' def src dst next = GameAuto $ do
    c    <- partyAt' src
    know <- knowSpell' c def
    can  <- canSpell'  c def
    if      not know then
      run $ events [ShowStatus src "you can't casting it." SingleKey] next
    else if not can  then
      run $ events [ShowStatus src "no more MP." SingleKey] next
    else do
      join $ updateCharacter <$> partyAt src <*> costSpell' c def
      run $ spellInCampNoCost def src dst next

spellInCampNoCost :: Spell.Define -> PartyPos -> PartyPos -> GameMachine -> GameMachine
spellInCampNoCost def src dst next = GameAuto $ do
    pn  <- length . party <$> world
    c   <- partyAt' src
    case Spell.effect def of
      Spell.Damage _  -> undefined
      Spell.Cure f ss -> do
        let tgt = case Spell.target def of
                    Spell.AllySingle -> [dst]
                    Spell.AllyAll    -> toPartyPos <$> [1..pn]
                    _                -> []
        efs <- castCureSpell (Spell.name def) f ss (Left c) (Left tgt)
        run $ with (fst <$> efs) (events [ShowStatus src "done" SingleKey] next)


castCureSpell :: Spell.Name -> Formula -> [StatusError]
              -> Either Chara.Character Enemy.Instance  -- ^ src
              -> Either [PartyPos] [Enemy.Instance]     -- ^ dst
              -> GameState [(GameState (), String)]
castCureSpell n f ss (Left src) (Left is) = do
    ps  <- party <$> world
    ts  <- forM is $ \i -> do
      dst <- partyAt' i
      let ssc = statusErrorsOf dst
      if hpOf dst == 0 && all (`notElem` ssc) ss then return []
      else do
        d <- evalWith (formulaMapSO src dst) f
        let dst' = foldl (&) (setHp (hpOf dst + d) dst) (removeStatusError <$> ss)
        let msg = if hpOf dst /= hpOf dst' then
                    nameOf dst ++ " heal " ++ show (hpOf dst' - hpOf dst) ++ "."
                  else
                    nameOf dst ++ " cured."
        return [(join $ updateCharacter <$> partyAt i <*> pure dst', msg)]
    return $ concat ts
castCureSpell _ _ _ _ _ = undefined



