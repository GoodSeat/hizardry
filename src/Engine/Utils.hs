module Engine.Utils
where

import PreludeL
import Prelude hiding ((!!))
import System.Random
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Data.List (find)
import Data.Map hiding (filter, null, foldl)
import Data.Maybe (fromMaybe)
import Data.Function ((&))

import Engine.GameAuto
import Data.Primitive
import Data.World
import Data.Maze
import Data.Formula
import qualified Data.Characters as Chara
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell


-- =================================================================================
-- General
-- ---------------------------------------------------------------------------------

world :: GameState World
world = get

option :: GameState Option
option = asks scenarioOption

home :: GameState GameMachine
home = asks scenarioHome

err :: String -> GameState a
err = throwError

-- =================================================================================

eval :: Formula -> GameState Int
eval = evalWith empty

evalWith :: Map String Int -> Formula -> GameState Int
evalWith m f = do
    w <- world
    let (res, g') = evalFormula m f $ randomGen w
    put w { randomGen = g' }
    case res of Right i   -> return i
                Left  msg -> err msg

-- =================================================================================
-- Random
-- ---------------------------------------------------------------------------------

happens :: Int -> GameState Bool
happens prob = (prob >=) <$> randomNext 1 100

randomNext :: Int -> Int -> GameState Int
randomNext min max = do
    w <- world
    let (v, g') = randomR (min, max) $ randomGen w
    put w { randomGen = g' }
    return v

randomIn :: [a] -> GameState a
randomIn as = do
    n <- randomNext 1 $ length as
    return $ as !! (n - 1)

-- =================================================================================
-- General commands.
-- ---------------------------------------------------------------------------------

mazeAt :: Int -> GameState Maze
mazeAt z = asks $ (!!z) . mazes

movePlace :: Place -> GameState ()
movePlace p = modify $ \w -> w { place = p }


spellByID :: SpellID -> GameState (Maybe Spell.Define)
spellByID n = do
    ss <- asks spells
    return $ Data.Map.lookup n ss

spellByName :: String -> GameState (Maybe Spell.Define)
spellByName n = do
    ss <- asks spells
    return $ (ss!) <$> Spell.findID ss n


inspectCharacter :: GameMachine -> Bool -> PartyPos -> GameMachine
inspectCharacter h canSpell i = GameAuto $ do
    pn <- length . party <$> world
    c  <- partyAt' i
    let cancel = inspectCharacter h canSpell i
    run $ selectWhen (ShowStatus i msg SingleKey)
                     [(Key "l", h, True)
                     ,(Key "s", inputSpell c iCast sCast (spellInCamp i cancel) cancel, canSpell)
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


inputSpell :: Chara.Character -> (String -> Event) -> (String -> Event)
           -> (String -> SpellTarget -> GameMachine)
           -> GameMachine -> GameMachine
inputSpell c msgForCasting msgForSelecting next cancel = GameAuto $
    return (msgForCasting "Input spell.\n(Empty to cancel.)",
            \(Key s) -> if null s then cancel else selectCastTarget s next)
  where
    selectCastTarget :: String -> (String -> SpellTarget -> GameMachine) -> GameMachine
    selectCastTarget s next = GameAuto $ do
        ps   <- party <$> world
        def' <- spellByName s
        sdb  <- asks spells
        case def' of
            Nothing  -> run $ next s (Left F1) -- MEMO:target should be ignored...
            Just def -> case Spell.target def of
                Spell.OpponentSingle -> do
                    ess <- lastEnemies
                    select True (length ess) (next s)
                Spell.OpponentGroup  -> do
                    ess <- lastEnemies
                    let mx = if Chara.knowSpell' sdb def c then length ess else 1
                    select True  mx (next s)
                Spell.AllySingle     -> do
                    let mx = if Chara.knowSpell' sdb def c then length ps else 1
                    select False mx (next s)
                _                    -> run $ next s (Left F1) -- MEMO:target should be ignored...
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


spellInCamp :: PartyPos -> GameMachine -> String -> SpellTarget -> GameMachine
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
    pn  <- length . party <$> world
    c   <- partyAt' src
    sdb <- asks spells
    if      not (Chara.knowSpell' sdb def c) then
      run $ events [ShowStatus src "you can't casting it." SingleKey] next
    else if not (Chara.canSpell'  sdb def c) then
      run $ events [ShowStatus src "no more MP." SingleKey] next
    else do
      join $ updateCharacter <$> partyAt src <*> pure (Chara.costSpell' sdb def c)
      case Spell.effect def of
        Spell.Damage _  -> undefined
        Spell.Cure f ss -> do
          let tgt = case Spell.target def of
                      Spell.AllySingle -> [dst]
                      Spell.AllyAll    -> toPartyPos <$> [1..pn]
                      _                -> []
          efs <- castCureSpell (Spell.name def) f ss (Left c) (Left tgt)
          run $ with (fst <$> efs) (events [ShowStatus src "done" SingleKey] next)


castCureSpell :: String -> Formula -> [StatusError]
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



-- =================================================================================
-- for Characters.
-- ---------------------------------------------------------------------------------

characterOf :: CharacterID -> GameState Chara.Character
characterOf id = do
    db <- allCharacters <$> world
    return $ db ! id

partyAt :: PartyPos -> GameState CharacterID
partyAt pos = (!! (partyPosToNum pos - 1)) . party <$> world

partyAt' :: PartyPos -> GameState Chara.Character
partyAt' pos = characterOf =<< partyAt pos


toParty :: CharacterID -> GameState ()
toParty id = do
    w <- world
    let w' = w { party           = party w ++ [id]
               , inTarvernMember = filter (/= id) $ inTarvernMember w
               , inMazeMember    = filter (\(id', _) -> id' /= id) $ inMazeMember w
               }
    put w'

updateCharacter :: CharacterID -> Chara.Character -> GameState ()
updateCharacter id c = do
    w  <- world
    let db = allCharacters w
        w' = w { allCharacters = insert id c db }
    put w'

updateCharacterWith :: CharacterID -> (Chara.Character -> Chara.Character) -> GameState ()
updateCharacterWith id f = do
    db <- allCharacters <$> world
    updateCharacter id (f $ db ! id)

poolGold :: CharacterID -> GameState ()
poolGold id = do
    ids <- party <$> world
    cs  <- mapM characterOf ids
    let gp = sum $ Chara.gold <$> cs
    forM_ ids $ \id' -> updateCharacterWith id' $ \c -> c { Chara.gold = if id' == id then gp else 0 }

sortPartyAuto :: GameState ()
sortPartyAuto = sortPartyAutoWith . party =<< world

sortPartyAutoWith :: [CharacterID] -> GameState ()
sortPartyAutoWith psOrg = do
    w   <- world
    ps' <- zip psOrg <$> mapM characterOf psOrg
    let p2s = filter (isCantFight . snd) ps'
        p1s = filter (`notElem` p2s) ps'
    put $ w { party = fst <$> (p1s ++ p2s) }


-- =================================================================================
-- for Enemies.
-- ---------------------------------------------------------------------------------

lastEnemies :: GameState [[Enemy.Instance]]
lastEnemies = do
    p <- place <$> world
    case p of InBattle _ ess -> return ess
              _              -> return []

enemyOf :: EnemyID -> GameState Enemy.Define
enemyOf eid = do
    es <- asks enemies
    return $ es ! eid

currentEnemyByNo :: Int -- ^ target enemy noID.
                 -> GameState (Maybe Enemy.Instance)
currentEnemyByNo no = do
    p <- place <$> world
    (pos, ess) <- case p of InBattle pos ess -> return (pos, ess)
                            _                -> err "invalid currentEnemyByNo."
    return $ find (\ei -> Enemy.noID ei == no) (concat ess)

findEnemyLine :: Enemy.Instance -- ^ target enemy.
              -> GameState (Maybe Int)
findEnemyLine e = do
    p <- place <$> world
    (_, ess) <- case p of InBattle pos ess -> return (pos, ess)
                          _                -> err "invalid findEnemyLine."
    return $ search 1 e ess
  where
    search _ _ []       = Nothing
    search l e (es:ess) = if e `elem` es then Just l else search (l + 1) e ess


updateEnemy :: Enemy.Instance -- ^ target enemy.
            -> (Enemy.Instance -> Enemy.Instance) -> GameState ()
updateEnemy e f = do
    l' <- findEnemyLine e
    let l  = Data.Maybe.fromMaybe undefined l'
    p  <- place <$> world
    (pos, ess) <- case p of InBattle pos ess -> return (pos, ess)
                            _                -> err "invalid updateEnemy."
    movePlace $ InBattle pos (updateEnemyLine l e ess f)
  where
    updateEnemyLine :: Int
                    -> Enemy.Instance
                    -> [[Enemy.Instance]]
                    -> (Enemy.Instance -> Enemy.Instance) -> [[Enemy.Instance]]
    updateEnemyLine _ _ [] _ = []
    updateEnemyLine l e (es:ess) f
        | l == 1    = updateEnemyInstance es e f : ess
        | otherwise = es : updateEnemyLine (l - 1) e ess f

    updateEnemyInstance :: [Enemy.Instance]
                        -> Enemy.Instance
                        -> (Enemy.Instance -> Enemy.Instance) -> [Enemy.Instance]
    updateEnemyInstance (e1:es) e f
        | e1 == e   = f e : es
        | otherwise = e1 : updateEnemyInstance es e f
    updateEnemyInstance [] _ _ = undefined


-- =================================================================================
-- Other.
-- ---------------------------------------------------------------------------------

currentPosition :: GameState Position
currentPosition = do
    plc <- place <$> world
    case plc of InMaze p     -> return p
                InBattle p _ -> return p
                _            -> err "failed on currentPosition."

formulaMapSO :: Object s => Object o => s -> o -> Map String Int
formulaMapSO s o = fromList [
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


