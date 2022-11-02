module Engine.Utils
where

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


inspectCharacter :: GameMachine -> Bool -> Int -> GameMachine
inspectCharacter h canSpell i = GameAuto $ do
    ids <- party <$> world
    c   <- characterOf (ids !! (i - 1))
    let cancel = inspectCharacter h canSpell i
    run $ selectWhen (ShowStatus i msg SingleKey)
                     [(Key "l", h, True)
                     ,(Key "s", inputSpell c iCast sCast (spellInCamp i cancel) cancel, canSpell)
                     ,(Key "1", inspectCharacter h canSpell 1, length ids >= 1)
                     ,(Key "2", inspectCharacter h canSpell 2, length ids >= 2)
                     ,(Key "3", inspectCharacter h canSpell 3, length ids >= 3)
                     ,(Key "4", inspectCharacter h canSpell 4, length ids >= 4)
                     ,(Key "5", inspectCharacter h canSpell 5, length ids >= 5)
                     ,(Key "6", inspectCharacter h canSpell 6, length ids >= 6)]
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
           -> (String -> Int -> GameMachine) -> GameMachine -> GameMachine
inputSpell c msgForCasting msgForSelecting next cancel = GameAuto $
    return (msgForCasting "Input spell.\n(Empty to cancel.)",
            \(Key s) -> if null s then cancel else selectCastTarget s next)
  where
    selectCastTarget :: String -> (String -> Int -> GameMachine) -> GameMachine
    selectCastTarget s next = GameAuto $ do
        ps   <- party <$> world
        def' <- spellByName s
        sdb  <- asks spells
        case def' of
            Nothing  -> run $ next s 1
            Just def -> case Spell.target def of
                Spell.OpponentSingle -> do
                                        ess <- lastEnemies
                                        select True  (length ess) (next s)
                Spell.OpponentGroup  -> do
                                        ess <- lastEnemies
                                        let mx = if Chara.knowSpell' sdb def c then length ess else 1
                                        select True  mx (next s)
                Spell.AllySingle     -> do
                                        let mx = if Chara.knowSpell' sdb def c then length ps else 1
                                        select False mx (next s)
                _                    -> run $ next s 0
    select toEnemy mx nextWith = if mx <= 1 then run (nextWith 1) else
                                 run $ selectWhen (msgForSelecting $
                                         if toEnemy then "Target group? (1~"     ++ show mx ++ ")\n\nC)ancel" 
                                                    else "Target character? (1~" ++ show mx ++ ")\n\nC)ancel")
                                         [(Key "1", nextWith 1, mx > 0)
                                         ,(Key "2", nextWith 2, mx > 1)
                                         ,(Key "3", nextWith 3, mx > 2)
                                         ,(Key "4", nextWith 4, mx > 3)
                                         ,(Key "5", nextWith 5, mx > 4)
                                         ,(Key "6", nextWith 6, mx > 5)
                                         ,(Key "c", cancel, True)]


spellInCamp :: Int -> GameMachine -> String -> Int -> GameMachine
spellInCamp i next s l = GameAuto $ do
    spellDef <- spellByName s
    case spellDef of Just def -> if Spell.InCamp `elem` Spell.enableIn def then
                                   run $ spellInCamp' def i l next
                                 else
                                   run $ events [ShowStatus i "can't cast it hear." SingleKey] next
                     Nothing  -> run $ events [ShowStatus i "what?" SingleKey] next

spellInCamp' :: Spell.Define -> Int -> Int -> GameMachine -> GameMachine
spellInCamp' def i l next = GameAuto $ do
    ids <- party <$> world
    c   <- characterOf (ids !! (i - 1))
    sdb <- asks spells
    if      not (Chara.knowSpell' sdb def c) then
      run $ events [ShowStatus i "you can't casting it." SingleKey] next
    else if not (Chara.canSpell'  sdb def c) then
      run $ events [ShowStatus i "no more MP." SingleKey] next
    else do
      updateCharacter (ids !! (i - 1)) (Chara.costSpell' sdb def c)
      case Spell.effect def of
        Spell.Damage _  -> undefined
        Spell.Cure f ss -> do
          let tgt = case Spell.target def of
                      Spell.AllySingle -> [l]
                      Spell.AllyAll    -> [1..length ids]
                      _                -> []
          efs <- castCureSpell (Spell.name def) f ss (Left c) (Left tgt)
          run $ with (fst <$> efs) (events [ShowStatus i "done" SingleKey] next)


castCureSpell :: String -> Formula -> [StatusError]
              -> Either Chara.Character Enemy.Instance  -- ^ src
              -> Either [Int] [Enemy.Instance]          -- ^ dst
              -> GameState [(GameState (), String)]
castCureSpell n f ss (Left src) (Left is) = do
    ps  <- party <$> world
    ts  <- forM is $ \i -> do
      let idc = (i - 1) `mod` length ps
      dst <- characterOf (ps !! idc)
      let ssc = statusErrorsOf dst
      if hpOf dst == 0 && all (`notElem` ssc) ss then return []
      else do
        d <- evalWith (formulaMapSO src dst) f
        let dst' = foldl (&) (setHp (hpOf dst + d) dst) (removeStatusError <$> ss)
        let msg = if hpOf dst /= hpOf dst' then
                    nameOf dst ++ " heal " ++ show (hpOf dst' - hpOf dst) ++ "."
                  else
                    nameOf dst ++ " cured."
        return [(updateCharacter (ps !! idc) dst', msg)]
    return $ concat ts
castCureSpell _ _ _ _ _ = undefined



-- =================================================================================
-- for Characters.
-- ---------------------------------------------------------------------------------

characterOf :: CharacterID -> GameState Chara.Character
characterOf id = do
    db <- allCharacters <$> world
    return $ db ! id

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


