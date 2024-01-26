{-# LANGUAGE TupleSections #-}
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

import Engine.GameAuto
import Data.Primitive
import Data.World
import Data.Maze
import Data.Formula
import qualified Data.Characters as Chara
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell
import qualified Data.Items as Item


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
mazeAt z = asks $ snd . (!!z) . mazes

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

itemByID :: ItemID -> GameState Item.Define
itemByID id = asks $ flip (!) id . items


-- =================================================================================
-- for Characters.
-- ---------------------------------------------------------------------------------

characterByID :: CharacterID -> GameState Chara.Character
characterByID id = do
    db <- allCharacters <$> world
    return $ db ! id

characterIDInPartyAt :: PartyPos -> GameState CharacterID
characterIDInPartyAt pos = (!! (partyPosToNum pos - 1)) . party <$> world

characterInPartyAt :: PartyPos -> GameState Chara.Character
characterInPartyAt pos = characterByID =<< characterIDInPartyAt pos


addCharacterToParty :: CharacterID -> GameState ()
addCharacterToParty id = do
    w <- world
    put $ w { party           = party w ++ [id]
            , inTarvernMember = filter (/= id) $ inTarvernMember w
            , inMazeMember    = filter (\(id', _) -> id' /= id) $ inMazeMember w
            }

deleteCharacter :: CharacterID -> GameState ()
deleteCharacter id = do
    w <- world
    put $ w { party           = filter (/= id) $ party w
            , inTarvernMember = filter (/= id) $ inTarvernMember w
            , inMazeMember    = filter (\(id', _) -> id' /= id) $ inMazeMember w
            }


updateCharacter :: CharacterID -> Chara.Character -> GameState ()
updateCharacter id c = do
    w  <- world
    put $ w { allCharacters = insert id c (allCharacters w) }

updateCharacterWith :: CharacterID -> (Chara.Character -> Chara.Character) -> GameState ()
updateCharacterWith id f = do
    db <- allCharacters <$> world
    updateCharacter id (f $ db ! id)

poolGoldTo :: CharacterID -> GameState ()
poolGoldTo id = do
    ids <- party <$> world
    cs  <- mapM characterByID ids
    let gp = sum $ Chara.gold <$> cs
    forM_ ids $ \id' -> updateCharacterWith id' $ \c -> c { Chara.gold = if id' == id then gp else 0 }

divvyGold :: GameState ()
divvyGold = do
    ids <- party <$> world
    cs  <- mapM characterByID ids
    let ga = sum $ Chara.gold <$> cs
        n  = length cs
        gp = ga `div` n
        su = ga - gp * n
        gs = fmap (gp+) (replicate su 1 ++ repeat 0)
    forM_ (zip ids gs) $ \(id', g) -> updateCharacterWith id' $ \c -> c { Chara.gold = g }


knowSpell :: CharacterID -> SpellID -> GameState Bool
knowSpell cid sid = Chara.knowSpell sid <$> characterByID cid 

knowSpell' :: Chara.Character -> Spell.Define -> GameState Bool
knowSpell' c def = asks (Chara.knowSpell' . spells) <*> pure def <*> pure c

canSpell :: CharacterID -> SpellID -> GameState Bool
canSpell cid sid = asks (Chara.canSpell . spells) <*> pure sid <*> characterByID cid

canSpell' :: Chara.Character -> Spell.Define -> GameState Bool
canSpell' c def = asks (Chara.canSpell' . spells) <*> pure def <*> pure c

costSpell' :: Chara.Character -> Spell.Define -> GameState Chara.Character
costSpell' c def = asks (Chara.costSpell' . spells) <*> pure def <*> pure c


sortPartyAuto :: GameState ()
sortPartyAuto = sortPartyAutoWith . party =<< world

sortPartyAutoWith :: [CharacterID] -> GameState ()
sortPartyAutoWith psOrg = do
    w   <- world
    ps' <- zip psOrg <$> mapM characterByID psOrg
    let p2s = filter (isCantFight . snd) ps'
        p1s = filter (`notElem` p2s) ps'
    put $ w { party = fst <$> (p1s ++ p2s) }

-- =================================================================================
-- for Enemies.
-- ---------------------------------------------------------------------------------

enemyDefineByID :: EnemyID -> GameState Enemy.Define
enemyDefineByID eid = asks ((!) . enemies) <*> pure eid

------------------------------------------------------------------------------------
-- for Enemy Instance

lastEnemies :: GameState [[Enemy.Instance]]
lastEnemies = do
    p <- place <$> world
    case p of InBattle _ ess -> return ess
              _              -> return []

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
-- for make number commands.
-- ---------------------------------------------------------------------------------

cmdNums :: Int
        -> (Int -> GameMachine)
        -> [(Input, GameMachine)]
--cmdNums n f = [(Key (show i), f i) | i <- [1..n]]
cmdNums n f = rmv <$> cmdNumsWhen n ((,True).f)
  where rmv (a,b,_) = (a,b)


cmdNumParties :: ((PartyPos, Chara.Character) -> GameMachine)
              -> GameState [(Input, GameMachine)]
cmdNumParties f = fmap rmv <$> cmdNumPartiesWhen ((,True).f)
  where rmv (a,b,_) = (a,b)


cmdNumPartiesID :: ((PartyPos, CharacterID) -> GameMachine)
                    -> GameState [(Input, GameMachine)]
cmdNumPartiesID f = fmap rmv <$> cmdNumPartiesIDWhen ((,True).f)
  where rmv (a,b,_) = (a,b)


cmdNumsWhen :: Int
            -> (Int -> (GameMachine, Bool))
            -> [(Input, GameMachine, Bool)]
cmdNumsWhen n f = [(Key (show i), fst (f i), snd (f i)) | i <- [1..n]]


cmdNumPartiesWhen :: ((PartyPos, Chara.Character) -> (GameMachine, Bool))
                  -> GameState [(Input, GameMachine, Bool)]
cmdNumPartiesWhen f = do
    np <- length . party <$> world
    cs <- mapM characterByID . party =<< world
    let f' x = f (toPartyPos x, cs !! (x - 1))
    return [(Key (show i), fst (f' i), snd (f' i)) | i <- [1..np]]


cmdNumPartiesIDWhen :: ((PartyPos, CharacterID) -> (GameMachine, Bool))
                    -> GameState [(Input, GameMachine, Bool)]
cmdNumPartiesIDWhen f = do
    np  <- length . party <$> world
    ids <- party <$> world
    let f' x = f (toPartyPos x, ids !! (x - 1))
    return [(Key (show i), fst (f' i), snd (f' i)) | i <- [1..np]]


-- =================================================================================
-- Other.
-- ---------------------------------------------------------------------------------

currentPosition :: GameState Position
currentPosition = do
    plc <- place <$> world
    case plc of InMaze p              -> return p
                InBattle p _          -> return p
                FindTreasureChest p _ -> return p
                Camping p             -> return p
                _                     -> err "failed on currentPosition."

formulaMapS :: Object s => s -> Map String Int
formulaMapS s = fromList [
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
    ]

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

addEvFlagToFormulaMap :: Map String Int -> GameState (Map String Int)
addEvFlagToFormulaMap m = do
  efs <- eventFlags <$> world
  return $ foldl (\acc i -> Data.Map.insert ("evf." ++ show i) (efs !! i) acc) m [0..99]

