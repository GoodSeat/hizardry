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
import Data.Maybe (fromMaybe, fromJust)

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

option :: GameState ScenarioOption
option = asks scenarioOption

home :: GameState GameMachine
home = asks scenarioHome

err :: String -> GameState a
err = throwError

msgDebug :: String -> GameState ()
msgDebug t = do
    w <- world
    when (debugMode w) $ put w { debugMessage = t : debugMessage w }

-- =================================================================================

eval :: Formula -> GameState Int
eval = evalWith empty

evalWith :: Map String Int -> Formula -> GameState Int
evalWith m f = do
    w <- world
    let (res, g') = evalFormula m f $ randomGen w
    put w { randomGen = g' }
    msgDebug $ "    " ++ show f ++ " = " ++ show res
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
mazeAt z = asks $ g3 . (!!z) . mazes
  where g3 (_,_,a) = a

mazeSizeAt :: Int -> GameState (Int, Int)
mazeSizeAt z = asks $ g2 . (!!z) . mazes
  where g2 (_,a,_) = a

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


equipOf :: Chara.Character -> (Item.Define -> Bool) -> GameState (Maybe Item.Define)
equipOf c isTarget = do
    items <- mapM itemByID $ itemID <$> Chara.equips c
    let eqs = filter isTarget items
    return $ if null eqs then Nothing else Just (head eqs)


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

cmdNums :: Int -> (Int -> GameMachine) -> [(Input, GameMachine)]
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


cmdNumsWhen :: Int -> (Int -> (GameMachine, Bool)) -> [(Input, GameMachine, Bool)]
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
-- Character or Enemy as Target.
-- ---------------------------------------------------------------------------------

type TargetSO = Either Chara.Character (Enemy.Instance, Enemy.Define)

acOf :: TargetSO -> GameState Int
acOf s@(Left c) = do
    eqis <- filter (/= Nothing) <$> mapM (equipOf c) Item.allEquipTypeTest
    let eats = Item.equipBaseAttr . fromJust . Item.equipType . fromJust <$> eqis
        m    = formulaMapSBase s
    acEq <- sum <$> mapM (evalWith m . Item.ac) eats

    acBase <- evalWith m (Chara.baseAC (Chara.job c))
    ps     <- partyParamDelta <$> world
    let pss = deltaAC . snd <$> ps
    let acC = sum $ (acBase : (deltaAC . snd <$> Chara.paramDelta c)) ++ pss
    return $ acC + acEq


acOf (Right (e, def)) = return $ Enemy.ac def + deltaAC (Enemy.modParam e)

paramOf :: TargetSO -> GameState Parameter
paramOf (Left c) = do
    ps <- partyParamDelta <$> world
    let pss = deltaParam . snd <$> ps
    let p = foldl1 (<>) $ (Chara.param c : (deltaParam . snd <$> Chara.paramDelta c)) ++ pss
    return p

paramOf (Right (ei, def)) = return $ Enemy.param def <> deltaParam (Enemy.modParam ei)


toParamChange :: TargetSO -> TargetSO -> AdParam -> GameState ParamChange
toParamChange s o ad = do
    m <- formulaMapSO s o
    str <- evalWith m (adStrength ad)
    iq  <- evalWith m (adIq       ad)
    pie <- evalWith m (adPiety    ad)
    vit <- evalWith m (adVitality ad)
    agi <- evalWith m (adAgility  ad)
    luc <- evalWith m (adLuck     ad)
    ac  <- evalWith m (adAC       ad)
    return $ ParamChange {
        deltaParam = Parameter {
          strength = str 
        , iq       = iq  
        , piety    = pie 
        , vitality = vit 
        , agility  = agi 
        , luck     = luc 
        }
        , deltaAC = ac
        , effectName = adName ad
    }

-- =================================================================================
-- for Formula map.
-- ---------------------------------------------------------------------------------

addParamToMap :: String -> TargetSO -> Map String Int -> GameState (Map String Int)
addParamToMap prefix s m = do
    ac  <- acOf s
    str <- strength <$> paramOf s
    iq  <- iq       <$> paramOf s
    pie <- piety    <$> paramOf s
    vit <- vitality <$> paramOf s
    agi <- agility  <$> paramOf s
    luc <- luck     <$> paramOf s
    let m' = insert (prefix ++ "ac" ) ac
           . insert (prefix ++ "str") str
           . insert (prefix ++ "iq" ) iq
           . insert (prefix ++ "pie") pie
           . insert (prefix ++ "vit") vit
           . insert (prefix ++ "agi") agi
           . insert (prefix ++ "luc") luc
           $ m
    return m

addParamBase :: String -> TargetSO -> Map String Int -> Map String Int
addParamBase prefix (Left  s) = addParamBase' prefix s
addParamBase prefix (Right s) = addParamBase' prefix s

addParamBase' :: Object o => String -> o -> Map String Int -> Map String Int
addParamBase' prefix o = insert (prefix ++ "lv") (lvOf o)
                       . insert (prefix ++ "hp") (hpOf o)
                       . insert (prefix ++ "maxhp") (maxhpOf o)


formulaMapS :: TargetSO -> GameState (Map String Int)
formulaMapS s = addParamToMap "" s (formulaMapSBase s)

formulaMapSO :: TargetSO -> TargetSO -> GameState (Map String Int)
formulaMapSO s o = addParamToMap "" s (formulaMapSOBase s o) >>= addParamToMap "o." o

formulaMapSBase :: TargetSO -> Map String Int
formulaMapSBase s = addParamBase "" s empty

formulaMapSOBase :: TargetSO -> TargetSO -> Map String Int
formulaMapSOBase s o = addParamBase "o." o . addParamBase "" s $ empty


formulaMap1 :: Int -> Int -> Chara.Character -> GameState (Map String Int)
formulaMap1 i n o = do
     m <- formulaMapS (Left o)
     return $ insert "order" i
            . insert "partynum" n
            . insert "partynum" n
            . insert "age"     (Chara.age  o)
            . insert "exp"     (Chara.exp  o)
            . insert "gold"    (Chara.gold o)
            . insert "marks"   (Chara.marks o)
            . insert "rips"    (Chara.rips o)
            $ m 


addEvFlagToFormulaMap :: Map String Int -> GameState (Map String Int)
addEvFlagToFormulaMap m = do
  efs <- eventFlags <$> world
  return $ foldl (\acc i -> Data.Map.insert ("evf." ++ show i) (efs !! i) acc) m [0..99]


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

