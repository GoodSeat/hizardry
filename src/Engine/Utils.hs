{-# LANGUAGE TupleSections #-}
module Engine.Utils
where

import PreludeL
import Prelude hiding ((!!))
import System.Random
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader

import Data.List (find, sort)
import Data.Map hiding (filter, null, foldl,drop,take)
import Data.Maybe (fromMaybe, fromJust, catMaybes)

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

randomsIn :: Int -> [a] -> GameState [a]
randomsIn _ [] = return []
randomsIn n as
     | n <= 0 = return []
     | otherwise = do
         n <- randomNext 1 $ length as
         let as' = take (n - 1) as ++ drop n as
         (:) (as !! (n - 1)) <$> randomsIn (n-1) as'

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

allValidEquipAttrs :: Chara.Character -> GameState [Item.EquipBaseAttr]
allValidEquipAttrs c = do
    eqis  <- filter (/= Nothing) <$> mapM (equipOf c) Item.allEquipTypeTest
    return $ Item.equipBaseAttr . fromJust . Item.equipType . fromJust <$> eqis


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



lvup :: Chara.Character -> GameState (String, Chara.Character)
lvup c = do
    changes <- forM ps $ \(v, mv, dp, t) -> do
      d <- deltaParam v mv (Chara.age c)
      return (toText t d, dp d)
    let p' = foldl1 mappend $ Chara.param c : (snd <$> changes)
        c' = c { Chara.lv = Chara.lv c + 1, Chara.param = p' }

    (sn', maxmp') <- learnSpellsAndMps c'
    let ss' = sort $ Chara.spells c' ++ sn'

    m   <- formulaMapS $ Left c'
    hp' <- evalWith m (Chara.hpFormula $ Chara.job c)
    let maxhp' = max (Chara.maxhp c + 1) hp'
        uphp   = maxhp' - Chara.maxhp c
        txt = "You made the next level !\n\n"
           ++ "You gained " ++ show uphp ++ " HitPoitns."
           ++ foldl1 (++) (fst <$> changes)
           ++ if null sn' then "" else "\nYou have learned a new spell."
    return (txt, c' { Chara.maxhp = Chara.maxhp c + uphp
                    , Chara.hp    = Chara.hp c + uphp 
                    , Chara.maxmp = maxmp'
                    , Chara.mp    = maxmp'
                    , Chara.spells = ss'
                    })
  where
    toText p (-1) = "\nYou lost "   ++ p ++ " ."
    toText p   1  = "\nYou gained " ++ p ++ " ."
    toText _   _  = ""
    cp = Chara.param c
    mp = Chara.maxParam $ Chara.race c
    ps = [ (strength cp, strength mp, \n -> emptyParam { strength = n }, "strength")
         , (iq       cp, iq       mp, \n -> emptyParam { iq       = n }, "I.Q."    )
         , (piety    cp, piety    mp, \n -> emptyParam { piety    = n }, "piety"   )
         , (vitality cp, vitality mp, \n -> emptyParam { vitality = n }, "vitality")
         , (agility  cp, agility  mp, \n -> emptyParam { agility  = n }, "agility" )
         , (luck     cp, luck     mp, \n -> emptyParam { luck     = n }, "luck"    )
         ]
    deltaParam v maxv age = do
        n1 <- randomIn [1..4]
        n2 <- randomIn [0..130]
        n3 <- randomIn [1..6]
        if      n1 == 1  then return 0
        else if age < n2 && v < maxv then return 1
        else if age < n2             then return 0
        else if v >= maxv && n3 == 1 then return 0
        else if v < 2                then return 0
        else                              return (-1)

learnSpellsAndMps :: Chara.Character -> GameState ([SpellID], ([Int], [Int]))
learnSpellsAndMps c' = do
    let j = Chara.job c'
    m <- formulaMapS $ Left c'

    let ss = Chara.spells c'
    sn' <- fmap concat $ forM (Chara.learningSpells j) $ \(f, spells) -> do
        let so = filter (`elem`    ss) spells
            sn = filter (`notElem` ss) spells
        n' <- evalWith m f
        randomsIn (min n' (length spells) - length so) sn
    let ss' = sort $ ss ++ sn'
    spls' <- catMaybes <$> mapM spellByID ss'

    maxmp1' <- forM [1..(length . fst) (Chara.maxmp c')] $ \mlv -> do
      let m' = insert "mlv" mlv m
          nm = length $ filter ((== mlv) . Spell.lv) . filter ((== Spell.M) . Spell.kind) $ spls'
          mfs = fst (Chara.mpFormula j)
      mp' <- if length mfs < mlv then return 0 else evalWith m' $ mfs !! (mlv - 1)
      return $ max nm (max mp' (fst (Chara.maxmp c') !! (mlv - 1)))
    maxmp2' <- forM [1..(length . snd) (Chara.maxmp c')] $ \mlv -> do
      let m' = insert "mlv" mlv m
          nm = length $ filter ((== mlv) . Spell.lv) . filter ((== Spell.P) . Spell.kind) $ spls'
          mfs = snd (Chara.mpFormula j)
      mp' <- if length mfs < mlv then return 0 else evalWith m' $ mfs !! (mlv - 1)
      return $ max nm (max mp' (snd (Chara.maxmp c') !! (mlv - 1)))

    return (sort sn', (maxmp1', maxmp2'))


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

type TargetSO = Either Chara.Character Enemy.Instance

acOf :: TargetSO -> GameState Int
acOf (Right e) = return $ Enemy.ac (Enemy.define e) + deltaAC (Enemy.modParam e)
acOf s@(Left c) = do
    eats <- allValidEquipAttrs c
    let m = formulaMapSBase s
    acEq <- sum <$> mapM (evalWith m . Item.ac) eats

    acBase <- evalWith m (Chara.baseAC (Chara.job c))
    ps     <- partyParamDelta <$> world
    let pss = deltaAC . snd <$> ps
    let acC = sum $ (acBase : (deltaAC . snd <$> Chara.paramDelta c)) ++ pss
    return $ acC + acEq


paramOf :: TargetSO -> GameState Parameter
paramOf (Right ei) = return $ Enemy.param (Enemy.define ei) <> deltaParam (Enemy.modParam ei)
paramOf (Left c) = do
    ps <- partyParamDelta <$> world
    let pss = deltaParam . snd <$> ps
    let p = foldl1 (<>) $ (Chara.param c : (deltaParam . snd <$> Chara.paramDelta c)) ++ pss
    return p


vsEffectLabelsOf :: TargetSO -> GameState [(EffectLabel, Formula)]
vsEffectLabelsOf (Right e) = return $ Enemy.vsEffectLabels (Enemy.define e)
vsEffectLabelsOf (Left c) = concatMap Item.vsEffectLabels <$> allValidEquipAttrs c


applyVsEffect :: [EffectLabel] -> [(EffectLabel, Formula)] -> TargetSO -> TargetSO -> Int -> GameState Int
applyVsEffect es [] s o v = return v
applyVsEffect es (ev:evs) s o v = do
    m  <- insert "value" v <$> formulaMapSO s o
    v' <- if fst ev `elem` es then evalWith m $ snd ev
                              else return v
    applyVsEffect es evs s o v'


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


resistStatusError :: Map String Int -> StatusError -> [(StatusError, Formula)] -> GameState Bool
resistStatusError m s [] = return False
resistStatusError m s ((t, p):ts)
    | s == t = do
        resist <- happens =<< evalWith m p
        if resist then return True else resistStatusError m s ts
    | otherwise = resistStatusError m s ts

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
    return $ insert (prefix ++ "ac" ) ac
           . insert (prefix ++ "str") str
           . insert (prefix ++ "iq" ) iq
           . insert (prefix ++ "pie") pie
           . insert (prefix ++ "vit") vit
           . insert (prefix ++ "agi") agi
           . insert (prefix ++ "luc") luc
           $ m

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
                Camping p _           -> return p
                _                     -> err "failed on currentPosition."

