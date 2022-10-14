module Utils
where

import Control.Monad.State
import Control.Monad.Reader

import Data.Map hiding (filter, null)

import Primitive
import GameAuto
import World
import qualified Characters as Character
import qualified Enemies as Enemy
import qualified Spells as Spell
import Formula


movePlace :: Place -> GameState ()
movePlace p = modify $ \w -> w { place = p }

moveToBattle :: [[Enemy.Instance]] -> GameState ()
moveToBattle es = do
    p <- place <$> world
    case p of InMaze pos     -> movePlace $ InBattle pos es
              InBattle pos _ -> movePlace $ InBattle pos es
              _              -> err "invalid moveToBattle."

-- =================================================================================

characterOf :: Character.ID -> GameState Character.Character
characterOf id = do
    db <- allCharacters <$> world
    return $ db ! id

toParty :: Character.ID -> GameState ()
toParty id = do
    w <- world
    let w' = w { party           = party w ++ [id]
               , inTarvernMember = filter (/= id) $ inTarvernMember w
               , inMazeMember    = filter (\(id', _) -> id' /= id) $ inMazeMember w
               }
    put w'

updateCharacter :: Character.ID -> Character.Character -> GameState ()
updateCharacter id c = do
    w  <- world
    let db = allCharacters w
        w' = w { allCharacters = insert id c db }
    put w'

updateCharacterWith :: Character.ID -> (Character.Character -> Character.Character) -> GameState ()
updateCharacterWith id f = do
    db <- allCharacters <$> world
    updateCharacter id (f $ db ! id)

poolGold :: Character.ID -> GameState ()
poolGold id = do
    ids <- party <$> world
    cs  <- mapM characterOf ids
    let gp = sum $ Character.gold <$> cs
    forM_ ids $ \id' -> updateCharacterWith id' $ \c -> c { Character.gold = if id' == id then gp else 0 }

sortPartyAuto :: GameState ()
sortPartyAuto = sortPartyAutoWith . party =<< world

sortPartyAutoWith :: [Character.ID] -> GameState ()
sortPartyAutoWith psOrg = do
    w   <- world
    ps' <- zip psOrg <$> mapM characterOf psOrg
    let p2s = filter (isCantFight . snd) ps'
        p1s = filter (`notElem` p2s) ps'
    put $ w { party = fst <$> (p1s ++ p2s) }


-- =================================================================================

lastEnemies :: GameState [[Enemy.Instance]]
lastEnemies = do
    p <- place <$> world
    case p of InBattle _ ess -> return ess
              _              -> err "invalid lastEnemies."

enemyOf :: Enemy.ID -> GameState Enemy.Define
enemyOf eid = do
    es <- asks enemies
    return $ es ! eid

updateEnemy :: Int            -- ^ target enemy line(1～4).
            -> Enemy.Instance -- ^ target enemy.
            -> (Enemy.Instance -> Enemy.Instance) -> GameState ()
updateEnemy l e f = do
    p <- place <$> world
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

-- =================================================================================
spellByName :: String -> GameState (Maybe Spell.Define)
spellByName n = do
    ss <- asks spells
    return $ (ss!) <$> Spell.findID ss n

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
