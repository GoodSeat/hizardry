module Utils
where

import Control.Monad.State
import Control.Monad.Reader

import Data.Map hiding (filter)

import GameAuto
import World
import qualified Characters as Character
import qualified Enemies as Enemy
import Formula


movePlace :: Place -> GameState ()
movePlace p = modify $ \w -> w { place = p }

moveToBattle :: [[Enemy.Instance]] -> GameState ()
moveToBattle es = do
    p <- place <$> world
    case p of InMaze pos -> movePlace $ InBattle pos es
              _          -> err "invalid moveToBattle."

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
    cs  <- sequence $ characterOf <$> ids
    let gp = sum $ Character.gold <$> cs
    forM_ ids $ \id' -> updateCharacterWith id' $ \c -> c { Character.gold = if id' == id then gp else 0 }

-- =================================================================================

lastEnemies :: GameState [[Enemy.Instance]]
lastEnemies = do
    p <- place <$> world
    case p of InBattle _ ess -> return ess
              _              -> err "invalid lastEnemies."

-- =================================================================================

eval :: Formula -> GameState Int
eval f = do
    w <- world
    let (res, g') = Formula.eval (fromList []) f $ randomGen w
    put w { randomGen = g' }
    case res of Right i   -> return i
                Left  msg -> err msg

