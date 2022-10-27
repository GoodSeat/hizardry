module Engine.InBattle
where

import Data.List
import Data.Maybe
import Data.Function
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.State

import Engine.GameAuto
import Engine.Utils
import Engine.BattleAction
import Data.World
import Data.Primitive
import Data.Formula (parse')
import qualified Data.Characters as Character
import qualified Data.Enemies as Enemy
import qualified Data.Spells as Spell
import qualified Data.Items as Item

data BattleAction = ByParties Character.ID Action
                  | ByEnemies Int Enemy.Instance Enemy.Action
    deriving (Show)

data Action = Fight Int
            | Spell String Int
            | Hide
            | Ambush Int
            | Run
            | Parry
            | UseItem Item.ID Int
            | CantMove
    deriving (Show, Eq)

data Condition = Condition {
      afterWin     :: GameMachine
    , afterRun     :: GameMachine
    , gotExps      :: Int
    , dropGold     :: Int
    , dropItems    :: [Int]
    , defaultOrder :: [Character.ID] -- ^ party order when battle started.
    }

-- ==========================================================================
decideEnemyInstance :: Enemy.ID -> GameState [[Enemy.Instance]]
decideEnemyInstance e = decideEnemyInstance' 1 e >>= tryDetermineEnemies
  where
    decideEnemyInstance' :: Int -> Enemy.ID -> GameState [[Enemy.Instance]]
    decideEnemyInstance' l eid = if l > 4 then return [] else do
        def <- enemyOf eid
        n   <- eval $ Enemy.numOfOccurrences def
        withBack <- happens $ Enemy.withBackProb def
        bl  <- if withBack then decideEnemyInstance' (l + 1) . Enemy.ID =<< eval (Enemy.backEnemyID def)
                           else return []
        el  <- createEnemyInstances n l eid True
        return $ el : bl

createEnemyInstances :: Int          -- ^ num of create instaces.
                     -> Int          -- ^ belong line no.
                     -> Enemy.ID     -- ^ target id of enemy.
                     -> Bool         -- ^ maybe drop item or not.
                     -> GameState [Enemy.Instance]
createEnemyInstances 0 _ _ _              = return []
createEnemyInstances n l eid dropItem = do
    def <- enemyOf eid
    es  <- createEnemyInstances (n - 1) l eid False
    mhp <- eval $ Enemy.hpFormula def
    let e = Enemy.Instance {
        Enemy.id            = eid
      , Enemy.noID          = n + 10 * l
      , Enemy.determined    = False
      , Enemy.hp            = mhp
      , Enemy.maxhp         = mhp
      , Enemy.statusErrors  = []
      , Enemy.maybeDropItem = dropItem
      , Enemy.modAc         = 0
    }
    return $ e : es

tryDetermineEnemies :: [[Enemy.Instance]] -> GameState [[Enemy.Instance]]
tryDetermineEnemies = mapM $ \es -> do
    determine <- (<=) <$> randomNext 1 10 <*> (length . party <$> world)
    return $ if determine then fmap (\e -> e { Enemy.determined = True }) es else es

-- ==========================================================================
startBattle :: Enemy.ID                   -- ^ encounted enemy.
            -> (GameMachine, GameMachine) -- ^ after battle won, run from battle.
            -> GameMachine
startBattle eid (g1, g2) = GameAuto $ do
    es <- decideEnemyInstance eid
    ps <- party <$> world
    -- TODO:maybe enemies (or parties) ambush.
    -- TODO:maybe friendly enemy.
    let con = Condition {
      afterWin = g1, afterRun = g2, gotExps = 0, dropGold = 0, dropItems = [], defaultOrder = ps
    }
    run $ events [MessageTime (-1000) "\nEncounter!\n"]
          (GameAuto $ moveToBattle es >> run (selectBattleCommand 1 [] con))
    -- TODO:following code is ideal...
--  select (Message "\nEncounter!\n") [(Clock, selectBattleCommand 1)]

moveToBattle :: [[Enemy.Instance]] -> GameState ()
moveToBattle es = do
    p <- place <$> world
    case p of InMaze pos     -> movePlace $ InBattle pos es
              InBattle pos _ -> movePlace $ InBattle pos es
              _              -> err "invalid moveToBattle."


selectBattleCommand :: Int -- ^ character index in party(start from 1).
                    -> [(Character.ID, Action)]
                    -> Condition
                    -> GameMachine
selectBattleCommand i cmds con = GameAuto $ do
    p <- party <$> world
    if length p < i then
      run $ confirmBattle cmds con
    else do
      let cid = p !! (i - 1)
      c <- characterOf cid
      let cs     = Character.enableBattleCommands $ Character.job c
          next a = selectBattleCommand (i + 1) ((cid, a) : cmds) con
          cancel = selectBattleCommand i cmds con
      if isCantFight c then
        run $ next CantMove
      else 
        run $ selectWhen (BattleCommand $ Character.name c ++ "'s Option\n\n" ++ concatMap toMsg cs)
                         [( Key "f"
                          , selectFightTarget next
                          , Character.Fight `elem` cs && i <= 3)
                         ,( Key "p"
                          , next Parry
                          , Character.Parry `elem` cs)
                         ,( Key "s"
                          , inputSpell next cancel
                          , Character.Spell `elem` cs)
                         ,( Key "r"
                          , events [Message $ Character.name c ++ " flees."] (afterRun con) -- TODO:implement possible of fail to run.
                          , Character.Run `elem` cs)
                         ,( Key " "
                          , events [None] (selectBattleCommand i cmds con)
                          , True)
                          ]
  where
    toMsg cmd = case cmd of Character.Fight   -> "F)ight\n"
                            Character.Spell   -> "S)pell\n"
                            Character.Hide    -> "H)ide\n"
                            Character.Ambush  -> "A)mbush\n"
                            Character.Run     -> "R)un\n"
                            Character.Parry   -> "P)arry\n"
                            Character.UseItem -> "U)se Item\n"

selectFightTarget :: (Action -> GameMachine) -> GameMachine
selectFightTarget next = GameAuto $ do
    ess <- lastEnemies
    if length ess == 1 then run $ next (Fight 1)
    else run $ selectWhen (BattleCommand "Target group?")
                          [(Key "1", next (Fight 1), not (null ess))
                          ,(Key "2", next (Fight 2), length ess > 1)
                          ,(Key "3", next (Fight 3), length ess > 2)
                          ,(Key "4", next (Fight 4), length ess > 3)]

inputSpell :: (Action -> GameMachine) -> GameMachine -> GameMachine
inputSpell next cancel = GameAuto $
    return (SpellCommand "Input spell.\n(Empty to cancel.)",
            \(Key s) -> if null s then cancel else selectCastTarget s next)

selectCastTarget :: String -> (Action -> GameMachine) -> GameMachine
selectCastTarget s next = GameAuto $ do
    ess <- lastEnemies
    p   <- party <$> world
    def <- spellByName s
    case def of
        Nothing  -> run $ next (Spell s 1)
        Just def -> case Spell.target def of
            Spell.OpponentSingle -> select (length ess) (next . Spell s)
            Spell.OpponentGroup  -> select (length ess) (next . Spell s)
            Spell.AllySingle     -> select (length p  ) (next . Spell s)
            _                    -> run $ next (Spell s 0)
  where
    select mx nextWith = if mx <= 1 then run (nextWith 1) else
                         run $ selectWhen (BattleCommand "Target group?")
                               [(Key "1", nextWith 1, mx > 0)
                               ,(Key "2", nextWith 2, mx > 1)
                               ,(Key "3", nextWith 3, mx > 2)
                               ,(Key "4", nextWith 4, mx > 3)
                               ,(Key "5", nextWith 5, mx > 4)
                               ,(Key "6", nextWith 6, mx > 5)]


confirmBattle :: [(Character.ID, Action)]
              -> Condition
              -> GameMachine
confirmBattle cmds con = select (BattleCommand "Are you OK?\n\nF)ight\nT)ake Back")
                                [(Key "f", startProgressBattle cmds con)
                                ,(Key "t", selectBattleCommand 1 [] con)
                                ]

-- ==========================================================================

nextTurn :: Condition -> GameMachine
nextTurn con = GameAuto $ do
    ps   <- party <$> world
    rs   <- replicateM (length ps) (randomNext 0 100)
    forM_ (zip ps rs) $ \(p, r) -> do
      c <- characterOf p
      updateCharacter p $ foldl (&) c (whenToNextTurn r <$> statusErrorsOf c)
    sortPartyAutoWith (defaultOrder con)
    -- TODO!:if all character dead, move to gameover.

    con' <- updateCondition con
    ess  <- execState (do
                modify $ fmap $ filter (\e -> Enemy.hp e > 0) -- remove dead or fleed enemy.
                modify $ filter (not . null)                  -- remove null line.
            ) <$> lastEnemies
    ess' <- tryDetermineEnemies ess
    moveToBattle ess'
    run $ (if null ess' then wonBattle else selectBattleCommand 1 []) con'

updateCondition :: Condition -> GameState Condition
updateCondition con = do
    ess   <- lastEnemies
    drops <- forM (concat ess) (\e -> do
        edef <- enemyOf $ Enemy.id e
        if notElem Dead $ Enemy.statusErrors e then return (0, 0, [])
        else (,,) <$> eval (Enemy.dropGold edef) <*> pure (Enemy.exp edef) <*> pure [])
    let (g, exp, is) = foldl' (\(g1, e1, is1) (g2, e2, is2) -> (g1 + g2, e1 + e2, is1 ++ is2)) (0, 0, []) drops
    return $ con { dropGold = dropGold con + g, gotExps = gotExps con + exp, dropItems = dropItems con ++ is }

wonBattle :: Condition -> GameMachine
wonBattle con = GameAuto $ do
    ps <- party <$> world
    let e = gotExps con  `div` length ps
        g = dropGold con `div` length ps
        es = [Just (Message $ "Each survivor got " ++ show e ++ " E.P."), if g > 0 then Just (Message $ "Each survivor got " ++ show g ++ " G.P.") else Nothing]
    forM_ ps $ flip updateCharacterWith (\c -> c { Character.exp = Character.exp c + e, Character.gold = Character.gold c + g })
    run $ events (catMaybes es) (afterWin con)


-- ==========================================================================
startProgressBattle :: [(Character.ID, Action)]
                    -> Condition
                    -> GameMachine
startProgressBattle cmds con = GameAuto $ run =<< nextProgressBattle <$> determineActions cmds <*> pure con


nextProgressBattle :: [BattleAction]
                   -> Condition
                   -> GameMachine
nextProgressBattle as con = foldr act (nextTurn con) as

act :: BattleAction -> GameMachine -> GameMachine
act (ByParties id a) next = GameAuto $ do
    cantFight <- isCantFight <$> characterOf id
    if cantFight then run next
    else case a of
        Fight l   -> run $ fightOfCharacter id l next
        Spell s l -> run $ spell s (Left id) l next
        Parry     -> run next
        Run       -> run next
        CantMove  -> run next
        _         -> undefined
act (ByEnemies l e a) next = GameAuto $ do
    e_ <- currentEnemyByNo $ Enemy.noID e
    case e_ of
      Nothing -> run next
      Just e' -> do
        edef <- enemyOf $ Enemy.id e'
        if isCantFight (e', edef) then run next
        else case a of
          Enemy.Fight n d t effs -> run $ fightOfEnemy e' n d t effs next
          Enemy.Spelling f       -> do
              s'  <- spellByID . Spell.ID =<< eval f
              cid <- eval $ parse' "1d6"
              case s' of Just s  -> run $ spell' s (Right e') cid next
                         Nothing -> run $ spellUnknown "?" (Right e') 0 next
          Enemy.Run              -> do
              en   <- enemyNameOf e'
              updateEnemy e' $ const e' { Enemy.hp = 0 }
              run $ events [Message $ en ++ " flees."] next
          _                      -> undefined

-- "*** hidden away"
--  vs = ["tries to ambush"]

-- ==========================================================================
determineActions :: [(Character.ID, Action)]
                 -> GameState [BattleAction]
determineActions cmds = do 
    pcs  <- mapM toPair cmds
    elss <- zip [1..] <$> lastEnemies
    let els = concatMap (\(l, es) -> zip (repeat l) es) elss
    ecs  <- mapM toEnemyAction els
    return $ snd <$> sortOn fst (pcs ++ ecs)
  where
    toPair :: (Character.ID, Action) -> GameState (Int, BattleAction)
    toPair (id, act) = do
        agi <- agility . Character.param <$> characterOf id
        key <- agiBonus agi
        return (key, ByParties id act)


toEnemyAction :: (Int, Enemy.Instance) -> GameState (Int, BattleAction)
toEnemyAction (l, ei) = do
    def <- enemyOf $ Enemy.id ei
    key <- agiBonus $ agility . Enemy.param $ def
    act <- randomIn $ Enemy.actions def
    return (key, ByEnemies l ei act)


agiBonus :: Int -> GameState Int
agiBonus agi = do
    b <- (+) <$> randomNext 1 10 <*> pure (bonus agi)
    return $ max 2 (min 10 b)
  where
    bonus agi | agi <=  3 =  3
              | agi <=  5 =  2
              | agi <=  7 =  1
              | agi <= 14 =  0
              | agi <= 15 = -1
              | agi <= 16 = -2
              | agi <= 17 = -3
              | otherwise = -4

-- ==========================================================================
