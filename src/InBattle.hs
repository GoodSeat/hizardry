module InBattle
where

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import GameAuto
import Utils
import World
import Formula (parse')
import qualified Characters as Character
import qualified Enemies as Enemy
import qualified Spells as Spell
import qualified Items as Item

data BattleAction = ByParties Character.ID Action
                  | ByEnemies Enemy.Instance Enemy.Action
    deriving (Show)

data Action = Fight Int
            | Spell Spell.ID Int
            | Hide
            | Ambush Int
            | Run
            | Parry
            | UseItem Item.ID Int
    deriving (Show, Eq)

data Condition = Condition {
      afterWin  :: GameAuto 
    , afterRun  :: GameAuto
    , gotExps   :: Int
    , dropGold  :: Int
    , dropItems :: [Int]
    }

-- ==========================================================================
decideEnemyInstance :: Enemy.ID -> GameState [[Enemy.Instance]]
decideEnemyInstance = decideEnemyInstance' 1
  where
    decideEnemyInstance' :: Int -> Enemy.ID -> GameState [[Enemy.Instance]]
    decideEnemyInstance' n eid = if n > 4 then return [] else do
        def <- enemyOf eid
        n   <- eval $ Enemy.numOfOccurrences def
        withBack <- happens $ Enemy.withBackProb def
        bl  <- if withBack then decideEnemyInstance' (n + 1) =<< (Enemy.ID <$> eval (Enemy.backEnemyID def))
                           else return []
        det <- identifyEnemies
        el  <- createEnemyInstances n eid det True
        return $ el : bl

identifyEnemies :: GameState Bool
identifyEnemies = (<=) <$> randomNext 1 10 <*> (length . party <$> world)

createEnemyInstances :: Int          -- ^ num of create instaces.
                     -> Enemy.ID     -- ^ target id of enemy.
                     -> Bool         -- ^ enemies are determined or not.
                     -> Bool         -- ^ maybe drop item or not.
                     -> GameState [Enemy.Instance]
createEnemyInstances 0 _ _ _            = return []
createEnemyInstances n eid det dropItem = do
    def <- enemyOf eid
    es  <- createEnemyInstances (n - 1) eid det False
    mhp <- eval $ Enemy.maxhp def
    let e = Enemy.Instance {
      Enemy.id = eid, Enemy.determined = det, Enemy.hp = mhp, Enemy.statusErrors = [], Enemy.maybeDropItem = dropItem
    }
    return $ e : es

-- ==========================================================================
startBattle :: Enemy.ID             -- ^ encounted enemy.
            -> (GameAuto, GameAuto) -- ^ after battle won, run from battle.
            -> GameAuto
startBattle eid (g1, g2) = Auto $ do
    es <- decideEnemyInstance eid
    moveToBattle es
    -- TODO:maybe enemies (or parties) ambush.
    -- TODO:maybe friendly enemy.
    run $ events [Message "\nEncounter!\n"] (selectBattleCommand 1 [] con)
    -- TODO:following code is ideal...
--  select (Message "\nEncounter!\n") [(Clock, selectBattleCommand 1)]
  where
    con = Condition { afterWin = g1, afterRun = g2, gotExps = 0, dropGold = 0, dropItems = [] }


selectBattleCommand :: Int -- ^ character index in party(start from 1).
                    -> [(Character.ID, Action)]
                    -> Condition
                    -> GameAuto
selectBattleCommand i cmds con = Auto $ do
    p <- party <$> world
    if length p < i then run $ confirmBattle cmds con else do
        let cid = p !! (i - 1)
        c <- characterOf cid
        let cs = Character.enableBattleCommands $ Character.job c
        selectWhen (BattleCommand $ Character.name c ++ "'s Option\n\n" ++ concat (toMsg <$> cs))
                   [( Key "f"
                    , selectFightTarget $ \a -> selectBattleCommand (i + 1) ((cid, a) : cmds) con
                    , Character.Fight `elem` cs)
                   ,( Key "p"
                    , selectBattleCommand (i + 1) ((cid, Parry) : cmds) con
                    , Character.Parry `elem` cs)
                   ,( Key "r"
                    , afterRun con -- TODO:implement possible of fail to run.
                    , Character.Run `elem` cs)
                    ]
  where
    toMsg cmd = case cmd of Character.Fight   -> "F)ight\n"
                            Character.Spell   -> "S)pell\n"
                            Character.Hide    -> "H)ide\n"
                            Character.Ambush  -> "A)mbush\n"
                            Character.Run     -> "R)un\n"
                            Character.Parry   -> "P)arry\n"
                            Character.UseItem -> "U)se Item\n"

selectFightTarget :: (Action -> GameAuto) -> GameAuto
selectFightTarget next = Auto $ do
    ess <- lastEnemies
    if length ess == 1 then run $ next (Fight 1)
    else selectWhen (BattleCommand "Target group?")
                    [(Key "a", next (Fight 1), length ess > 0)
                    ,(Key "b", next (Fight 2), length ess > 1)
                    ,(Key "c", next (Fight 3), length ess > 2)
                    ,(Key "d", next (Fight 4), length ess > 3)]


confirmBattle :: [(Character.ID, Action)]
              -> Condition
              -> GameAuto
confirmBattle cmds con = Auto $ select (BattleCommand "Are you OK?\n\nF)ight\nT)ake Back")
                                       [(Key "f", startProgressBattle cmds con)
                                       ,(Key "t", selectBattleCommand 1 [] con)
                                       ]
                            
-- ==========================================================================

nextTurn :: Condition -> GameAuto
nextTurn con = Auto $ do
    ess  <- lastEnemies
    con' <- updateCondition con
    let ess'  = filter (\e -> Enemy.hp e > 0) <$> ess -- remove dead enemy.
        ess'' = filter (not . null) ess'              -- remove null line.
    ess''' <- sequence $ tryDetermineGroup <$> ess''
    moveToBattle ess'''
    if null ess''' then run $ wonBattle con'
    else run $ selectBattleCommand 1 [] con'

tryDetermineGroup :: [Enemy.Instance] -> GameState [Enemy.Instance]
tryDetermineGroup es = do
    determine <- identifyEnemies
    return $ if determine then fmap (\e -> e { Enemy.determined = True }) es else es

updateCondition :: Condition -> GameState Condition
updateCondition con = do
    ess   <- lastEnemies
    drops <- forM (concat ess) (\e -> do
        edef <- enemyOf $ Enemy.id e
        if Enemy.hp e > 0 then return (0, 0, [])
        else (,,) <$> eval (Enemy.dropGold edef) <*> pure (Enemy.exp edef) <*> pure [])
    let (g, exp, is) = foldl' (\(g1, e1, is1) (g2, e2, is2) -> (g1 + g2, e1 + e2, is1 ++ is2)) (0, 0, []) drops
    return $ con { dropGold = dropGold con + g, gotExps = gotExps con + exp, dropItems = dropItems con ++ is }

wonBattle :: Condition -> GameAuto
wonBattle con = Auto $ do
    ps <- party <$> world
    let e = gotExps con  `div` length ps
        g = dropGold con `div` length ps
        es = Just (Message $ "Each survivor got " ++ show e ++ " E.P.")
           : (if g > 0 then Just (Message $ "Each survivor got " ++ show g ++ " G.P.") else Nothing)
           : []
    run $ events (catMaybes es) (afterWin con)


-- ==========================================================================
startProgressBattle :: [(Character.ID, Action)]
                    -> Condition
                    -> GameAuto
startProgressBattle cmds con = Auto $ run =<< nextProgressBattle <$> determineActions cmds <*> pure con


nextProgressBattle :: [BattleAction]
                   -> Condition
                   -> GameAuto
nextProgressBattle [] con     = nextTurn con
nextProgressBattle (a:as) con = Auto $ run =<< events <$> act a <*> pure (nextProgressBattle as con)

act :: BattleAction -> GameState [Event]
act (ByParties id a) = case a of
    Fight l -> do
      ses <- Character.statusErrors <$> characterOf id
      enm <- filter (\e -> Enemy.hp e > 0) <$> ((!!) <$> lastEnemies <*> pure (l - 1))
      if   any (`elem` Character.cantFightStatus) ses then return []
      else if null enm then return []
      else do
        c <- characterOf id
        e <- randomIn enm
        (h, d) <- fightDamage l c e
        let e' = e { Enemy.hp = Enemy.hp e - d }
        updateEnemy l e (const e')
        fmap Message <$> fightMessage c e' (h, d)
    Parry   -> return []
    Run     -> return []
act (ByEnemies e a) = case a of
    Enemy.Fight n d t effs -> return []
    Enemy.Run              -> return []


fightDamage :: Int -> Character.Character -> Enemy.Instance -> GameState (Int, Int)
fightDamage l c e = do
    edef     <- enemyOf $ Enemy.id e
    let tryCountF = parse' "lv/5 + 1" -- TODO:
        jobBonusF = parse' "lv/3 + 2" -- TODO:
        damageF   = parse' "2d2"      -- TODO:from wepon.
        weponAt   = 0 -- TODO:AT value of wepon.
        stBonus   = 0 -- TODO:sum of equip item's ST value.
        m         = makeMap edef
    tryCount <- max <$> (min <$> evalWith m tryCountF <*> pure 10) <*> pure weponAt
    jobBonus <- evalWith m jobBonusF
    let str      = Character.strength . Character.param $ c
        strBonus = if str >= 16 then str - 15 else if str < 6 then str - 6 else 0
        hitSkill = jobBonus + strBonus + stBonus
        atSkill  = max (min (Enemy.ac edef + hitSkill - 3 * l) 19) 1
    rs <- replicateM tryCount $ do
        hit <- (<=) <$> randomNext 1 20 <*> pure atSkill
        dam <- (+) <$> evalWith m damageF <*> pure (max 0 strBonus)
        let dam' = if (not . null $ Enemy.statusErrors e) then dam * 2 else dam
        return $ if hit then (1, dam') else (0, 0)
    return $ foldl' (\(h1, d1) (h2, d2) -> (h1 + h2, d1 + d2)) (0, 0) rs
  where
    makeMap edef = Map.fromList [("ac", Enemy.ac edef)
                                ,("lv", Character.lv c)
                                ,("str", Character.strength . Character.param $ c)
                                ]

fightMessage :: Character.Character -> Enemy.Instance -> (Int, Int) -> GameState [String]
fightMessage c e (h, d) = do
    en <- (if Enemy.determined e then Enemy.name else Enemy.nameUndetermined) <$> enemyOf (Enemy.id e)
    v  <- randomIn vs
    let m1 = Character.name c ++ " " ++ v ++ "\n " ++ en ++ ".\n"
    let m2 = if h == 0 then " and misses." else " and hits " ++ show h ++ " times for " ++ show d ++ ".\n"
    let m3 = if Enemy.hp e <= 0 then en ++ " is killed." else ""
    return $ (m1 ++ m2) : if null m3 then [] else (m1 ++ m3) : []
  where
    vs = ["leaps at", "attempts to slice", "thrusts violently at", "tries to ram", "tries to bash", "charges at", "tries to slash"]

--  vs = ["charges at", "claws at"]

-- "*** hidden away"
--  vs = ["tries to ambush"]

-- ==========================================================================
determineActions :: [(Character.ID, Action)]
                 -> GameState [BattleAction]
determineActions cmds = do
    pcs <- sequence $ toPair <$> cmds
    ecs <- sequence =<< map toEnemyAction <$> (concat <$> lastEnemies)
    return $ snd <$> sortOn fst (pcs ++ ecs)
  where
    toPair :: (Character.ID, Action) -> GameState (Int, BattleAction)
    toPair (id, act) = do
        agi <- Character.agility . Character.param <$> characterOf id
        key <- agiBonus agi
        return (key, ByParties id act)


toEnemyAction :: Enemy.Instance -> GameState (Int, BattleAction)
toEnemyAction ei = do
    def <- enemyOf $ Enemy.id ei
    key <- agiBonus $ Character.agility . Enemy.param $ def
    act <- randomIn $ Enemy.actions def
    return (key, ByEnemies ei act)


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

