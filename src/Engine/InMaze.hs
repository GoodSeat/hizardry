{-# LANGUAGE TupleSections #-}
module Engine.InMaze (openCamp) where

import Control.Monad (when)
import Control.Monad.State (modify, forM_, guard, gets)
import Control.Monad.Reader (asks)
import Data.Function ((&))
import Data.List (find, nub)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)
import qualified Data.Map as Map

import Engine.GameAuto
import Engine.Utils
import Engine.InBattle
import Engine.InEvent (doEvent, setLightValueWith, setLightValue, resetEffectInOnlyBattle, returnToCastle)
import Engine.CharacterAction (inspectCharacter)
import Data.World
import Data.Maze
import Data.Primitive
import qualified Data.Spells as Spell
import qualified Data.Characters as Chara
import qualified Data.GameEvent as Ev

import Control.CUI (translate)

-- =======================================================================
-- depends on Scenario.

enterWithoutEncount :: Event -> Position -> GameMachine
enterWithoutEncount evMoved p = GameAuto $ do
  ev <- eventOn p
  run $ enterGrid ev False evMoved p

enterMaybeEncount :: Event -> Position -> GameMachine
enterMaybeEncount evMoved p = GameAuto $ do
  ev <- eventOn p
  run $ enterGrid ev True evMoved p

enterMaybeEncount' :: Event -> Position -> GameMachine
enterMaybeEncount' evMoved p = GameAuto $ do
  ev <- eventOn' p
  run $ enterGrid ev True evMoved p

eventOn :: Position -> GameState (Maybe Ev.Define)
eventOn p = do
  evMap <- asks eventMap
  evDB  <- asks mazeEvents
  case Map.lookup (x p, y p, z p) evMap of
    Nothing  -> eventOn' p
    Just eid -> return $ Map.lookup eid evDB

eventOn' :: Position -> GameState (Maybe Ev.Define)
eventOn' p = do
  evMap <- asks eventMapDir
  evDB  <- asks mazeEvents
  case Map.lookup p evMap of
    Nothing  -> return Nothing
    Just eid -> return $ Map.lookup eid evDB

-- =======================================================================

enterGrid :: Maybe Ev.Define -- ^ happened event.
          -> Bool            -- ^ probably encount enemy.
          -> Event           -- ^ event when moved.
          -> Position        -- ^ moved position.
          -> GameMachine
enterGrid e probEncount evMoved p = GameAuto $ do
    movePlace $ InMaze p
    resetEffectInOnlyBattle
    -- record visit history.
    modify $ \w -> w { visitHitory = Map.insert (coordOf p) True (visitHitory w) }
    -- TODO!:all character lost if they are in stone.
    lab <- mazeAt $ z p
    let c = coordOf p
    when (Dark `elem` noticesInView lab p 0 0) $ setLightValue False 0
    encount <- if probEncount then
                 if visiblityAt lab p 0 0 B /= Passage then checkRoomBattle c
                 else fmap (,False) <$> checkEncount c False
               else return Nothing
    case e of Just edef -> run $ doEvent edef (escapeEvent evMoved) (endEvent evMoved) cantSpelling
              Nothing   -> case encount of
                Nothing         -> run $ with [updateRoomVisit] (select evMoved $ moves p)
                Just (ei, isRB) -> run $ encountEnemy ei isRB
  where
    cantSpelling _ = events [message "can't spelling at this place."]
              

checkRoomBattle :: Coord -> GameState (Maybe (EnemyID, Bool))
checkRoomBattle c = do
    isRB <- notElem c . roomBattled <$> world
    res  <- checkEncount c isRB
    return $ (,isRB) <$> res

checkEncount :: Coord -> Bool -> GameState (Maybe EnemyID)
checkEncount c checkRoomBattle = do
    emap <- if checkRoomBattle then asks roomBattleMap else asks encountMap
    r    <- randomNext 1 100
    let es' = do {
        (prob, es) <- Map.lookup c emap;
        guard $ r < prob;
        return es
        }
    case es' of Nothing -> return Nothing
                Just es -> Just <$> randomIn es

ouch :: Position -> GameMachine
ouch p = ouch1
  where
    ouch1 = with [d1] $ select (flashMessage ( -30) " Ouch !! ") $ (Clock, ouch2) : moves'
    ouch2 = with [d2] $ select (flashMessage ( -20) " Ouch !! ") $ (Clock, ouch3) : moves'
    ouch3 = with [d3] $ select (flashMessage ( -30) " Ouch !! ") $ (Clock, ouch4) : moves'
    ouch4 = with [d4] $ select (flashMessage (-330) " Ouch !! ") $ moves p
    moves' = map ((\f (a, b) -> (a, f b)) $ with [d4]) (moves p)
    d1  = modify $ \w ->  w { frameTrans = frameTrans w . translate ( 0,  1)
                            , sceneTrans = sceneTrans w . translate ( 0,  1) }
    d2  = modify $ \w ->  w { frameTrans = frameTrans w . translate (-1,  0)
                            , sceneTrans = sceneTrans w . translate (-1,  0) }
    d3  = modify $ \w ->  w { frameTrans = frameTrans w . translate ( 2, -1)
                            , sceneTrans = sceneTrans w . translate ( 2, -1) }
    d4  = modify $ \w ->  w { frameTrans = id 
                            , sceneTrans = id }


flashMoveView :: String -> Event
flashMoveView = flashMessage (-100)

moves :: Position -> [(Input, GameMachine)]
moves p = [(Key "a", enterMaybeEncount' (flashMoveView " <- ") $ turnLeft p)
          ,(Key "d", enterMaybeEncount' (flashMoveView " -> ") $ turnRight p)
          ,(Key "w", goStraight p walkForward)
          ,(Key "k", goStraight p kickForward)
          ,(Key "c", openCamp p)
          ,(Key "q", suspend p)
          ,(Key "i", inspect p)
          ,(Key "s", with [modify (\w -> w { statusOn = not $ statusOn w })] (select None $ moves p))
          ,(Key "o", with [modify (\w -> w { guideOn  = not $ guideOn  w })] (select None $ moves p))
          ,(Key "m", with [nextMiniMap] (select None $ moves p))
          ,(Key " "   , select None $ moves p)
          ,(Key "\ESC", select None $ moves p)
          ,(Clock     , select None $ moves p)
          ]
  where
    goStraight p f = GameAuto $ do
        w <- world
        modify $ \w -> w { globalTime = globalTime w + 1 }
        lab <- mazeAt $ z p
        case f lab p of
          Nothing -> run $ ouch p
          Just p' -> do 
            -- update milwa effect.
            setLightValueWith True  (\n -> n - 1)
            setLightValueWith False (\n -> n - 1)
            -- update party status.
            ps <- party <$> world
            forM_ ps (`updateCharacterWith` whenWalking)
            sortPartyAuto
            -- TODO!:if all character dead, move to gameover.
            run $ enterMaybeEncount (flashMoveView " 1  ") p'


nextMiniMap :: GameState ()
nextMiniMap = do
    o  <- minimapType . worldOption <$> world
    os <- enableMinimapType <$> option
    let nm = if length os <= 1 then o
                               else next o os os
    modify (\w -> w { worldOption = (worldOption w) { minimapType = nm } })
  where
    next a org []  = head org
    next a org [n] = head org
    next a org (n:ns) | n == a = head ns
                      | n /= a = next a org ns

-- =======================================================================

suspend :: Position -> GameMachine
suspend p = GameAuto $ do
    modify $ \w -> w { inMazeMember = inMazeMember w ++ ((,p) <$> party w), party = [] }
    toCastle <- home
    returnToCastle >> run toCastle

inspect :: Position -> GameMachine
inspect p = GameAuto $ do
    movePlace (Camping p "Inspect")
    run $ selectWhenEsc (message "^S)earch character\n^I)nspect surround\n^L)eave `[`E`S`C`]")
          [(Key "l", enterWithoutEncount None p, True)
          ,(Key "s", searchCharacter p, True)
          ,(Key "i", searchSurroundings p, True)
          ]

searchSurroundings :: Position -> GameMachine
searchSurroundings p = GameAuto $ do
    eventMap <- asks eventInspect
    let foundNothing = events (searchMsgs ++ [message "You found nothing."]) (inspect p)
    case Map.lookup p eventMap of
        Nothing -> run foundNothing
        Just eid -> do
            evDef <- asks ((Map.! eid) . mazeEvents)
            let returnToInspect _ = inspect p
                cantSpelling _ _  = inspect p
            run $ events searchMsgs (doEvent evDef returnToInspect returnToInspect cantSpelling)

searchCharacter :: Position -> GameMachine
searchCharacter p = GameAuto $ do
    cs <- gets inMazeMember
    let ts = fst <$> filter ((== coordOf p) . (coordOf . snd)) cs
    run $ events searchMsgs (editParty p 0 ts)

searchMsgs = [ messageTime 400 " Searching.    " Nothing
             , messageTime 400 " Searching..   " Nothing
             , messageTime 400 " Searching...  " Nothing
             , messageTime 400 " Searching.... " Nothing
             , messageTime 400 " Searching....." Nothing
             ]

editParty :: Position -> Int -> [CharacterID] -> GameMachine
editParty p page ts = let mxPage = max 0 ((length ts - 1) `div` 10) in
    if      page < 0      then editParty p mxPage ts
    else if page > mxPage then editParty p 0 ts
    else GameAuto $ do
      let ts' = if null ts then [] else take 10 $ drop (page*10) ts 
      ns <- zipWith (++) (('^':) . (++")") <$> ms) <$> mapM (fmap (Chara.toText 28) <$> characterByID) ts'
      let cmdRemoves = cmdNums 6 (removeFromParty p ts page)
          cmdAdds = zip (Key <$> (fmap toLower <$> ms))
                        ((\cid -> addToParty p cid ts page) <$> ts')
          msg = if null ns then "\n No body found." else "You found ...\n\n" ++ unlines ns
      run $ selectEsc (message $ msg ++ "\n\n" ++
            "\n======================(" ++ show (page+1) ++ "/" ++ show (mxPage+1) ++ ")======================\n\n" ++
            "^A~)Add to party  ^#)Remove from party \n" ++ 
            "^N)ext list  ^P)revious list  ^L)eave `[`E`S`C`]\n"
            )
          ([(Key "l", enterWithoutEncount None p)
           ,(Key "n", editParty p (page + 1) ts)
           ,(Key "p", editParty p (page - 1) ts)
           ] ++ cmdRemoves ++ cmdAdds)
  where
    ms = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J"]

addToParty :: Position -> CharacterID -> [CharacterID] -> Int -> GameMachine
addToParty p cid ts page = GameAuto $ do
    ps <- gets party
    if length ps >= 6 then run $ editParty p page ts
    else do
      modify $ \w -> w {
          party = party w ++ [cid]
        , inMazeMember = filter ((/= cid) . fst) (inMazeMember w)
        }
      run $ editParty p page (filter (/= cid) ts)

removeFromParty :: Position -> [CharacterID] -> Int -> Int -> GameMachine
removeFromParty p ts page n = GameAuto $ do
    ps <- gets party
    if length ps < n || length ps == 1 then run $ editParty p page ts
    else do
      let cid = ps !! (n - 1)
      modify $ \w -> w {
          party = take (n - 1) ps ++ drop n ps
        , inMazeMember = inMazeMember w ++ [(cid, p)]
        }
      run $ editParty p page (ts ++ [cid])

-- =======================================================================

encountEnemy :: EnemyID -> Bool -> GameMachine
encountEnemy id isRB = startBattle id isRB (with [updateRoomVisit, whenEndBattle]      (escapeEvent None False)
                                           ,with [when isRB backfoward, whenEndBattle] (escapeEvent None False))


whenEndBattle :: GameState ()
whenEndBattle = do
    ps <- party <$> world
    forM_ ps (`updateCharacterWith` whenBattleEnd)

updateRoomVisit :: GameState ()
updateRoomVisit = do
    c  <- coordOf <$> currentPosition
    rd <- asks roomDefine
    let cas = fromMaybe [c] $ find (elem c) rd
    modify $ \w -> w { roomBattled = nub $ cas ++ roomBattled w }

-- =======================================================================

openCamp :: Position -> GameMachine
openCamp p = GameAuto $ do
    movePlace (Camping p "")
    np <- length . party <$> world
    run $ selectWhenEsc (message "^#)Inspect\n^R)eorder Party\n^L)eave Camp `[`E`S`C`]")
          [(Key "l", enterWithoutEncount None p, True)
          ,(Key "1", inspectCharacter (openCamp p) True F1, np >= 1)
          ,(Key "2", inspectCharacter (openCamp p) True F2, np >= 2)
          ,(Key "3", inspectCharacter (openCamp p) True F3, np >= 3)
          ,(Key "4", inspectCharacter (openCamp p) True B4, np >= 4)
          ,(Key "5", inspectCharacter (openCamp p) True B5, np >= 5)
          ,(Key "6", inspectCharacter (openCamp p) True B6, np >= 6)
          ]

endEvent :: Event -> Bool -> GameMachine
endEvent ev isHidden = GameAuto $ do
    p <- currentPosition
    run $ if isHidden then enterWithoutEncount ev   p
                      else enterWithoutEncount None p

escapeEvent :: Event -> Bool -> GameMachine
escapeEvent ev isHidden = GameAuto $ do
    p <- currentPosition
    run $ if isHidden then enterGrid Nothing False ev p
                      else enterGrid Nothing False None p

backfoward :: GameState ()
backfoward = sortPartyAuto >> ((movePlace . InMaze) . moveBack =<< currentPosition)


-- =======================================================================





