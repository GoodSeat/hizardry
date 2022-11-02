module Engine.InCastle
where

import Engine.GameAuto
import Engine.Utils
import Engine.InEdgeOfTown
import Data.World
import Data.Primitive
import qualified Data.Characters as Character

inCastle :: GameMachine
inCastle = GameAuto $ do
    movePlace InCastle
    notnull <- not . null . party <$> world
    run $ selectWhen msg [(Key "g", inGilgamesh'sTarvern, True)
                         ,(Key "a", inAdventure'sInn, notnull)
                         ,(Key "e", inEdgeOfTown, True)]
  where
    msg = Message $ "G)ilgamesh's Tarvern\n"
                 ++ "A)dventure's Inn\n"
                 ++ "B)oltac's Trading Post\n"
                 ++ "T)emple of Cant\n"
                 ++ "E)dge of Town\n"

-- =======================================================================

inGilgamesh'sTarvern :: GameMachine
inGilgamesh'sTarvern = GameAuto $ do
    movePlace Gilgamesh'sTarvern
    np <- length . party <$> world
    run $ selectWhen msg
            [(Key "l", inCastle, True)
            ,(Key "1", inspectCharacter inGilgamesh'sTarvern False F1, np >= 1)
            ,(Key "2", inspectCharacter inGilgamesh'sTarvern False F2, np >= 2)
            ,(Key "3", inspectCharacter inGilgamesh'sTarvern False F3, np >= 3)
            ,(Key "4", inspectCharacter inGilgamesh'sTarvern False B4, np >= 4)
            ,(Key "5", inspectCharacter inGilgamesh'sTarvern False B5, np >= 5)
            ,(Key "6", inspectCharacter inGilgamesh'sTarvern False B6, np >= 6)
            ,(Key "a", selectCharacterAddToParty, np < 6)]
  where
    msg = Message $ "A)dd\n"
                 ++ "R)emove\n"
                 ++ "#)Inspect\n"
                 ++ "D)ivvy Gold\n"
                 ++ "L)eave\n"

selectCharacterAddToParty :: GameMachine
selectCharacterAddToParty = GameAuto $ do
    ids <- inTarvernMember <$> world
    cs  <- sequence $ characterOf <$> ids
    let msg = "#)Add to Party    L)eave\n\n"
            ++ unlines (toShow <$> zip [1..] cs)
    let lst = [(Key "l", inGilgamesh'sTarvern, True)
              ,(Key "1", addParty (ids !! 0), length ids >= 1)
              ,(Key "2", addParty (ids !! 1), length ids >= 2)
              ,(Key "3", addParty (ids !! 2), length ids >= 3)
              ,(Key "4", addParty (ids !! 3), length ids >= 4)
              ,(Key "5", addParty (ids !! 4), length ids >= 5)
              ,(Key "6", addParty (ids !! 5), length ids >= 6)
              ,(Key "7", addParty (ids !! 6), length ids >= 7)
              ,(Key "8", addParty (ids !! 7), length ids >= 8)
              ,(Key "9", addParty (ids !! 8), length ids >= 9)]
    if null ids then run inGilgamesh'sTarvern
                else run $ selectWhen (Message msg) lst
  where
    addParty id = GameAuto $ toParty id >> run selectCharacterAddToParty
    toShow (n, c) = show n ++ ") " ++ Character.name c

-- =======================================================================

inAdventure'sInn :: GameMachine
inAdventure'sInn = GameAuto $ do
    movePlace Adventure'sInn
    ids <- party <$> world
    run $ selectWhen msg
            [(Key "l", inCastle, True) 
            ,(Key "1", selectStayPlan (ids !! 0), length ids >= 1)
            ,(Key "2", selectStayPlan (ids !! 1), length ids >= 2)
            ,(Key "3", selectStayPlan (ids !! 2), length ids >= 3)
            ,(Key "4", selectStayPlan (ids !! 3), length ids >= 4)
            ,(Key "5", selectStayPlan (ids !! 4), length ids >= 5)
            ,(Key "6", selectStayPlan (ids !! 5), length ids >= 6)]
  where
    msg = Message $ "Who will stay?\n\n"
                 ++ "#)Select\n"
                 ++ "L)eave\n"

selectStayPlan :: CharacterID -> GameMachine
selectStayPlan id = GameAuto $ do
    c <- characterOf id
    let nam = Character.name c
        gp  = Character.gold c
        msg = Message $ "Welcome " ++ nam ++ ". You have " ++ show gp ++ " GP.\n\n"
                     ++ "We have:\n"
                     ++ "A)The Stables        (FREE)\n"
                     ++ "B)A Cot               10 GP/Week\n"
                     ++ "C)Economy Rooms       50 GP/Week\n"
                     ++ "D)Merchant Suites    200 GP/Week\n"
                     ++ "E)The Royal Suite    500 GP/Week\n\n"
                     ++ "P)ool Gold\n"
                     ++ "L)eave\n"
        lst = [(Key "l", inAdventure'sInn)
              ,(Key "p", GameAuto $ poolGold id >> run (selectStayPlan id))
              ,(Key "a", sleep id  0   0 1)
              ,(Key "b", sleep id  1  10 7)
              ,(Key "c", sleep id  3  50 7)
              ,(Key "d", sleep id  7 200 7)
              ,(Key "e", sleep id 10 500 7)]
    run $ select msg lst

sleep :: CharacterID
      -> Int         -- heal hp per week.
      -> Int         -- charge per week.
      -> Int         -- pass days per week.
      -> GameMachine
sleep id h g d = GameAuto $ do
    c <- characterOf id 
    if Character.gold c < g then
      run $ events [Message "not money."] $ selectStayPlan id
    else do
      updateCharacterWith id Character.healMp
      run $ select (MessageTime (-1000) ( Character.name c
                                       ++ " is napping. \n\n"
                                       ++ show (Character.name c) ++ " has "
                                       ++ show (Character.gold c) ++ " G.P.\n\n"
                                       ++ "W)ake up"
                                        ) Nothing)
                   [(Key "w", checkLvup id)
                   ,(Clock  , next)]
  where
    next = GameAuto $ updateCharacterWith id (Character.healHp h . Character.useGold g . Character.addDay d)
                   >> run (sleep id h g $ if d == 1 then 0 else d)

checkLvup :: CharacterID -> GameMachine
checkLvup id = GameAuto $ do
    c <- characterOf id
    let nextLvExp = neps !! (Character.lv c - 1)
        nextLvMsg = "You need " ++ show (nextLvExp - Character.exp c) ++ 
                    " more E.P.\nto make the next level."
    if Character.exp c >= nextLvExp
      then run $ doLvup id
      else run $ events [Message nextLvMsg] (selectStayPlan id)
  where
    neps = [1100, 3500, 5000] -- TODO

doLvup :: CharacterID -> GameMachine
doLvup id = GameAuto $ do
    (txt, c') <- Character.lvup <$> characterOf id
    updateCharacter id c' >> run (events [Message txt] $ selectStayPlan id)

-- =======================================================================

