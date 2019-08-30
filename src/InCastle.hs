module InCastle
where

import GameAuto
import World
import qualified Characters as Character
import Utils
import InEdgeOfTown

inCastle :: GameAuto
inCastle = Auto $ do
    movePlace InCastle
    let msg = Message $ "G)ilgamesh's Tarvern\n" ++
                        "A)dventure's Inn\n" ++
                        "B)oltac's Trading Post\n" ++
                        "T)emple of Cant\n" ++
                        "E)dge of Town\n"
    notnull <- not . null . party <$> world
    selectWhen msg [(Key "g", inGilgamesh'sTarvern, True)
                   ,(Key "a", inAdventure'sInn, notnull)
                   ,(Key "e", inEdgeOfTown, True)]

-- =======================================================================

inGilgamesh'sTarvern :: GameAuto
inGilgamesh'sTarvern = Auto $ movePlace Gilgamesh'sTarvern >>
    select (Message $ "A)dd\n" ++ 
                      "R)emove\n" ++
                      "#)Inspect\n" ++
                      "D)ivvy Gold\n" ++
                      "L)eave\n")
            [(Key "l", inCastle)
            ,(Key "a", selectCharacterAddToParty)]

selectCharacterAddToParty :: GameAuto
selectCharacterAddToParty = Auto $ do
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
                else selectWhen (Message msg) lst
  where
    addParty id = Auto $ toParty id >> run selectCharacterAddToParty
    toShow (n, c) = show n ++ ") " ++ Character.name c


-- =======================================================================

inAdventure'sInn :: GameAuto
inAdventure'sInn = Auto $ do
    movePlace Adventure'sInn
    ids <- party <$> world
    let msg = Message $ "Who will stay?\n" ++ 
                        "#)Select\n" ++
                        "L)eave\n"
        lst = [(Key "l", inCastle, True) 
              ,(Key "1", selectStayPlan (ids !! 0), length ids >= 1)
              ,(Key "2", selectStayPlan (ids !! 1), length ids >= 2)
              ,(Key "3", selectStayPlan (ids !! 2), length ids >= 3)
              ,(Key "4", selectStayPlan (ids !! 3), length ids >= 4)
              ,(Key "5", selectStayPlan (ids !! 4), length ids >= 5)
              ,(Key "6", selectStayPlan (ids !! 5), length ids >= 6)]
    selectWhen msg lst

selectStayPlan :: Character.ID -> GameAuto
selectStayPlan id = Auto $ do
    c <- characterOf id
    let nam = Character.name c
        gp  = Character.gold c
    let msg = Message $ "Welcome " ++ nam ++ ". You have " ++ show gp ++ " GP.\n\n" ++
                        "We have:\n" ++
                        "A)The Stables        (FREE)\n" ++
                        "B)A Cot               10 GP/Week\n" ++
                        "C)Economy Rooms       50 GP/Week\n" ++
                        "D)Merchant Suites    200 GP/Week\n" ++
                        "E)The Royal Suite    500 GP/Week\n\n" ++
                        "P)ool Gold\n" ++
                        "L)eave\n"
        lst = [(Key "l", inAdventure'sInn)
              ,(Key "p", Auto $ poolGold id >> (run $ selectStayPlan id))
              ,(Key "a", sleep id  0   0)
              ,(Key "b", sleep id  1  10)
              ,(Key "c", sleep id  3  50)
              ,(Key "d", sleep id  7 200)
              ,(Key "e", sleep id 10 500)]
    select msg lst

sleep :: Character.ID
      -> Int         -- heal hp per week.
      -> Int         -- charge per week.
      -> GameAuto
sleep id h g = Auto $ do
    c <- characterOf id 
    if Character.gold c < g then
      run $ events [Message "not money."] $ selectStayPlan id
    else
      run $ selectNext (Message $  Character.name c
                                ++ " is sleeping... \n\nlast gold is "
                                ++ show (Character.gold c)
                                ++ " gold.\n\nW)ake up")
                 [(Key "w", checkLvup id)
                 ,(Clock, next) 
                 ,(Key "n", next)]
  where
    next = Auto $  updateCharacterWith id (Character.healHp h . Character.useGold g)
                >> run (sleep id h g)

checkLvup :: Character.ID -> GameAuto
checkLvup id = Auto $ do
    c <- characterOf id
    let nextLvExp = neps !! (Character.lv c - 1)
        nextLvMsg = "You need " ++ show (nextLvExp - Character.exp c) ++ 
                    " more E.P.\nto make the next level."
    if Character.exp c >= nextLvExp
        then run $ doLvup id
        else run $ events [Message nextLvMsg] $ selectStayPlan id
  where
    neps = [1100, 3500, 5000]

doLvup :: Character.ID -> GameAuto
doLvup id = Auto $ do
    (txt, c') <- Character.lvup <$> characterOf id
    updateCharacter id c' >> run (events [Message txt] $ selectStayPlan id)

-- =======================================================================

