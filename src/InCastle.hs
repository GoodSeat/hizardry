module InCastle
where

import Control.Monad.Trans.State
import GameAuto
import World
import Characters


inCastle :: GameAuto World Input Event
inCastle = GameAuto $ do
    movePlace InCastle
    let msg = Message $ "G)ilgamesh's Tarvern\n" ++
                        "A)dventure's Inn\n" ++
                        "B)oltac's Trading Post\n" ++
                        "T)emple of Cant\n" ++
                        "E)dge of Town\n"
    notnull <- not . null . party <$> get
    selectWhen msg [(Key "g", inGilgamesh'sTarvern, True)
                   ,(Key "e", edgeOfTown, True)
                   ,(Key "a", inAdventure'sInn, notnull)]

-- =======================================================================

inGilgamesh'sTarvern :: GameAuto World Input Event
inGilgamesh'sTarvern = GameAuto $ movePlace (Gilgamesh'sTarvern Nothing) >>
    select (Message $ "A)dd\n" ++ 
                      "R)emove\n" ++
                      "#)Inspect\n" ++
                      "D)ivvy Gold\n" ++
                      "L)eave\n")
            [(Key "l", inCastle)
            ,(Key "a", selectCharacterAddToParty)]

selectCharacterAddToParty :: GameAuto World Input Event
selectCharacterAddToParty = GameAuto $ do
    cs <- inTarvernMember <$> get
    let msg = show cs
    let lst = [(Key "l", inGilgamesh'sTarvern, True)
              ,(Key "1", addParty (cs !! 0), length cs >= 1)
              ,(Key "2", addParty (cs !! 1), length cs >= 2)
              ,(Key "3", addParty (cs !! 2), length cs >= 3)
              ,(Key "4", addParty (cs !! 3), length cs >= 4)
              ,(Key "5", addParty (cs !! 4), length cs >= 5)
              ,(Key "6", addParty (cs !! 5), length cs >= 6)
              ,(Key "7", addParty (cs !! 6), length cs >= 7)
              ,(Key "8", addParty (cs !! 7), length cs >= 8)
              ,(Key "9", addParty (cs !! 8), length cs >= 9)]
    if null cs then run inGilgamesh'sTarvern
               else selectWhen (Message msg) lst
  where
    addParty c = GameAuto $ toParty c >> run selectCharacterAddToParty


-- =======================================================================

inAdventure'sInn :: GameAuto World Input Event
inAdventure'sInn = GameAuto $ do
    movePlace Adventure'sInn
    ps <- party <$> get
    let lst = [(Key "l", inCastle, True) 
              ,(Key "1", selectStayPlan (ps !! 0), length ps >= 1)
              ,(Key "2", selectStayPlan (ps !! 1), length ps >= 2)
              ,(Key "3", selectStayPlan (ps !! 2), length ps >= 3)
              ,(Key "4", selectStayPlan (ps !! 3), length ps >= 4)
              ,(Key "5", selectStayPlan (ps !! 4), length ps >= 5)
              ,(Key "6", selectStayPlan (ps !! 5), length ps >= 6)]
        msg = (Message $ "Who will stay?\n" ++ 
                         "#)Select\n" ++
                         "L)eave\n")
    selectWhen msg lst

selectStayPlan :: Character -> GameAuto World Input Event
selectStayPlan c = GameAuto $ do
    let msg = (Message $ "Where do you stay?\n" ++
                         "H)orse House               Free!!\n" ++
                         "N)ormal Room       10 Gold / Week\n" ++
                         "S)uite Room        50 Gold / Week\n" ++
                         "D)elax Suite      200 Gold / Week\n" ++
                         "?)Suite           500 Gold / Week\n\n" ++
                         "L)eave\n")
        lst = [(Key "l", inAdventure'sInn)
              ,(Key "h", sleep c  0   0)
              ,(Key "n", sleep c  1  10)
              ,(Key "s", sleep c  3  50)
              ,(Key "d", sleep c  7 200)
              ,(Key "?", sleep c 10 500)]
    select msg lst

sleep :: Character
      -> Int         -- heal hp per week.
      -> Int         -- charge per week.
      -> GameAuto World Input Event
sleep c heal gp =
  if gold c < gp then
    events [Message "not money."] $ selectStayPlan c
  else
    selectNext (Message $ name c ++ " is sleeping... \n\nlast gold is " ++ show (gold c) ++ " gold.\n\nL)eave")
               [(Key "l", checkLvup c)
               ,(Clock, next) 
               ,(Key "n", next)]
  where
    next = GameAuto $ do
        addAge <- randomNext 1 52
        let c' = (healHp heal c) {
              gold = gold c - gp
            , age  = age c + if addAge == 1 then 1 else 0 }
        updateCharacter c' >> run (sleep c' heal gp)

checkLvup :: Character -> GameAuto World Input Event
checkLvup c = if Characters.exp c >= nextLvExp then doLvup c else selectStayPlan c
  where
    nextLvExp = neps !! (lv c - 1)
    neps = [1100, 3500, 5000]

doLvup :: Character -> GameAuto World Input Event
doLvup c = GameAuto $ updateCharacter c' >> run (events [Message txt] $ selectStayPlan c')
  where
    (txt, c') = lvup c

-- =======================================================================

edgeOfTown :: GameAuto World Input Event
edgeOfTown = events [Message "Sorry...", Message "Not implmented."] inCastle

