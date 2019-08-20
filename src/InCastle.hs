module InCastle
where

import Control.Monad.Trans.State
import GameAuto
import World


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


inAdventure'sInn :: GameAuto World Input Event
inAdventure'sInn = GameAuto $ select (Message $ "Who will stay?\n" ++ 
                                     "#)Select\n" ++
                                     "L)eave\n")
                  [(Key "l", inCastle)]


edgeOfTown :: GameAuto World Input Event
edgeOfTown = events [Message "Sorry...", Message "Not implmented."] inCastle

