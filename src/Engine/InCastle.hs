module Engine.InCastle (inCastle) where

import PreludeL
import Prelude hiding ((!!))
import Control.Monad.State (put)
import qualified Data.Map as Map

import Engine.GameAuto
import Engine.Utils
import Engine.InEdgeOfTown
import Engine.CharacterAction (inspectCharacter)
import Data.World
import Data.Primitive
import Data.Bifunctor (bimap)
import Data.List (sort, sortOn)
import qualified Data.Characters as Character
import qualified Data.Items as Item

inCastle :: GameMachine
inCastle = GameAuto $ do
    movePlace InCastle
    notnull <- not . null . party <$> world
    run $ selectWhen msg [(Key "g", inGilgamesh'sTarvern, True)
                         ,(Key "a", inAdventure'sInn, notnull)
                         ,(Key "b", inBoltac'sTradingPost, notnull)
-- TODO                  ,(Key "t", inTempleOfCant, notnull)
                         ,(Key "e", inEdgeOfTown, True)
                         ]
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
    cmdsInspect <- cmdNumPartiesWhen $ bimap (inspectCharacter inGilgamesh'sTarvern False) (const True)
    run $ selectWhenEsc msg $ (Key "l", inCastle, True)
                            : (Key "a", selectCharacterAddToParty, np < 6)
                            : (Key "r", selectCharacterRemoveFromParty, np > 0)
                            : (Key "d", GameAuto $ divvyGold >> run inGilgamesh'sTarvern, np > 0)
                            : cmdsInspect
  where
    msg = Message $ "A)dd\n"
                 ++ "R)emove\n"
                 ++ "#)Inspect\n"
                 ++ "D)ivvy Gold\n"
                 ++ "L)eave [ESC]\n"

selectCharacterAddToParty :: GameMachine
selectCharacterAddToParty = GameAuto $ do
    ids <- inTarvernMember <$> world
    cs  <- sequence $ characterOf <$> ids
    let msg = "#)Add to Party    L)eave [ESC]\n\n"
            ++ unlines (toShow <$> zip [1..] cs)
    let lst = (Key "l", inGilgamesh'sTarvern)
            : cmdNums (length ids) (\i -> addParty (ids !! (i - 1)))
    if null ids then run inGilgamesh'sTarvern
                else run $ selectEsc (Message msg) lst
  where
    addParty id = GameAuto $ toParty id >> run selectCharacterAddToParty
    toShow (n, c) = show n ++ ") " ++ Character.name c

selectCharacterRemoveFromParty :: GameMachine
selectCharacterRemoveFromParty = GameAuto $ do
    cs <- party <$> world
    if null cs then run inGilgamesh'sTarvern
    else do
      cmds <- cmdNumPartiesID $ \(_, cid) -> removeParty cid
      run $ selectEsc (Message "#)Remove from Party    L)eave [ESC]") $
                      (Key "l", inGilgamesh'sTarvern) : cmds
  where
    removeParty cid = GameAuto $ do
      w <- world
      put $ w { party           = filter (/= cid) (party w)
              , inTarvernMember = sort $ cid : inTarvernMember w }
      run selectCharacterRemoveFromParty

-- =======================================================================

inAdventure'sInn :: GameMachine
inAdventure'sInn = GameAuto $ do
    movePlace Adventure'sInn
    cmds <- cmdNumPartiesID $ \(_, i) -> selectStayPlan i
    run $ selectEsc msg $ (Key "l", inCastle) : cmds
  where
    msg = Message $ "Who will stay?\n\n"
                 ++ "#)Select\n"
                 ++ "L)eave [ESC]\n"

selectStayPlan :: CharacterID -> GameMachine
selectStayPlan id = GameAuto $ do
    c <- characterOf id
    let nam = Character.name c
        gp  = Character.gold c
        msg = Message $ "Welcome " ++ nam ++ ". You have " ++ show gp ++ " G.P.\n\n"
                     ++ "We have:\n"
                     ++ "A)The Stables        (FREE)\n"
                     ++ "B)A Cot               10 G.P/Week\n"
                     ++ "C)Economy Rooms       50 G.P/Week\n"
                     ++ "D)Merchant Suites    200 G.P/Week\n"
                     ++ "E)The Royal Suite    500 G.P/Week\n\n"
                     ++ "P)ool Gold\n"
                     ++ "L)eave [ESC]\n"
        lst = [(Key "l", inAdventure'sInn)
              ,(Key "p", GameAuto $ poolGold id >> run (selectStayPlan id))
              ,(Key "a", sleep id  0   0 1)
              ,(Key "b", sleep id  1  10 7)
              ,(Key "c", sleep id  3  50 7)
              ,(Key "d", sleep id  7 200 7)
              ,(Key "e", sleep id 10 500 7)]
    run $ selectEsc msg lst

sleep :: CharacterID
      -> Int         -- ^ heal hp per week.
      -> Int         -- ^ charge per week.
      -> Int         -- ^ pass days per week.
      -> GameMachine
sleep id h g d = GameAuto $ do
    c <- characterOf id 
    if Character.gold c < g then
      run $ events [Message "not money."] $ selectStayPlan id
    else do
      updateCharacterWith id Character.healMp
      run $ selectEsc (MessageTime (-1000) ( Character.name c
                                       ++ " is napping. \n\n"
                                       ++ show (Character.name c) ++ " has "
                                       ++ show (Character.gold c) ++ " G.P.\n\n"
                                       ++ "W)ake up [ESC]"
                                        ) Nothing)
                      [(Key "w", checkLvup id), (Clock, next)]
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
    neps = [1100, 3500, 5000, 11000, 25000, 40000, 80000, 150000]  -- TODO

doLvup :: CharacterID -> GameMachine
doLvup id = GameAuto $ do
    (txt, c') <- Character.lvup <$> characterOf id
    updateCharacter id c' >> run (events [Message txt] $ selectStayPlan id)

-- =======================================================================

inBoltac'sTradingPost :: GameMachine
inBoltac'sTradingPost = GameAuto $ do
    movePlace Boltac'sTradingPost
    cmds <- cmdNumPartiesID $ \(_, i) -> selectShopAction i
    run $ selectEsc msg $ (Key "l", inCastle) : cmds
  where
    msg = Message $ "Who will enter?\n\n"
                 ++ "#)Select\n"
                 ++ "L)eave [ESC]\n"

selectShopAction :: CharacterID -> GameMachine
selectShopAction id = GameAuto $ do
    c <- characterOf id
    let nam = Character.name c
        gp  = Character.gold c
        msg = Message $ "Welcome " ++ nam ++ ". You have " ++ show gp ++ " G.P.\n\n"
                     ++ "We have:\n"
                     ++ "B)uy\n"
                     ++ "S)ell\n"
                     ++ "I)dentify Items\n"
                     ++ "P)ool Gold\n"
                     ++ "L)eave [ESC]\n"
        lst = [(Key "l", inBoltac'sTradingPost)
              ,(Key "p", GameAuto $ poolGold id >> run (selectShopAction id))
              ,(Key "b", buyItem id 0)
              ,(Key "s", sellItem id)
-- TODO       ,(Key "i", identifyItem id)
              ]
    run $ selectEsc msg lst

sizePage :: Int
sizePage = 9

lastPage :: GameState Int
lastPage = flip div sizePage . flip (-) 1 . length . filter ((/= 0) . snd) . Map.toList . shopItems <$> world

buyItem :: CharacterID -> Int -> GameMachine
buyItem cid (-1) = GameAuto $ do 
    mxPage <- lastPage
    run $ buyItem cid mxPage
buyItem cid page = GameAuto $ do
    lstItem <- fmap fst . filter ((/= 0) . snd) . sortOn fst . Map.toList . shopItems <$> world
    let lstItem' = take sizePage . drop (page * sizePage) $ lstItem
    if      null lstItem  then run $ selectShopAction cid
    else if null lstItem' then run $ buyItem cid 0 
    else do
      mxPage <- lastPage
      gp     <- Character.gold <$> characterOf cid
      defs   <- mapM itemByID lstItem'
      let items = zipWith (++) (take 43 . (++ repeat ' ') . Item.name <$> defs)
                               (rightTxt 10 . Item.valueInShop <$> defs)
          lst  = "\n=========================(" ++ show (page+1) ++ "/" ++ show (mxPage+1) ++ ")========================\n\n"
               ++ unlines (zipWith (++) ((++") ") . show <$> [1..]) items) ++ "\n"
          cmds = cmdNums (length lstItem')
               $ buy cid (buyItem cid page) (Message . (++lst)) . (lstItem' !!) . flip (-) 1
          msg  = Message $ "Select item to buy. You have " ++ show gp ++ " G.P.\n\n"
                        ++ "N)ext list  P)revious list  L)eave [Esc]" ++ lst
      run $ selectEsc msg $ (Key "l", selectShopAction cid)
                          : (Key "n", buyItem cid (page + 1))
                          : (Key "p", buyItem cid (page - 1))
                          : cmds

buy :: CharacterID -> GameMachine -> (String -> Event) -> ItemID -> GameMachine
buy cid next toMsg idItem = GameAuto $ do
    w  <- world
    v  <- Item.valueInShop <$> itemByID idItem
    is <- Character.items <$> characterOf cid
    g  <- Character.gold  <$> characterOf cid
    if length is >= 10 then run $ events [toMsg "you can't have any more item.\n\n"] next
    else if v > g then run $ events [toMsg "you are poor.\n\n"] next
    else do
      let map  = shopItems w
          pair = Map.lookup idItem map
          n'   = case pair of Nothing -> undefined
                              Just n  -> n - 1
          map' = if n' == 0 then Map.delete idItem map
                            else Map.insert idItem n' map
          msg  = if n' == 0 then "it is last one.\n\n" else "you must favorite in it.\n\n"
      put $ w { shopItems = map' }
      updateCharacterWith cid $ \c -> c { Character.items = is ++ [ItemInf idItem True]
                                        , Character.gold  = g - v }
      run $ events [toMsg msg] next


sellItem :: CharacterID -> GameMachine
sellItem = sellItem' False

sellItem' :: Bool -> CharacterID -> GameMachine
sellItem' greet cid = GameAuto $ do
    is <- Character.items <$> characterOf cid
    if not greet && null is then run $ selectShopAction cid
    else do
      gp <- Character.gold <$> characterOf cid
      ns <- mapM sellName is
      vs <- mapM sellValue is
      let items = zipWith (++) (take 43 . (++ repeat ' ') <$> ns) (rightTxt 10 <$> vs)
          ps    = Character.numToItemPos <$> take (length items) [0..]
          lst   = "=========================================================\n\n"
                ++ unlines (zipWith (++) ((++") ") . Character.itemPosToText <$> ps) items) ++ "\n"
      if greet then run $ events [Message $ "Thank you so much.\n\n\n" ++ lst] $ sellItem cid
      else
        return (Message $ "Select item to sell. You have " ++ show gp ++ " G.P.\n\n"
                            ++ "L)eave [Esc]\n" ++ lst,
                \(Key s) -> if s == "l" || s == "\ESC" then selectShopAction cid
                            else case Character.itemPosByChar s of
                              Nothing -> sellItem cid
                              Just i  -> if i `elem` ps then GameAuto $ sell cid i >> run (sellItem' True cid)
                                         else sellItem cid)
  where
    sellName :: ItemInf -> GameState String
    sellName (ItemInf id determined) = (if determined then Item.name else Item.nameUndetermined) <$> itemByID id

sellValue :: ItemInf -> GameState Int
sellValue (ItemInf _ False) = return 0
sellValue (ItemInf id True) = flip div 2 . Item.valueInShop <$> itemByID id

sell :: CharacterID -> Character.ItemPos -> GameState ()
sell cid pos = do
    c <- characterOf cid
    let idItem = Character.itemAt c pos
        is = Character.items c
        gp = Character.gold c
        n  = Character.itemPosToNum pos
    v <- sellValue $ is !! n
    let is' = take n is ++ drop (n + 1) is
        gp' = gp + v
    updateCharacter cid $ c { Character.items = is', Character.gold = gp' }

    w <- world
    let map  = shopItems w
        cnt  = Map.lookup idItem map
        cnt' = case cnt of Nothing -> 1
                           Just cn -> cn + 1
        map' = Map.insert idItem cnt' map
    put $ w { shopItems = map' }

-- =======================================================================



