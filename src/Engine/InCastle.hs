module Engine.InCastle (inCastle) where

import PreludeL
import Prelude hiding ((!!))
import Control.Monad (when)
import Control.Monad.State (put, filterM, modify)
import qualified Data.Map as Map

import Engine.GameAuto
import Engine.Utils
import Engine.InEdgeOfTown
import Engine.CharacterAction (inspectCharacter)
import Data.World
import Data.Primitive
import Data.Bifunctor (bimap)
import Data.List (sort, sortOn, find, intersperse)
import Data.Maybe (catMaybes)
import Data.Formula
import Data.Char (toLower)
import qualified Data.Characters as Character
import qualified Data.Items as Item

inCastle :: GameMachine
inCastle = with [movePlace InCastle] $ autoSaveToSlot0 $ GameAuto $ do
    notnull <- not . null . party <$> world
    run $ selectWhenEsc msg [(Key "e", inEdgeOfTown, True)
                            ,(Key "g", inGilgamesh'sTavern, True)
                            ,(Key "a", inAdventure'sInn, notnull)
                            ,(Key "b", inBoltac'sTradingPost, notnull)
                            ,(Key "t", inTempleOfCant, notnull)
                            ]
  where
    msg = message $ "^G)ilgamesh's Tavern\n"
                 ++ "^A)dventure's Inn\n"
                 ++ "^B)oltac's Trading Post\n"
                 ++ "^T)emple of Cant\n"
                 ++ "^E)dge of Town `[`E`S`C`]\n"

-- =======================================================================

inGilgamesh'sTavern :: GameMachine
inGilgamesh'sTavern = GameAuto $ do
    movePlace Gilgamesh'sTavern
    np <- length . party <$> world
    cmdsInspect <- cmdNumPartiesWhen $ bimap (inspectCharacter inGilgamesh'sTavern False) (const True)
    run $ selectWhenEsc msg $ (Key "l", inCastle, True)
                            : (Key "a", selectCharacterAddToParty 0, np < 6)
                            : (Key "r", selectCharacterRemoveFromParty, np > 0)
                            : (Key "d", with [divvyGold] inGilgamesh'sTavern, np > 0)
                            : cmdsInspect
  where
    msg = message $ "^A)dd Character to Party\n"
                 ++ "^R)emove Character from Party\n"
                 ++ "^#)Inspect Character\n"
                 ++ "^D)ivvy Gold\n"
                 ++ "^L)eave `[`E`S`C`]\n"

selectCharacterAddToParty :: Int -> GameMachine
selectCharacterAddToParty page = GameAuto $ do
    ps <- mapM characterByID . party =<< world
    np <- length . party <$> world
    ignoreA <- ignoreAlignment . worldOption <$> world
    let existG = Character.G `elem` (Character.alignment <$> ps)
        existE = Character.E `elem` (Character.alignment <$> ps)
        baseEn c = np /= 0 || not (mustGotoTemple c)
        canAdd c | existG    = baseEn c && (ignoreA || Character.alignment c /= Character.E)
                 | existE    = baseEn c && (ignoreA || Character.alignment c /= Character.G)
                 | otherwise = baseEn c
    ids <- inTavernMember <$> world
    if page /= 0 && page * 9 >= length ids then run $ selectCharacterAddToParty 0
    else if page < 0 then run $ selectCharacterAddToParty ((length ids - 1) `div` 9)
    else do
      let ids' = take 9 . drop (page * 9) $ ids
          toShow (n, c) = if canAdd c then show n ++ ") " ++ Character.toText 33 c
                                      else "   `" ++ intersperse '`' (Character.toText 33 c)
      cs  <- mapM characterByID ids'
      let msg = "^#)Add to Party  ^N)ext list  ^P)revious list  ^L)eave `[`E`S`C`]\n\n"
              ++ unlines (toShow <$> zip [1..] cs)
      let lst = (Key "l", inGilgamesh'sTavern)
              : (Key "n", selectCharacterAddToParty $ page + 1)
              : (Key "p", selectCharacterAddToParty $ page - 1)
              : cmdNums (length cs) (\i -> if canAdd (cs !! (i - 1)) then addParty (ids' !! (i - 1))
                                                                     else selectCharacterAddToParty page)
      run $ if np >= 6 || null ids then inGilgamesh'sTavern else selectEsc (message msg) lst
  where
    addParty id = with [addCharacterToParty id] (selectCharacterAddToParty page)

selectCharacterRemoveFromParty :: GameMachine
selectCharacterRemoveFromParty = GameAuto $ do
    cs <- party <$> world
    if null cs then run inGilgamesh'sTavern
    else do
      cmds <- cmdNumPartiesID $ \(_, cid) -> removeParty cid
      run $ selectEsc (message "^#)Remove from Party    ^L)eave `[`E`S`C`]") $
                      (Key "l", inGilgamesh'sTavern) : cmds
  where
    removeParty cid = GameAuto $ do
      w <- world
      put $ w { party          = filter (/= cid) (party w)
              , inTavernMember = sort $ cid : inTavernMember w }
      run selectCharacterRemoveFromParty

-- =======================================================================

inAdventure'sInn :: GameMachine
inAdventure'sInn = GameAuto $ do
    movePlace Adventure'sInn
    cmds <- cmdNumPartiesID $ \(_, i) -> GameAuto $ do
        c <- characterByID i
        run $ if mustGotoTemple c then inAdventure'sInn else selectStayPlan i
    run $ selectEsc msg $ (Key "l", inCastle) : cmds
  where
    msg = message $ "Who will stay?\n\n"
                 ++ "^#)Select\n"
                 ++ "^L)eave `[`E`S`C`]\n"

selectStayPlan :: CharacterID -> GameMachine
selectStayPlan id = GameAuto $ do
    c <- characterByID id
    let nam = Character.name c
        gp  = Character.gold c
        msg = message $ "Welcome " ++ nam ++ ". You have " ++ show gp ++ " G.P.\n\n"
                     ++ "We have:\n\n"
                     ++ "^A)The Stables        (FREE)\n"
                     ++ "^B)A Cot               10 G.P/Week\n"
                     ++ "^C)Economy Rooms       50 G.P/Week\n"
                     ++ "^D)Merchant Suites    200 G.P/Week\n"
                     ++ "^E)The Royal Suite    500 G.P/Week\n\n"
                     ++ "^P)ool Gold\n"
                     ++ "^L)eave `[`E`S`C`]\n"
        lst = [(Key "l", inAdventure'sInn)
              ,(Key "p", with [poolGoldTo id] (selectStayPlan id))
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
    c <- characterByID id 
    if Character.gold c < g then
      run $ events [message "no more money."] $ selectStayPlan id
    else do
      updateCharacterWith id Character.healMp
      run $ selectEsc (messageTime (-1000) ( Character.name c
                                       ++ " is napping. \n\n"
                                       ++ show (Character.name c) ++ " has "
                                       ++ show (Character.gold c) ++ " G.P.\n\n"
                                       ++ "^W)ake up `[`E`S`C`]"
                                        ) Nothing)
                      [(Key "w", checkLvup id), (Clock, next)]
  where
    next = GameAuto $ updateCharacterWith id (Character.healHp h . Character.useGold g . Character.addDay d)
                   >> run (sleep id h g $ if d == 1 then 0 else d)

checkLvup :: CharacterID -> GameMachine
checkLvup id = GameAuto $ do
    c <- characterByID id
    let nextLvExp = Character.totalExpToLv (Character.job c) (Character.lv c + 1)
        nextLvMsg = "You need " ++ show (nextLvExp - Character.exp c) ++ 
                    " more E.P.\nto make the next level."
    if Character.exp c >= nextLvExp
      then run $ doLvup id
      else run $ events [message nextLvMsg] (selectStayPlan id)

doLvup :: CharacterID -> GameMachine
doLvup id = GameAuto $ do
    (txt, c') <- lvup =<< characterByID id
    run $ with [updateCharacter id c'] (events [message txt] $ selectStayPlan id)

-- =======================================================================

inBoltac'sTradingPost :: GameMachine
inBoltac'sTradingPost = GameAuto $ do
    movePlace Boltac'sTradingPost
    cmds <- cmdNumPartiesID $ \(_, i) -> GameAuto $ do
        c <- characterByID i
        run $ if mustGotoTemple c then inBoltac'sTradingPost else selectShopAction i
    run $ selectEsc msg $ (Key "l", inCastle) : cmds
  where
    msg = message $ "Who will enter?\n\n"
                 ++ "^#)Select\n"
                 ++ "^L)eave `[`E`S`C`]\n"

selectShopAction :: CharacterID -> GameMachine
selectShopAction id = GameAuto $ do
    c <- characterByID id
    let nam = Character.name c
        gp  = Character.gold c
        msg = message $ "Welcome " ++ nam ++ ". You have " ++ show gp ++ " G.P.\n\n"
                     ++ "We have:\n"
                     ++ "^B)uy\n"
                     ++ "^S)ell\n"
                     ++ "^I)dentify Items\n"
                     ++ "^U)ncurse\n"
                     ++ "^P)ool Gold\n"
                     ++ "^L)eave `[`E`S`C`]\n"
        lst = [(Key "l", inBoltac'sTradingPost)
              ,(Key "p", with [poolGoldTo id] (selectShopAction id))
              ,(Key "b", buyItem id 0)
              ,(Key "s", sellItem id)
              ,(Key "i", determineItem id)
              ,(Key "u", uncurseItem id)
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
      gp     <- Character.gold <$> characterByID cid
      defs   <- mapM itemByID lstItem'
      let items = zipWith (++) (takeChar 43 . (++ repeat ' ') . Item.name <$> defs)
                               (rightTxt 10 . Item.valueInShop <$> defs)
          lst  = "\n=========================(" ++ show (page+1) ++ "/" ++ show (mxPage+1) ++ ")========================\n\n"
               ++ unlines (zipWith (++) ((++") ") . show <$> [1..]) items) ++ "\n"
          txt  = "Select item to buy. You have " ++ show gp ++ " G.P.\n\n"
              ++ "^N)ext list  ^P)revious list  ^?)Inspect  ^L)eave [Esc]" ++ lst
          msg  = message txt 
          cmds = cmdNums (length lstItem')
               $ buy cid (buyItem cid page) (flip (flashAndMessageTime (-1500) txt) Nothing . (++ "  \n  ") . ("\n  " ++)) . (lstItem' !!) . flip (-) 1
      run $ selectEsc msg $ (Key "l", selectShopAction cid)
                          : (Key "n", buyItem cid (page + 1))
                          : (Key "p", buyItem cid (page - 1))
--                        : (Key "?", infoItem cid page)  -- TODO
                          : cmds

buy :: CharacterID -> GameMachine -> (String -> Event) -> ItemID -> GameMachine
buy cid next toMsg idItem = GameAuto $ do
    w  <- world
    v  <- Item.valueInShop <$> itemByID idItem
    is <- Character.items <$> characterByID cid
    g  <- Character.gold  <$> characterByID cid
    if length is >= 10 then run $ events [toMsg "You can't have any more item."] next
    else if v > g then run $ events [toMsg "You don't have enough gold."] next
    else do
      let map  = shopItems w
          pair = Map.lookup idItem map
          n'   = case pair of Nothing -> undefined
                              Just n  -> n - 1
          map' = if n' == 0 then Map.delete idItem map
                            else Map.insert idItem n' map
          msg  = if n' == 0 then "It is last one." else "you must favorite in it."
      put $ w { shopItems = map' }
      updateCharacterWith cid $ \c -> c { Character.items = is ++ [ItemInf idItem True]
                                        , Character.gold  = g - v }
      run $ events [toMsg msg] next
   -- TODO:you can't equip this. OK? -> no people take no mistake.

-----

toSellGreet :: CharacterID -> GameState String
toSellGreet cid = do
    is <- Character.items <$> characterByID cid
    gp <- Character.gold <$> characterByID cid
    ns <- mapM sellName is
    vs <- mapM sellValue is
    let items = zipWith (++) (takeChar 43 . (++ repeat ' ') <$> ns) (rightTxt 10 <$> vs)
        ps    = toEnum <$> take (length items) [0..]
        lst   = "=========================================================\n\n"
              ++ unlines (zipWith (++) ((++") ") . Character.itemPosToText <$> ps) items) ++ "\n"
    return $ "Select item to sell. You have " ++ show gp ++ " G.P.\n\n^L)eave `[`E`s`c`]\n" ++ lst

toSellMessage :: CharacterID -> String -> GameState Event
toSellMessage cid msg = do
    greet <- toSellGreet cid
    return $ flashAndMessageTime (-1500) greet ("\n  " ++ msg ++ "  \n  ") Nothing

sellItem :: CharacterID -> GameMachine
sellItem cid = GameAuto $ do
    is <- Character.items <$> characterByID cid
    if null is then run $ selectShopAction cid
    else do
      greet <- toSellGreet cid
      let pis = toEnum <$> [0..(length is)]
      run $ selectEsc (message greet)
          $ (Key "l", selectShopAction cid)
          : fmap (\pos -> (Key (toLower <$> Character.itemPosToText pos), sell cid pos)) pis
  where
    sellName :: ItemInf -> GameState String
    sellName (ItemInf id determined) = (if determined then Item.name else Item.nameUndetermined) <$> itemByID id

sellValue :: ItemInf -> GameState Int
sellValue (ItemInf _ False) = return 0
sellValue (ItemInf id True) = flip div 2 . Item.valueInShop <$> itemByID id

sell :: CharacterID -> Character.ItemPos -> GameMachine
sell cid pos = GameAuto $ do
    c <- characterByID cid
    let idItem = Character.itemAt c pos
        is = Character.items c
        gp = Character.gold c
        n  = fromEnum pos
    idef <- itemByID (itemID (is !! n))
    let can'tSell = Item.CantDrop `elem` Item.attributes idef
    v <- sellValue $ is !! n

    msg <- if pos `elem` Character.equipPoss c then return "You can't sell what you equip."
           else if can'tSell then return "Sorry, but we can't this item."
           else if v <= 0    then return "It is no value."
           else do
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
             return "Thank you so much."
    ev <- toSellMessage cid msg
    run $ events [ev] (sellItem cid)

-----

toDetermineGreet :: CharacterID -> GameState String
toDetermineGreet cid = do
    is <- Character.items <$> characterByID cid
    gp <- Character.gold <$> characterByID cid
    ns <- mapM sellName is
    vs <- mapM determineValueTxt is
    let items = zipWith (++) (takeChar 43 . (++ repeat ' ') <$> ns) (rightString 10 <$> vs)
        ps    = toEnum <$> take (length items) [0..]
        lst   = "=========================================================\n\n"
              ++ unlines (zipWith (++) (("^"++) . (++") ") . Character.itemPosToText <$> ps) items) ++ "\n"
    return $ "Select item to determine. You have " ++ show gp ++ " G.P.\n\n^L)eave `[`E`s`c`]\n" ++ lst

toDetermineMessage :: CharacterID -> String -> GameState Event
toDetermineMessage cid msg = do
    greet <- toDetermineGreet cid
    return $ flashAndMessageTime (-1500) greet ("\n  " ++ msg ++ "  \n  ") Nothing

determineItem :: CharacterID -> GameMachine
determineItem cid = GameAuto $ do
    is <- Character.items <$> characterByID cid
    let ps = toEnum <$> take (length is) [0..]
    greet <- toDetermineGreet cid 
    run $ selectEsc (message greet)
        $ (Key "l", selectShopAction cid)
        : fmap (\pos -> (Key (toLower <$> Character.itemPosToText pos), determine cid pos)) ps

sellName :: ItemInf -> GameState String
sellName (ItemInf id determined) = (if determined then Item.name else Item.nameUndetermined) <$> itemByID id

determineValueTxt :: ItemInf -> GameState String
determineValueTxt (ItemInf _ True)  = return "---"
determineValueTxt (ItemInf i False) = show <$> sellValue (ItemInf i True)

determineValue :: ItemInf -> GameState Int
determineValue (ItemInf i b) = sellValue (ItemInf i $ not b)

determine :: CharacterID -> Character.ItemPos -> GameMachine
determine cid pos = GameAuto $ do
    c <- characterByID cid
    let is   = Character.items c
        gp   = Character.gold c
        n    = fromEnum pos
        item = is !! n
    v <- determineValue $ is !! n
    msg <- if      v > gp          then return "You don't have enough gold."
           else if identified item then return "You already know it."
           else do
             let i'  = item { identified = True }
                 is' = take n is ++ [i'] ++ drop (n + 1) is
                 gp' = gp - v
             updateCharacter cid $ c { Character.items = is', Character.gold = gp' }
             return "Determined."
    ev <- toDetermineMessage cid msg
    run $ events [ev] (determineItem cid)

-----

uncurseItem :: CharacterID -> GameMachine
uncurseItem cid = GameAuto $ do
    c <- characterByID cid
    let gp = Character.gold c
    let equippedItemInfs = map (\p -> (p, Character.itemInfAt c p)) (Character.equipPoss c)

    cursedEquippedItems <- flip filterM equippedItemInfs $ \(_, inf) -> do
        def <- itemByID (itemID inf)
        return $ Item.Cursed `elem` Item.attributes def

    if null cursedEquippedItems then run $ events [message "You have no cursed items equipped."] (selectShopAction cid)
    else do
      displayData <- mapM (\(pos, inf) -> do
            name  <- (if identified inf then Item.name else Item.nameUndetermined) <$> itemByID (itemID inf)
            value <- Item.valueInShop <$> itemByID (itemID inf)
            return (pos, name, value)) cursedEquippedItems
      let itemsText = map (\(pos, name, value) ->
            Character.itemPosToText pos ++ ") " ++ takeChar 43 (name ++ repeat ' ') ++ rightTxt 10 value) displayData
      let lst = "=========================================================\n\n" ++ unlines itemsText ++ "\n"
          greet = "Select item to uncurse. You have " ++ show gp ++ " G.P.\n\n^L)eave `[`E`s`c`]\n" ++ lst
          toMsg = flip (flashAndMessageTime (-1500) greet) Nothing . (++ "  \n  ") . ("\n  " ++)

      run $ selectEsc (message greet)
          $ (Key "l", selectShopAction cid)
          : fmap (\(pos, _, val) -> (Key (toLower <$> Character.itemPosToText pos), doUncurse cid pos val toMsg)) displayData

doUncurse :: CharacterID -> Character.ItemPos -> Int -> (String -> Event) -> GameMachine
doUncurse cid pos val toMsg = GameAuto $ do
    c <- characterByID cid
    if Character.gold c < val then
      run $ events [toMsg "You don't have enough gold."] (uncurseItem cid)
    else do
      let itemInfToRemove = Character.itemInfAt c pos
      let c' = c { Character.gold   = Character.gold c - val
                 , Character.items  = filter (/= itemInfToRemove) (Character.items c)
                 , Character.equips = filter (/= itemInfToRemove) (Character.equips c)
                 }
      updateCharacter cid c'
      run $ events [toMsg "The curse is broken."] (selectShopAction cid)


-- =======================================================================

inTempleOfCant :: GameMachine
inTempleOfCant = GameAuto $ do
    movePlace TempleOfCant
    ids <- filterM (fmap mustGotoTemple . characterByID) . inTavernMember =<< world
    if null ids then
      run (events [flashAndMessageTime (-2000) msg "\n  No body in tavern needs cure.  \n " Nothing] inCastle)
    else do
      cmds <- cmdNumPartiesID $ \(_, i) -> GameAuto $ do 
          c <- characterByID i
          run $ if mustGotoTemple c then inTempleOfCant
                                    else selectCureTarget i 0
      run $ selectEsc (message msg) $ (Key "l", inCastle) : cmds
  where
    msg = "Who will enter?\n\n"
       ++ "^#)Select\n"
       ++ "^L)eave `[`E`S`C`]\n"

selectCureTarget :: CharacterID -> Int -> GameMachine
selectCureTarget id page = GameAuto $ do
    ids <- filterM (fmap mustGotoTemple . characterByID) . inTavernMember =<< world
    if null ids then run inCastle
    else if page /= 0 && page * 9 >= length ids then run $ selectCureTarget id 0
    else if page < 0 then run $ selectCureTarget id ((length ids - 1) `div` 9)
    else do
      let ids' = take 9 . drop (page * 9) $ ids
      cs  <- mapM characterByID ids'
      let msg = message $ "Who do you want to help?\n"
                       ++ "^#)Select  ^N)ext list  ^P)revious list  ^L)eave `[`E`S`C`]\n\n"
                       ++ unlines (toShow <$> zip [1..] cs)
          lst = (Key "l", inTempleOfCant)
              : (Key "n", selectCureTarget id $ page + 1)
              : (Key "p", selectCureTarget id $ page - 1)
              : cmdNums (length cs) (\i -> cureCharacter id (ids' !! (i - 1)))
      run $ selectEsc msg lst
  where
    toShow (n, c) = show n ++ ") " ++ Character.name c

cureCharacter :: CharacterID -> CharacterID -> GameMachine
cureCharacter cid cidDst = GameAuto $ do
    c   <- characterByID cid
    cd  <- characterByID cidDst
    let nam = Character.name c
        gp  = Character.gold c
        ss  = statusErrorsOf cd
        lv  = lvOf cd
        fee | Ash    `elem` ss                        = 500 * lv
            | Dead   `elem` ss                        = 250 * lv
            | Stoned `elem` ss && Paralysis `elem` ss = 250 * lv
            | Stoned `elem` ss                        = 200 * lv
            | otherwise                               = 100 * lv
        msg = message $ "Welcome " ++ nam ++ ". You have " ++ show gp ++ " G.P.\n\n"
                     ++ "The prayer fee is " ++ show fee ++ " G.P.  ...OK?\n"
                     ++ "  ^Y)es\n"
                     ++ "  ^P)ool Gold\n"
                     ++ "  ^L)eave `[`E`S`C`]\n"
    canSpent <- canSpentGold cid fee
    let lst =[(Key "l", selectCureTarget cid 0)
             ,(Key "p", with [poolGoldTo cid] (cureCharacter cid cidDst))
             ,(Key "y", if canSpent then with [spentGold cid fee] $ tryCureCharacter cid cidDst
                                    else events [flashMessage (-1000) "\n     Get out! You cheap traitor!     \n "] $ selectCureTarget cid 0)]
    run $ selectEsc msg lst

tryCureCharacter :: CharacterID -> CharacterID -> GameMachine
tryCureCharacter cid cidDst = GameAuto $ do
    c   <- characterByID cid
    cd  <- characterByID cidDst
    let nam = Character.name cd
        ss  = statusErrorsOf cd
        isAsh  = Ash  `elem` ss
        isDead = Dead `elem` ss
    let prob | isAsh     = "50-age+3*vit"
             | isDead    = "60-age+3*vit"
             | otherwise = "100"
    run $ parse'D prob "success probability?" (\f -> GameAuto $ do
        m       <- formulaMapC c
        succeed <- happens =<< evalWith m f
        when succeed $ updateCharacterWith cidDst $ \c -> c { Character.statusErrors = [] 
                                                            , Character.hp  = if Character.hp c == 0 then Character.maxhp c   else Character.hp c
                                                            , Character.age = if Character.hp c == 0 then Character.age c + 1 else Character.age c
                                                            }
        when (not succeed && isDead) $ updateCharacterWith cidDst $ \c -> c { Character.statusErrors = [Ash] }
        when (not succeed && isAsh ) $ do
            updateCharacterWith cidDst (\c -> c { Character.statusErrors = [Lost] })
            modify (\w -> w { inTavernMember = filter (/= cidDst) $ inTavernMember w })

        let mg | succeed   = nam ++ " has recovered !!"
               | isAsh     = nam ++ " is lost..."
               | isDead    = nam ++ " reduced to ashes..."
               | otherwise = "no change in " ++ nam ++ "'s condition..."
        let ms  = flashMessage 1000 <$> [" MURMUR                          "
                                        ," MURMUR - CHANT                  "
                                        ," MURMUR - CHANT - PRAY           "
                                        ," MURMUR - CHANT - PRAY - INVOKE! "]
            mg' = " MURMUR - CHANT - PRAY - INVOKE! \n\n   " ++ mg
        run $ events (ms ++ [flashMessage (-100000) mg']) $ selectCureTarget cid 0
        )

