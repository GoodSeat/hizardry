module Engine.InCastle (inCastle) where

import PreludeL
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
    msg <- switchL (EnJp msgENG msgJPN)
    run $ selectWhenEsc msg [(Key "e", inEdgeOfTown, True)
                            ,(Key "g", inGilgamesh'sTavern, True)
                            ,(Key "a", inAdventure'sInn, notnull)
                            ,(Key "b", inBoltac'sTradingPost, notnull)
                            ,(Key "t", inTempleOfCant, notnull)
                            ]
  where
    msgENG = message $ "^G)ilgamesh's Tavern\n"
                    ++ "^A)dventure's Inn\n"
                    ++ "^B)oltac's Trading Post\n"
                    ++ "^T)emple of Cant\n"
                    ++ "^E)dge of Town `[`E`S`C`]\n"
    msgJPN = message $ "^G)ギルガメッシュの酒場\n"
                    ++ "^A)冒険者の宿\n"
                    ++ "^B)ボルタック商店\n"
                    ++ "^T)カント寺院\n"
                    ++ "^E)町外れ `[`E`S`C`]\n"

-- =======================================================================

inGilgamesh'sTavern :: GameMachine
inGilgamesh'sTavern = GameAuto $ do
    movePlace Gilgamesh'sTavern
    np  <- length . party <$> world
    msg <- switchL (EnJp msgENG msgJPN)
    cmdsInspect <- cmdNumPartiesWhen $ bimap (inspectCharacter inGilgamesh'sTavern False) (const True)
    run $ selectWhenEsc msg $ (Key "l", inCastle, True)
                            : (Key "a", selectCharacterAddToParty 0, np < 6)
                            : (Key "r", selectCharacterRemoveFromParty, np > 0)
                            : (Key "d", with [divvyGold] inGilgamesh'sTavern, np > 0)
                            : cmdsInspect
  where
    msgENG = message $ "^A)dd Character to Party\n"
                    ++ "^R)emove Character from Party\n"
                    ++ "^#)Inspect Character\n"
                    ++ "^D)ivvy Gold\n"
                    ++ "^L)eave `[`E`S`C`]\n"
    msgJPN = message $ "^A)パーティに加える\n"
                    ++ "^R)パーティから外す\n"
                    ++ "^#)調べる\n"
                    ++ "^D)所持金を分配\n"
                    ++ "^L)酒場を出る `[`E`S`C`]\n"

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
      txt <- switchL $ EnJp "^#)Add to Party  ^N)ext list  ^P)revious list  ^L)eave `[`E`S`C`]\n\n"
                            "^#)加える  ^N)次のリスト  ^P)前のリスト  ^L)離れる `[`E`S`C`]\n\n"
      let msg = txt ++ unlines (toShow <$> zip [1..] cs)
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
      txt <- switchL $ EnJp "^#)Remove from Party    ^L)eave `[`E`S`C`]"
                            "^#)パーティから外す    ^L)離れる `[`E`S`C`]"
      run $ selectEsc (message txt) $ (Key "l", inGilgamesh'sTavern) : cmds
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
    msg  <- switchL (EnJp msgENG msgJPN)
    cmds <- cmdNumPartiesID $ \(_, i) -> GameAuto $ do
        c <- characterByID i
        run $ if mustGotoTemple c then inAdventure'sInn else selectStayPlan i
    run $ selectEsc msg $ (Key "l", inCastle) : cmds
  where
    msgENG = message $ "Who will stay?\n\n"
                    ++ "^#)Select\n"
                    ++ "^L)eave `[`E`S`C`]\n"
    msgJPN = message $ "誰が泊まりますか ?\n\n"
                    ++ "^#)選択\n"
                    ++ "^L)宿を出る `[`E`S`C`]\n"

selectStayPlan :: CharacterID -> GameMachine
selectStayPlan id = GameAuto $ do
    c <- characterByID id
    let nam = Character.name c
        gp  = Character.gold c
        msgENG = message $ "Welcome " ++ nam ++ ". You have " ++ show gp ++ " G.P.\n\n"
                        ++ "We have:\n\n"
                        ++ "^A)The Stables        (FREE)\n"
                        ++ "^B)A Cot               10 G.P/Week\n"
                        ++ "^C)Economy Rooms       50 G.P/Week\n"
                        ++ "^D)Merchant Suites    200 G.P/Week\n"
                        ++ "^E)The Royal Suite    500 G.P/Week\n\n"
                        ++ "^P)ool Gold\n"
                        ++ "^L)eave `[`E`S`C`]\n"
        msgJPN = message $ "ようこそ " ++ nam ++ " さん。 所持金 : " ++ show gp ++ " G.P.\n\n"
                        ++ "どこに泊まりますか。\n\n"
                        ++ "^A)馬小屋             (FREE)\n"
                        ++ "^B)簡易寝台            10 G.P/Week\n"
                        ++ "^C)エコノミールーム    50 G.P/Week\n"
                        ++ "^D)スイートルーム     200 G.P/Week\n"
                        ++ "^E)ロイヤルスイート   500 G.P/Week\n\n"
                        ++ "^P)集金する\n"
                        ++ "^L)離れる `[`E`S`C`]\n"
        lst = [(Key "l", inAdventure'sInn)
              ,(Key "p", with [poolGoldTo id] (selectStayPlan id))
              ,(Key "a", sleep id  0   0 1 False)
              ,(Key "b", sleep id  1  10 7 False)
              ,(Key "c", sleep id  3  50 7 False)
              ,(Key "d", sleep id  7 200 7 False)
              ,(Key "e", sleep id 10 500 7 False)]
    msg <- switchL (EnJp msgENG msgJPN)
    run $ selectEsc msg lst

sleep :: CharacterID
      -> Int         -- ^ heal hp per week.
      -> Int         -- ^ charge per week.
      -> Int         -- ^ pass days per week.
      -> Bool        -- ^ birthday?
      -> GameMachine
sleep id h g d birthday = GameAuto $ do
    c <- characterByID id 
    let nam = Character.name c
    autoHeal <- (== CureWhenInn) <$> (hpHealType . worldOption <$> world)
    txt1 <- switchL (EnJp "Out of gold." "もうお金がありませんよ。") 
    if Character.gold c < g then
      run $ events [message txt1] $ selectStayPlan id
    else do
      updateCharacterWith id Character.healMp
      when autoHeal $ updateCharacterWith id (Character.healHp $ Character.maxhp c)
      txt2 <- switchL (EnJp
             (" is napping. \n\n" ++ nam ++ " has " ++ show (Character.gold c) ++ " G.P.\n\n^W)ake up `[`E`S`C`]")
             (" は寝ています。 \n\n  所持金 : " ++ show (Character.gold c) ++ " G.P.\n\n^W)起きる `[`E`S`C`]"))
      txt3 <- switchL (EnJp ("Happy Birthday, " ++ nam ++ "!") (nam ++ " さん 誕生日おめでとう !"))
      run $ selectEsc (messageTime (-1000) (nam ++ txt2) Nothing)
                      [(Key "w", events [Resume (changeFlash txt3) | birthday]
                                        (checkLvup id))
                      ,(Clock, next (Character.age c - if birthday then 1 else 0))]
  where
    next ageO = GameAuto $ do
                   updateCharacterWith id (Character.healHp h . Character.useGold g . Character.addDay d)
                   ageN <- Character.age <$> characterByID id
                   run (sleep id h g (if d == 1 then 0 else d) (ageO /= ageN))

checkLvup :: CharacterID -> GameMachine
checkLvup id = GameAuto $ do
    c <- characterByID id
    let nextLvExp = Character.totalExpToLv (Character.job c) (Character.lv c + 1)
        need      = show (nextLvExp - Character.exp c)
    nextLvMsg <- switchL (EnJp ("You need " ++ need ++ " more E.P.\nto make the next level.")
                            ("次のレベルまで、あと\n  " ++ need ++ " の経験値が必要です。"))
    if Character.exp c >= nextLvExp
      then run $ doLvup id
      else run $ events [message nextLvMsg] (selectStayPlan id)

doLvup :: CharacterID -> GameMachine
doLvup id = GameAuto $ do
    (txt, c') <- lvup =<< characterByID id
    run $ with [updateCharacter id c'] (events [withBGM LevelUp $ message txt] $ selectStayPlan id)

-- =======================================================================

inBoltac'sTradingPost :: GameMachine
inBoltac'sTradingPost = GameAuto $ do
    movePlace Boltac'sTradingPost
    msg  <- switchL (EnJp msgENG msgJPN)
    cmds <- cmdNumPartiesID $ \(_, i) -> GameAuto $ do
        c <- characterByID i
        run $ if mustGotoTemple c then inBoltac'sTradingPost else selectShopAction i
    run $ selectEsc msg $ (Key "l", inCastle) : cmds
  where
    msgENG = message $ "Who will enter?\n\n"
                    ++ "^#)Select\n"
                    ++ "^L)eave `[`E`S`C`]\n"
    msgJPN = message $ "誰が入りますか ?\n\n"
                    ++ "^#)選択\n"
                    ++ "^L)離れる `[`E`S`C`]\n"

selectShopAction :: CharacterID -> GameMachine
selectShopAction id = GameAuto $ do
    c <- characterByID id
    let nam = Character.name c
        gp  = Character.gold c
        msgENG = message $ "Welcome " ++ nam ++ ". You have " ++ show gp ++ " G.P.\n\n"
                        ++ "We have:\n"
                        ++ "^B)uy\n"
                        ++ "^S)ell\n"
                        ++ "^I)dentify Items\n"
                        ++ "^U)ncurse\n\n"
                        ++ "^P)ool Gold\n"
                        ++ "^L)eave `[`E`S`C`]\n"
        msgJPN = message $ "ようこそ " ++ nam ++ "さん。 所持金 : " ++ show gp ++ " G.P.\n\n"
                        ++ "今日は何用ですか ?\n"
                        ++ "^B)購入\n"
                        ++ "^S)売却\n"
                        ++ "^I)鑑定\n"
                        ++ "^U)解呪\n\n"
                        ++ "^P)集金する\n"
                        ++ "^L)離れる `[`E`S`C`]\n"
        lst = [(Key "l", inBoltac'sTradingPost)
              ,(Key "p", with [poolGoldTo id] (selectShopAction id))
              ,(Key "b", buyItem id 0 True)
              ,(Key "s", sellItem id)
              ,(Key "i", determineItem id)
              ,(Key "u", uncurseItem id)
              ]
    msg <- switchL (EnJp msgENG msgJPN)
    run $ selectEsc msg lst

sizePage :: Int
sizePage = 9

lastPage :: GameState Int
lastPage = flip div sizePage . flip (-) 1 . length . filter ((/= 0) . snd) . Map.toList . shopItems <$> world

buyItem :: CharacterID -> Int -> Bool -> GameMachine
buyItem cid (-1) bi = GameAuto $ do 
    mxPage <- lastPage
    run $ buyItem cid mxPage bi
buyItem cid page bi = GameAuto $ do
    c <- characterByID cid
    l <- language . worldOption <$> world
    lstItem <- fmap fst . filter ((/= 0) . snd) . sortOn fst . Map.toList . shopItems <$> world
    let lstItem' = take sizePage . drop (page * sizePage) $ lstItem
    if      null lstItem  then run $ selectShopAction cid
    else if null lstItem' then run $ buyItem cid 0 bi
    else do
      mxPage <- lastPage
      gp     <- Character.gold <$> characterByID cid
      defs   <- mapM itemByID lstItem'
      txt1   <- switchL (EnJp ("Select item to buy. You have " ++ show gp ++ " G.P.\n\n" ++ 
                               "^N)ext list  ^P)revious list  ^?)Inspect  ^L)eave `[`E`s`c`]")
                              ("どれを購入しますか。 所持金 : " ++ show gp ++ " G.P.\n\n" ++ 
                               "^N)次のリスト  ^P)前のリスト  ^?)商品の確認  ^L)離れる `[`E`s`c`]"))
      txt2   <- switchL (EnJp ("Select item to inspect. You have " ++ show gp ++ " G.P.\n\n" ++ 
                               "^N)ext list  ^P)revious list  ^?)Buy  ^L)eave `[`E`s`c`]")
                              ("どれを確認しますか。 所持金 : " ++ show gp ++ " G.P.\n\n" ++ 
                               "^N)次のリスト  ^P)前のリスト  ^?)商品の購入  ^L)離れる `[`E`s`c`]"))
      let canMs  = (\def -> if Character.canEquip c def || Character.canUse' c def then "  " else " #") <$> defs
      let items0 = zipWith (++) (takeChar 43 . (++ repeat ' ') . (switchL' l . Item.name) <$> defs)
                               (rightTxt 10 . Item.valueInShop <$> defs)
          items  = zipWith (++) items0 canMs
          lst  = "\n=========================(" ++ show (page+1) ++ "/" ++ show (mxPage+1) ++ ")========================\n\n"
               ++ unlines (zipWith (++) ((++") ") . show <$> [1..]) items) ++ "\n"
          txt  = (if bi then txt1 else txt2) ++ lst
          msg  = message txt 
           
          cmds = cmdNums (length lstItem')
               $ if bi then buy cid (buyItem cid page bi) . (lstItem' !!) . flip (-) 1
                       else \n -> let (ifm, _) = Item.itemInformation $ defs !! (n - 1)
                                  in events [Resume (changeFlash $ switchL' l ifm)] (buyItem cid page bi)
      run $ selectEsc msg $ (Key "l", selectShopAction cid)
                          : (Key "n", buyItem cid (page + 1) bi)
                          : (Key "p", buyItem cid (page - 1) bi)
                          : (Key "\16128", buyItem cid page (not bi))
                          : (Key "?", buyItem cid page (not bi))
                          : cmds

buy :: CharacterID -> GameMachine -> ItemID -> GameMachine
buy cid next idItem = GameAuto $ do
    w   <- world
    def <- itemByID idItem
    c   <- characterByID cid
    let canTreat = Character.canEquip c def || Character.canUse' c def
    v   <- Item.valueInShop <$> itemByID idItem
    is  <- Character.items <$> characterByID cid
    g   <- Character.gold  <$> characterByID cid
    txt1 <- switchL (EnJp "You can't carry any more items." "それ以上持てませんよ。")
    txt2 <- switchL (EnJp "You don't have enough gold."     "お金が足りませんよ。")
    if length is >= 10 then run $ events [toMsg txt1] next
    else if v > g      then run $ events [toMsg txt2] next
    else do
      txt3 <- switchL (EnJp "This is the last one."    "これは最後の一つですよ。")
      txt4 <- switchL (EnJp "I'm sure you'll love it." "きっとお気に召しますよ。")
      txt5 <- switchL (EnJp "You cannot use this item. Do you still want to buy it?\n\n^Y)es  ^N)o"
                            "あなたには使えないものですが、それでも買いますか ?\n\n^Y)はい  ^N)いいえ")
      txt6 <- switchL (EnJp "Don't worry about it - mistakes happen to everyone."
                            "間違いは誰にでもありますよ。")
      let map   = shopItems w
          pair  = Map.lookup idItem map
          n'    = case pair of Nothing -> undefined
                               Just n  -> n - 1
          map'  = if n' == 0 then Map.delete idItem map
                             else Map.insert idItem n' map
          msg   = if n' == 0 then txt3 else txt4
          is'   = is ++ [ItemInf idItem True]
          restG = g - v
      if not canTreat then
        run $ select (Resume (changeFlash txt5)) [ (Key "y", doBuy msg restG is' map')
                                                 , (Key "n", events [toMsg txt6] next)]
      else run $ doBuy msg restG is' map'
  where
    toMsg m = Resume (changeFlashTime m (-1500))

    doBuy :: String -> Int -> [ItemInf] -> Map.Map ItemID Int -> GameMachine
    doBuy msg restGP is mapShop = GameAuto $ do
      w <- world
      put $ w { shopItems = mapShop }
      updateCharacterWith cid $ \c -> c { Character.items = is
                                        , Character.gold  = restGP }
      run $ events [toMsg msg] next

-----

toSellGreet :: CharacterID -> GameState String
toSellGreet cid = do
    is <- Character.items <$> characterByID cid
    gp <- Character.gold <$> characterByID cid
    ns <- mapM sellName is
    vs <- mapM sellValue is
    txt1 <- switchL (EnJp ("Select item to sell. You have " ++ show gp ++ " G.P.\n\n^L)eave `[`E`s`c`]\n")
                          ("売りたいものは何ですか。 所持金 : " ++ show gp ++ " G.P.\n\n^L)離れる `[`E`s`c`]\n"))
    let items = zipWith (++) (takeChar 43 . (++ repeat ' ') <$> ns) (rightTxt 10 <$> vs)
        ps    = toEnum <$> take (length items) [0..]
        lst   = "=========================================================\n\n"
              ++ unlines (zipWith (++) ((++") ") . Character.itemPosToText <$> ps) items) ++ "\n"
    return $ txt1 ++ lst

toSellMessage :: CharacterID -> String -> GameState Event
toSellMessage cid msg = do
    greet <- toSellGreet cid
    return $ flashAndMessageTime (-1500) greet msg Nothing

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

    msg <- if pos `elem` Character.equipPoss c then switchL (EnJp "You can't sell what you equip." "身に着けているものは売れませんよ。")
           else if can'tSell then switchL (EnJp "Sorry, but we can't buy this item." "申し訳ございません、それは買い取りかねます。")
           else if v <= 0    then switchL (EnJp "It has no value." "それは無価値なので買い取れません。")
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
             switchL (EnJp "Thank you so much." "ありがとうございます。")
    ev <- toSellMessage cid msg
    run $ events [ev] (sellItem cid)

-----

toDetermineGreet :: CharacterID -> GameState String
toDetermineGreet cid = do
    is <- Character.items <$> characterByID cid
    gp <- Character.gold <$> characterByID cid
    ns <- mapM sellName is
    vs <- mapM determineValueTxt is
    txt1 <- switchL (EnJp ("Select item to identify. You have " ++ show gp ++ " G.P.\n\n^L)eave `[`E`s`c`]\n")
                          ("鑑定してほしいのはどれですか。 所持金 : " ++ show gp ++ " G.P.\n\n^L)離れる `[`E`s`c`]\n"))
    let items = zipWith (++) (takeChar 43 . (++ repeat ' ') <$> ns) (rightString 10 <$> vs)
        ps    = toEnum <$> take (length items) [0..]
        lst   = "=========================================================\n\n"
              ++ unlines (zipWith (++) (("^"++) . (++") ") . Character.itemPosToText <$> ps) items) ++ "\n"
    return $ txt1 ++ lst

toDetermineMessage :: CharacterID -> String -> GameState Event
toDetermineMessage cid msg = do
    greet <- toDetermineGreet cid
    return $ flashAndMessageTime (-1500) greet msg Nothing

determineItem :: CharacterID -> GameMachine
determineItem cid = GameAuto $ do
    is <- Character.items <$> characterByID cid
    let ps = toEnum <$> take (length is) [0..]
    greet <- toDetermineGreet cid 
    run $ selectEsc (message greet)
        $ (Key "l", selectShopAction cid)
        : fmap (\pos -> (Key (toLower <$> Character.itemPosToText pos), determine cid pos)) ps

sellName :: ItemInf -> GameState String
sellName (ItemInf id determined) = switchL . (if determined then Item.name else Item.nameUndetermined) =<< itemByID id

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
    msg <- if      v > gp          then switchL (EnJp "You don't have enough gold." "お金が足りませんよ。")
           else if identified item then switchL (EnJp "You already know what it is."        "それが何であるかご存じですよね。")
           else do
             let i'  = item { identified = True }
                 is' = take n is ++ [i'] ++ drop (n + 1) is
                 gp' = gp - v
             updateCharacter cid $ c { Character.items = is', Character.gold = gp' }
             switchL (EnJp "Identified." "鑑定できました。")
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

    txt1 <- switchL (EnJp "You have no cursed items equipped." "あなたは呪われたものを身に着けていませんよ。") 
    txt2 <- switchL (EnJp ("Select item to remove curse. You have " ++ show gp ++ " G.P.\n\n^L)eave `[`E`s`c`]\n")
                          ("お祓いをしてほしいものはどれですか。 所持金 : " ++ show gp ++ " G.P.\n\n^L)離れる `[`E`s`c`]\n"))

    if null cursedEquippedItems then run $ events [message txt1] (selectShopAction cid)
    else do
      displayData <- mapM (\(pos, inf) -> do
            name  <- switchL . (if identified inf then Item.name else Item.nameUndetermined) =<< itemByID (itemID inf)
            value <- Item.valueInShop <$> itemByID (itemID inf)
            return (pos, name, value)) cursedEquippedItems
      let itemsText = map (\(pos, name, value) ->
            Character.itemPosToText pos ++ ") " ++ takeChar 43 (name ++ repeat ' ') ++ rightTxt 10 value) displayData
      let lst = "=========================================================\n\n" ++ unlines itemsText ++ "\n"
          greet = txt2 ++ lst
          toMsg = flip (flashAndMessageTime (-1500) greet) Nothing

      run $ selectEsc (message greet)
          $ (Key "l", selectShopAction cid)
          : fmap (\(pos, _, val) -> (Key (toLower <$> Character.itemPosToText pos), doUncurse cid pos val toMsg)) displayData

doUncurse :: CharacterID -> Character.ItemPos -> Int -> (String -> Event) -> GameMachine
doUncurse cid pos val toMsg = GameAuto $ do
    c <- characterByID cid
    txt1 <- switchL (EnJp "You don't have enough gold." "お金が足りませんよ。")
    txt2 <- switchL (EnJp "The curse is broken."        "お祓いできました。")
    if Character.gold c < val then
      run $ events [toMsg txt1] (uncurseItem cid)
    else do
      let itemInfToRemove = Character.itemInfAt c pos
      let c' = c { Character.gold   = Character.gold c - val
                 , Character.items  = filter (/= itemInfToRemove) (Character.items c)
                 , Character.equips = filter (/= itemInfToRemove) (Character.equips c)
                 }
      updateCharacter cid c'
      run $ events [toMsg txt2] (selectShopAction cid)


-- =======================================================================

inTempleOfCant :: GameMachine
inTempleOfCant = GameAuto $ do
    movePlace TempleOfCant
    ids <- filterM (fmap mustGotoTemple . characterByID) . inTavernMember =<< world
    txt1 <- switchL (EnJp "Nobody in the tavern needs a cure." "救いを必要とする者はいません。")
    msg  <- switchL (EnJp msgENG msgJPN)
    if null ids then
      run (events [flashAndMessageTime (-2000) msg txt1 Nothing] inCastle)
    else do
      cmds <- cmdNumPartiesID $ \(_, i) -> GameAuto $ do 
          c <- characterByID i
          run $ if mustGotoTemple c then inTempleOfCant
                                    else selectCureTarget i 0
      run $ selectEsc (message msg) $ (Key "l", inCastle) : cmds
  where
    msgENG = "Who will enter?\n\n"
          ++ "^#)Select\n"
          ++ "^L)eave `[`E`S`C`]\n"
    msgJPN = "誰が入りますか ?\n\n"
          ++ "^#)選択\n"
          ++ "^L)寺院を出る `[`E`S`C`]\n"

selectCureTarget :: CharacterID -> Int -> GameMachine
selectCureTarget id page = GameAuto $ do
    ids <- filterM (fmap mustGotoTemple . characterByID) . inTavernMember =<< world
    txt1 <- switchL (EnJp ("Who do you want to help?\n"
                        ++ "^#)Select  ^N)ext list  ^P)revious list  ^L)eave `[`E`S`C`]\n\n")
                          ("誰を救いたいのだ ?\n"
                        ++ "^#)選択  ^N)次のリスト  ^P)前のリスト  ^L)離れる `[`E`S`C`]\n\n"))
    if null ids then run inCastle
    else if page /= 0 && page * 9 >= length ids then run $ selectCureTarget id 0
    else if page < 0 then run $ selectCureTarget id ((length ids - 1) `div` 9)
    else do
      let ids' = take 9 . drop (page * 9) $ ids
      cs  <- mapM characterByID ids'
      let msg = message $ txt1 ++ unlines (toShow <$> zip [1..] cs)
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
        msgENG = message $ "Welcome " ++ nam ++ ". You have " ++ show gp ++ " G.P.\n\n"
                        ++ "The prayer fee is " ++ show fee ++ " G.P.  ...OK?\n"
                        ++ "  ^Y)es\n"
                        ++ "  ^P)ool Gold\n"
                        ++ "  ^L)eave `[`E`S`C`]\n"
        msgJPN = message $ "ようこそ " ++ nam ++ " 。 所持金 : " ++ show gp ++ " G.P.\n\n"
                        ++ "祈禱料は " ++ show fee ++ " G.P. です。  ...よろしいですね ?\n"
                        ++ "  ^Y)支払う\n"
                        ++ "  ^P)集金する\n"
                        ++ "  ^L)やめる `[`E`S`C`]\n"
    msg  <- switchL (EnJp msgENG msgJPN)
    txt1 <- switchL (EnJp "  Get out, you penniless beggars!  " "  出ていけ ! ケチな背信者どもめ !  ")
    canSpent <- canSpentGold cid fee
    let lst =[(Key "l", selectCureTarget cid 0)
             ,(Key "p", with [poolGoldTo cid] (cureCharacter cid cidDst))
             ,(Key "y", if canSpent then with [spentGold cid fee] $ tryCureCharacter cid cidDst
                                    else events [Resume (changeFlashTime txt1 (-1000))] $ selectCureTarget cid 0)]
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

        let mgENG | succeed   = nam ++ " has recovered !!"
                  | isAsh     = nam ++ " is lost forever!"
                  | isDead    = nam ++ " was reduced to ashes!"
                  | otherwise = "There is no change in " ++ nam ++ "'s condition..."
            msENG  = flashMessage 1000 <$> [" MURMUR                          "
                                           ," MURMUR - CHANT                  "
                                           ," MURMUR - CHANT - PRAY           "
                                           ," MURMUR - CHANT - PRAY - INVOKE! "]
            mgENG' = " MURMUR - CHANT - PRAY - INVOKE! \n\n   " ++ mgENG
        let mgJPN | succeed   = nam ++ " は元気になった !!"
                  | isAsh     = nam ++ " は失われた..."
                  | isDead    = nam ++ " は灰と化した..."
                  | otherwise = nam ++ " の容態に変化はなかった..."
            msJPN  = flashMessage 1000 <$> [" 囁き                          "
                                           ," 囁き - 詠唱                   "
                                           ," 囁き - 詠唱 - 祈り            "
                                           ," 囁き - 詠唱 - 祈り - 念じろ ! "]
            mgJPN' = " 囁き - 詠唱 - 祈り - 念じろ ! \n\n   " ++ mgJPN
        ms  <- switchL (EnJp msENG msJPN)
        mg' <- switchL (EnJp mgENG' mgJPN')
        run $ events (ms ++ [flashMessageInf mg']) $ selectCureTarget cid 0
        )

