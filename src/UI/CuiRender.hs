module UI.CuiRender (
      cuiRender
    , renderWithCache
    , render
    ) where

import Control.Monad
import Data.List (isPrefixOf, intersperse)

import Control.CUI
import Engine.GameAuto
import Engine.Utils
import Data.Primitive
import Data.Characters as Character
import Data.World
import Data.Maze
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import qualified Data.Map as Map
import qualified Data.Enemies as Enemy
import qualified Data.Items as Item

-- ========================================================================

windowW = 75
windowH = 40

debugWindowH = 10

type RenderMethod = Bool    -- ^ visible of debug window.
                 -> Craphic -- ^ target craphic.
                 -> IO ()

render :: RenderMethod
render showDebugWindow = if showDebugWindow then draw (windowW + 2, windowH + 2 + (debugWindowH + 3))
                                            else draw (windowW + 2, windowH + 2)

renderWithCache :: DrawCache -> RenderMethod
renderWithCache cache showDebugWindow = if showDebugWindow then drawWithCache (windowW + 2, windowH + 2 + (debugWindowH + 3)) cache
                                                           else drawWithCache (windowW + 2, windowH + 2) cache

-- ========================================================================

cuiRender :: RenderMethod -> (Maybe PictureInf -> Craphic) -> Scenario -> DisplayIO
cuiRender rm picOf s (General            (Display m c f t p n)) w = rendering rm picOf s (toT m) (toT f) (toT c) Nothing p w
cuiRender rm picOf s (ShowStatus cid his (Display m c f t p n)) w = rendering rm picOf s (toT m) (toT f) (toT c) (Just (cid, his)) p w
cuiRender rm picOf s None                                       w = cuiRender rm picOf s (wait 0 Nothing) w
cuiRender rm _ s (ShowMap m trans) w = rm (debugMode w) (mapView m (place w) trans (visitHitory w) $ mazeInf s w)
cuiRender rm _ _ (SaveGame _ _)    w = rm (debugMode w) (flashMsgBox "  ** NOW SAVING ... **  ")
cuiRender rm _ _ (LoadGame _  )    w = rm (debugMode w) (flashMsgBox "  ** NOW LOADING... **  ")
cuiRender rm _ _ Exit              w = undefined

mazeInf :: Scenario -> World -> MazeInf
mazeInf s w = case runGameState s w mazeInf' of (Right m, w') -> m
                                                (Left  _, w') -> ("", (0,0), undefined)
  where
    mazeInf' = currentPosition >>= mazeInfAt . thd3 . coordOf

toT :: Maybe String -> String
toT (Just s) = s
toT Nothing  = ""

-- --------------------------------------------------------------------------

rendering :: RenderMethod
          -> (Maybe PictureInf -> Craphic)
          -> Scenario
          -> String -- ^ message on MessageBox
          -> String -- ^ message on FlashMessageBox
          -> String -- ^ message on CommandBox
          -> Maybe (CharacterID, Maybe [ItemPos]) -- ^ inspection view target, valid item indecies.
          -> Maybe PictureInf
          -> World
          -> IO()
rendering rm picOf s mMsg fMsg cMsg cid' picInf w = rm (debugMode w) $
       t1 (if null locationText         then mempty else location locationText)
    <> t1 (if null fMsg                 then mempty else flashMsgBox fMsg)
    <> t1 (if null mMsg' || isJust cid' then mempty else (msgTrans . msgBox') mMsg')
    <> t1 (if null cMsg  || isJust cid' then mempty else cmdBox cMsg )
    <> t1 (if visibleStatusWindow w && not hideStatus then status s w (catMaybes ps) else mempty)
    <> t1 (if visibleGuideWindow w then guide else mempty)
    <>    (if null cMsg && null mMsg && isNothing picInf then minimapScreen else mempty)
--  <> t1 location (show $ (take 5 . eventFlags) w) -- MEMO:forDebug
    <> t1 statusScene
    <> t1 (debugWindow $ debugMessage w) -- MEMO:forDebug
    <> t1 (frameTrans w $ frame)
    <> t1 (enemyTrans w $ enemyScene picOf s (place w))
    <> t1 treasureScene
    <> t1 (picOf picInf)
    <> t1 (sceneTrans w $ scene (place w) (partyLight w > 0) (partyLight' w > 0) (thd3 $ mazeInf s w))
  where
    t1    = translate (1, 1)
    ps    = flip Map.lookup (allCharacters w) <$> party w
    cs    = allCharacters w
    ess   = case place w of InBattle _ ess' -> ess'
                            _               -> []
    isInBattle   = case place w of InBattle _ _ -> True
                                   _            -> False
    isChestOpend = case place w of FindTreasureChest _ True  -> True
                                   _                         -> False
    isOnTreasure = case place w of FindTreasureChest {} -> True
                                   _                    -> False
    treasureScene = case place w of FindTreasureChest _ False -> treasureChest
                                    FindTreasureChest _ True  -> treasure
                                    _                         -> mempty
    statusScene   = case cid' of Nothing         -> mempty
                                 Just (cid, his) -> statusView s w mMsg cMsg his itemDefOf (cs Map.! cid)
    msgBox' = case place w of Camping _ _ -> msgBoxCamp
                              _           -> msgBox
    mMsg' | not (null mMsg) = mMsg
          | not (null ess)  = unlines $ take 4 $ fmap txtEnemy (zip [1..] ess) ++ repeat "\n"
          | isOnTreasure    = "you found a treasure chest."
          | otherwise       = mMsg
    hideStatus = ((not . null) ess && null cMsg && isNothing cid')
              || (isOnTreasure && (not . null) cMsg || isChestOpend)
              || (isInBattle && null ess)
    txtEnemy (l, es) = let
         e          = head es
         edef       = enemies s Map.! Enemy.id e
         determined = Enemy.determined e
         ename      = if determined then Enemy.name edef else Enemy.nameUndetermined edef
         nAll       = show $ length es
         nActive    = show $ length . filter (null . Enemy.statusErrors) $ es
      in show l ++ ") " ++ nAll ++ " " ++ ename ++ replicate (43 - length ename) ' '  ++ " (" ++ nActive ++ ")"
    itemDefOf = (Map.!) (Engine.GameAuto.items s)
    locationText = if isJust cid' then "" else
                   case place w of InCastle            -> "Castle" 
                                   Gilgamesh'sTavern   -> "ギルガメッシュの酒場" --"Gilgamesh's Tavern"
                                   Adventure'sInn      -> "Adventure's Inn"
                                   Boltac'sTradingPost -> "Boltac's Trading Post"
                                   TempleOfCant        -> "Temple of Cant"
                                   InEdgeOfTown        -> "Edge of Town"
                                   TrainingGrounds     -> "Training Grounds"
                                   Camping _ t         -> if null t then "Camp" else t
                                   _ -> []
    msgTrans = if null locationText then id else translate (0, 1)

    minimapScreen = case minimapType (worldOption w) of
                      Disable -> mempty
                      Normal  -> miniMapView  (place w) (visitHitory w) (6, 6) True (mazeInf s w)
                      AlwaysN -> miniMapViewN (place w) (visitHitory w) (6, 6) True (mazeInf s w)
                            

enemyScene :: (Maybe PictureInf -> Craphic) -> Scenario -> Place -> Craphic
enemyScene picOf s (InBattle _ (es:_)) =
    let e    = head es
        edef = enemies s Map.! Enemy.id e
    in if Enemy.determined e then picOf (Just $ Single $ Enemy.pic edef)
                             else changeSGR 'B' $ picOf (Just $ Single $ Enemy.picUndetermined edef)
enemyScene _ _ _ = mempty


visibleStatusWindow :: World -> Bool
visibleStatusWindow w = (statusOn w && inMaze) || showStatusAlways
  where
    inMaze = case place w of InMaze _ -> True
                             _        -> False
    showStatusAlways = case place w of InMaze _        -> False
                                       TrainingGrounds -> False
                                       _               -> True

visibleGuideWindow :: World -> Bool
visibleGuideWindow w = let inMaze = case place w of InMaze _ -> True
                                                    _        -> False
    in guideOn w && inMaze

-- ========================================================================

msgBox :: String -> Craphic
msgBox m = foldl1 (<>) (fmap toText (zip [1..] ls))
        <> rect (8, 5) (61, length ls + 2) (Draw ' ')
  where
    ls = lines m
    toText (n, t) = textSGR (9, 5 + n) (toTextMessage t) (toTextSGR t)

msgBoxCamp :: String -> Craphic
msgBoxCamp m = foldl1 (<>) (fmap toText (zip [1..] ls))
             <> rect (x, y) (lg + 6, length ls + 2) (Draw ' ')
  where
    ls = lines m
    lg = maximum $ len . toTextMessage <$> ls
    x  = (windowW - lg) `div` 2 - 3
    y  = 9
    toText (n, t) = textSGR (x + 1, y + n) (toTextMessage t) (toTextSGR t)


flashMsgBox :: String -> Craphic
flashMsgBox m = foldl1 (<>) (fmap toText (zip [1..] ls))
             <> rect (x, y) (lg + 2, length ls + 2) (Draw ' ')
  where
    ls = lines m
    lg = maximum $ len . toTextMessage <$> ls
    x  = (windowW - lg) `div` 2
    y  = 14
    toText (n, t) = textSGR (x, y + n) (toTextMessage t) (toTextSGR t)

cmdBox :: String -> Craphic
cmdBox m = foldl1 (<>) (fmap toText (zip [1..] ls))
        <> rect (45, 15) (26, h) (Draw ' ')
  where
    ls = lines m
    h  = max 10 $ length ls + 2
    toText (n, t) = textSGR (46, 15 + n) (toTextMessage t) (toTextSGR t)

debugWindow :: [String] -> Craphic
debugWindow ls = if null ls then mempty else
                 addSGR 'y' $ text (2, windowH + 3) "<< Debug Window >>"
                           <> foldl (<>) mempty (fmap toText (zip [1..] $ take h ls))
                           <> rect (1, windowH + 3) (windowW, h + 2) (Draw ' ')
  where
    h = debugWindowH
    toText (n, t) = text (2, windowH + 3 + n) t



toTextMessage :: String -> String
toTextMessage = filter (/= '^') . filter (/= '`')

toTextSGR :: String -> String
toTextSGR = reverse . foldl (\acc t -> if      t == '^' || t == '`'              then t : acc
                                       else if not (null acc) && head acc == '^' then 'W' : tail acc
                                       else if not (null acc) && head acc == '`' then 'B' : tail acc
                                       else                                           '_' : acc) []



partyStatus :: String -> Craphic
partyStatus m = foldl1 (<>) (fmap toText (zip [1..] ls))
        <> rect (30, 17) (17, 6) (Draw ' ')
  where
    ls = lines m
    toText (n, t) = text (31, 18 + n) t


status :: Scenario -> World -> [Character] -> Craphic
status s w p = foldl1 (<>) $ fmap toStatusLine (zip [1..] p) ++
                             [text (6, windowH - 6) headerPlaceHolder
                             ,rect (5, windowH - 6) (67, 8) (Draw ' ')]
  where
    headerPlaceHolder =  "#--CHARACTER NAME-------------CLASS-----AC----HITS---STATUS--"
    statusPlaceHolder =  "#  [NAME]                     [CLASS]  [AC]  [HP]  / [STAT]     @"
    toStatusLine (n, c) = let (ac', _) = runGameState s w (acOf $ Left c);
                              ac = case ac' of Right v -> v
                                               _       -> 99
                              sgr | Ash       `elem` statusErrors c = 'w'
                                  | Dead      `elem` statusErrors c = 'r'
                                  | Paralysis `elem` statusErrors c = 'm'
                                  | Sleep     `elem` statusErrors c = 'y'
                                  | hasStatusError c (Poison 0)     = 'g'
                                  | hasStatusError c (Fear 0)       = 'B'
                                  | otherwise                       = ' '
                              sgrs = replicate windowW sgr
                              stxt = toStatusText (statusErrorsOf c)
                        in textSGR (6,  windowH - 6 + n)
                                   ( replaceLine "[NAME]"  (name c) (Left 26)
                                   . replaceLine "#"       (show n) (Left 1)
                                   . replaceLine "[CLASS]" (show (alignment c) ++ "-" ++ take 3 (jobName $ job c)) (Left 7)
                                   . replaceLine "[AC]"    (show ac) (Right 4)
                                   . replaceLine "[HP]"    (show $ hp c) (Right 5)
                                   . (if null stxt then replaceLine "[STAT]"  (show $ maxhp c) (Right 6)
                                                   else replaceLine "[STAT]"  stxt             (Left 10))
                                   . replaceLine "@"       (if isLvUp c then "@" else "|") (Left 1)
                                   $ statusPlaceHolder ) sgrs
    isLvUp c = Character.exp c >= Character.totalExpToLv (Character.job c) (Character.lv c + 1)

statusView :: Scenario -> World -> String -> String -> Maybe [ItemPos] -> (ItemID -> Item.Define)  -> Character -> Craphic
statusView s w msg altContent his itemDefOf c = foldl1 (<>) (fmap toText (zip [1..] $ lines msg))
                                             <> rect (8, 24) (61, 7) (Draw ' ')
                                             <> if null altContent then statusDetailView else spellListView
  where
    his' = case his of Nothing -> toEnum <$> [0..9]
                       Just hs -> hs
    toText (n, t) = textSGR (11, 25 + n) (toTextMessage t) (toTextSGR t)
    spellListView = foldl (<>) mempty (fmap toText' (zip [1..] $ lines altContent)) <> rect (6, 4) (65, 22) (Draw ' ')
      where toText' (n, t) = textSGR (8, 4 + n) (toTextMessage t) (toTextSGR t)
    statusDetailView = translate (5, 4) (fromTextsSGR ' ' (toTextMessage <$> statusDetailText) (toTextSGR <$> statusDetailText))
                    <> rect (6, 4) (65, 22) (Draw ' ')
    stxt = toStatusText (statusErrorsOf $ removeStatusError (Command "") c)
    statusDetailText = replaceText "[Name]"   (name c) (Left 30)
                     . replaceText "[Item1]"  (itemN 0) (Left inMax)
                     . replaceText "[Item2]"  (itemN 1) (Left inMax)
                     . replaceText "[Item3]"  (itemN 2) (Left inMax)
                     . replaceText "[Item4]"  (itemN 3) (Left inMax)
                     . replaceText "[Item5]"  (itemN 4) (Left inMax)
                     . replaceText "[Item6]"  (itemN 5) (Left inMax)
                     . replaceText "[Item7]"  (itemN 6) (Left inMax)
                     . replaceText "[Item8]"  (itemN 7) (Left inMax)
                     . replaceText "[Item9]"  (itemN 8) (Left inMax)
                     . replaceText "[Item0]"  (itemN 9) (Left inMax)
                     . replaceText "A)#" ((if ItemA `elem` his' then "A)" else "  ") ++ equipMarks !! 0) (Left 3)
                     . replaceText "B)#" ((if ItemB `elem` his' then "B)" else "  ") ++ equipMarks !! 1) (Left 3)
                     . replaceText "C)#" ((if ItemC `elem` his' then "C)" else "  ") ++ equipMarks !! 2) (Left 3)
                     . replaceText "D)#" ((if ItemD `elem` his' then "D)" else "  ") ++ equipMarks !! 3) (Left 3)
                     . replaceText "E)#" ((if ItemE `elem` his' then "E)" else "  ") ++ equipMarks !! 4) (Left 3)
                     . replaceText "F)#" ((if ItemF `elem` his' then "F)" else "  ") ++ equipMarks !! 5) (Left 3)
                     . replaceText "G)#" ((if ItemG `elem` his' then "G)" else "  ") ++ equipMarks !! 6) (Left 3)
                     . replaceText "H)#" ((if ItemH `elem` his' then "H)" else "  ") ++ equipMarks !! 7) (Left 3)
                     . replaceText "I)#" ((if ItemI `elem` his' then "I)" else "  ") ++ equipMarks !! 8) (Left 3)
                     . replaceText "J)#" ((if ItemJ `elem` his' then "J)" else "  ") ++ equipMarks !! 9) (Left 3)
                     . replaceText "[Lv]"    (show $ lv c)            (Right 4)
                     . replaceText "[STR]"   (show $ strength st)     (Right 3)
                     . replaceText "[IQ]"    (show $ iq       st)     (Right 3)
                     . replaceText "[PIE]"   (show $ piety    st)     (Right 3)
                     . replaceText "[VIT]"   (show $ vitality st)     (Right 3)
                     . replaceText "[AGI]"   (show $ agility  st)     (Right 3)
                     . replaceText "[LUK]"   (show $ luck     st)     (Right 3)
                     . replaceText "[HP]"    (show $ hp c)            (Right 4)
                     . replaceText "[MxHP]"  (show $ maxhp c)         (Right 4)
                     . replaceText "[Exp]"   (show $ Character.exp c) (Right 8)
                     . replaceText "[Gold]"  (show $ gold c)          (Right 8)
                     . replaceText "[Age]"   (show $ age c)           (Right 4)
                     . replaceText "[AC]"    (show $ ac)              (Right 4)
                     . replaceText "[Marks]" (show $ marks c)         (Right 4)
                     . replaceText "[RIPs]"  (show $ rips c)          (Right 4)
                     . replaceText "[Status]" (if null stxt then "正常" else stxt) (Left 11)
                     . replaceText "M1"  (show $ (fst (mp c) ++ repeat 0) !! 0) (Right 2)
                     . replaceText "M2"  (show $ (fst (mp c) ++ repeat 0) !! 1) (Right 2)
                     . replaceText "M3"  (show $ (fst (mp c) ++ repeat 0) !! 2) (Right 2)
                     . replaceText "M4"  (show $ (fst (mp c) ++ repeat 0) !! 3) (Right 2)
                     . replaceText "M5"  (show $ (fst (mp c) ++ repeat 0) !! 4) (Right 2)
                     . replaceText "M6"  (show $ (fst (mp c) ++ repeat 0) !! 5) (Right 2)
                     . replaceText "M7"  (show $ (fst (mp c) ++ repeat 0) !! 6) (Right 2)
                     . replaceText "P1"  (show $ (snd (mp c) ++ repeat 0) !! 0) (Right 2)
                     . replaceText "P2"  (show $ (snd (mp c) ++ repeat 0) !! 1) (Right 2)
                     . replaceText "P3"  (show $ (snd (mp c) ++ repeat 0) !! 2) (Right 2)
                     . replaceText "P4"  (show $ (snd (mp c) ++ repeat 0) !! 3) (Right 2)
                     . replaceText "P5"  (show $ (snd (mp c) ++ repeat 0) !! 4) (Right 2)
                     . replaceText "P6"  (show $ (snd (mp c) ++ repeat 0) !! 5) (Right 2)
                     . replaceText "P7"  (show $ (snd (mp c) ++ repeat 0) !! 6) (Right 2)
                     $ statusViewPlaceHolder
      where
        (ac', _) = runGameState s w (acOf $ Left c)
        ac = case ac' of Right v -> v
                         _       -> 99
        (st', _) = runGameState s w (paramOf $ Left c)
        st = case st' of Right v -> v
                         _       -> emptyParam
        inMax = 24 -- maximum length of item name
        items' = ((\(ItemInf id identified) -> if identified then Item.name (itemDefOf id) else Item.nameUndetermined (itemDefOf id))
                  <$> Character.items c) ++ repeat ""
        itemN n = let nam = lenCut inMax (items' !! n) in if toEnum n `elem` his' then nam else '`' : intersperse '`' (nam ++ replicate (inMax - len nam) ' ')
        equipMarks = me (Character.items c) (Character.equips c)
          where me (i@(ItemInf id identified):is) eqs
                    | i `elem` eqs                                            = "*" : me is (filter (/=i) eqs)
                    | identified && not (c `Character.canEquip` itemDefOf id) = "#" : me is eqs
                    | otherwise                                               = " " : me is eqs
                me _ [] = repeat " "
                me [] _ = undefined


replaceText :: String -> String -> Either Int Int -> [String] -> [String]
replaceText src dst align ls = replaceLine src dst align <$> ls

replaceLine :: String -> String -> Either Int Int -> String -> String
replaceLine src dst align = rep src' dst''
  where
    dstT   = toTextMessage dst
    alignR = case align of Left  _ -> False
                           Right _ -> True
    tl     = case align of Left  i -> i
                           Right i -> i
    dstT'  = fill (tl - len dstT) dstT alignR
    dst'   = fill (tl - len dstT) dst  alignR
    n'     = max (len src) (len dstT')
    dst''  = fill (n' - len dstT') dst' False
    src'   = fill (n' - len src  ) src   False
    fill n s toL
      | n <= 0    = s
      | otherwise = if toL then ' ' : fill (n - 1) s toL else fill (n - 1) s toL ++ " "

rep :: String -> String -> String -> String
rep _   _  [] = []
rep src dst s = if src `isPrefixOf` s then dst ++ rep src dst (drop (len src) s) else head s : rep src dst (tail s)


statusViewPlaceHolder =
  ["                                                                 "  --   1
  ,"    [Name]                          Lv [Lv]           [KAJ]      "  --   2
  ,"                                                                 "  --   3
  ,"                         HP : [HP]/[MxHP]   Status : [Status]    "  --   4
  ,"      STR :[STR]                                                 "  --   5
  ,"       IQ :[IQ]         Exp : [Exp]            Age : [Age]       "  --   6
  ,"      PIE :[PIE]       Next : [Next]            AC : [AC]        "  --   7
  ,"      VIT :[VIT]       Gold : [Gold]         Marks : [Marks]     "  --   8
  ,"      AGI :[AGI]                              RIPs : [RIPs]      "  --   9
  ,"      LUK :[LUK]                                                 "  --   10
  ,"                         Spell M:M1/M2/M3/M4/M5/M6/M7            "  --   11
  ,"                               P:P1/P2/P3/P4/P5/P6/P7            "  --   12
  ,"                                                                 "  --   13
  ,"    A)#[Item1]                    F)#[Item6]                     "  --   14
  ,"    B)#[Item2]                    G)#[Item7]                     "  --   15
  ,"    C)#[Item3]                    H)#[Item8]                     "  --   16
  ,"    D)#[Item4]                    I)#[Item9]                     "  --   17
  ,"    E)#[Item5]                    J)#[Item0]                     "  --   18
  ]
--  12345678901234567890123456789012345678901234567890123456789012345
--           1         2         3         4         5         6      



scene :: Place
      -> Bool       -- ^ light effect.
      -> Bool       -- ^ super light effect.
      -> Maze
      -> Craphic
scene (InMaze p)              onLight superLight = dunsion p onLight superLight True
scene (InBattle p _)          onLight superLight = dunsion p onLight superLight False
scene (FindTreasureChest p _) onLight superLight = dunsion p onLight superLight False
scene (Camping p _)           onLight superLight = do
          msg <- const $ if onLight || superLight then partyStatus "    Light" else mempty
          d   <- dunsion p onLight superLight False
          return $ msg <> d
scene InCastle                _       _          = const $ translate (0, 2) city2
scene InEdgeOfTown            _       _          = const $ translate (0, 2) edgeOfTown
scene EnteringMaze            onLight superLight = scene InEdgeOfTown onLight superLight
scene Gilgamesh'sTavern       _       _          = const $ translate (0, 2) tavern
scene _                       _       _          = const mempty


-- ========================================================================

type MazeInf = (String, Size2D, Maze)

-- mapView
mapView :: String -> Place -> (Int, Int) -> Map.Map Coord Bool -> MazeInf -> Craphic
mapView msg place (dx, dy) mvt (_, (w, h), m) = case place of
    InMaze   p   -> mapView' p
    Camping  p _ -> mapView' p
    InBattle p _ -> mapView' p
    _            -> mempty
  where
    mapView' p = 
         translate (0, -4) (msgBox msg) <> frame <>
         (translate (1 + dx, 3 + dy) .
          translate (windowW `div` 2 - pcx * 3 - 2, windowH `div` 2 - (h - pcy) * 2 + 1))
         (coord <> noVisitArea mvt size (z p) <> fromTextsA '*' 'c' (showMaze size p m))
      where
        size = (w, h)
        pcx  = if w <= 20 then w `div` 2 else x p
        pcy  = if h <= 16 then h `div` 2 else y p
        coord = Craphic $ \(sx, sy) ->
          let cx = (sx - 3) `div` 3;
              cy = (h * 2 - sy) `div` 2  in
          if      sy == h * 2 + 2 && cx >=   0 && sx `mod` 3 == 0 && cx < w then
            DrawSGR (head $ show cx) (fromJust $ toSGR 'c')
          else if sy == h * 2 + 3 && cx >=  10 && sx `mod` 3 == 0 && cx < w then
            DrawSGR (head $ show $ cx `mod` 10) (fromJust $ toSGR 'c')
          else if sy == h * 2 + 4 && cx >= 100 && sx `mod` 3 == 0 && cx < w then
            DrawSGR (head $ show $ cx `mod` 100) (fromJust $ toSGR 'c')
          else if sx ==   0  && cy >=   0 && sy `mod` 2 == 0 && cy < h then
            DrawSGR (head $ show $ cy `mod`   10) (fromJust $ toSGR 'c')
          else if sx == (-1) && cy >=  10 && sy `mod` 2 == 0 && cy < h then
            DrawSGR (head $ show $ cy `mod`  100) (fromJust $ toSGR 'c')
          else if sx == (-2) && cy >= 100 && sy `mod` 2 == 0 && cy < h then
            DrawSGR (head $ show $ cy `mod` 1000) (fromJust $ toSGR 'c')
          else
            Blank


-- mini map view.
miniMapView :: Place
            -> Map.Map Coord Bool
            -> (Int, Int)
            -> Bool
            -> MazeInf
            -> Craphic
miniMapView place mvt (viewW, viewH) isTransparent (fn, (w, h), m) = case place of
    (InMaze p) ->
      let vw = (viewW - 1) * 3 + 1 + 4;
          vh = (viewH - 1) * 2 + 1 + 2;
          size = (w, h);
          filter    = if isTransparent then addSGR 'c' else id;
          rectBack  = if isTransparent then Blank      else Draw ' ';
          blankChar = if isTransparent then ' '        else '*'
      in filter (text (1, vh+2) (fn ++ "(" ++ show (x p) ++ "," ++ show (y p) ++ ")")) <>
         translate (1, 1) ((trim (1, 1) (vw, vh) .
                            translate (vw `div` 2 - x p * 3 - 2, vh `div` 2 - (h - y p) * 2 + 1))
                           (noVisitArea mvt size (z p) <> fromTextsA blankChar 'c' (showMaze size p m))
                          <> filter (rect (0, 0) (vw + 2, vh + 2) rectBack))
    _          -> mempty

noVisitArea :: Map.Map Coord Bool -> Size -> Int -> Craphic
noVisitArea mvt = noVisitAreaR mvt Data.Maze.N



-- mini map view always draw as seen direction upside.
miniMapViewN :: Place
             -> Map.Map Coord Bool
             -> (Int, Int)
             -> Bool
             -> MazeInf
             -> Craphic
miniMapViewN place mvt (viewW, viewH) isTransparent (fn, (w, h), m) = case place of
    (InMaze p) ->
      let vw = (viewW - 1) * 3 + 1 + 4;
          vh = (viewH - 1) * 2 + 1 + 2;
          d  = direction p;
          w' = (case d of Data.Maze.E -> h
                          Data.Maze.W -> h
                          _           -> w);
          h' = (case d of Data.Maze.E -> w
                          Data.Maze.W -> w
                          _           -> h);
          size  = (w,  h );
          size' = (w', h');
          p'    = rotatePosition d size p;
          filter    = if isTransparent then addSGR 'c' else id;
          rectBack  = if isTransparent then Blank      else Draw ' ';
          blankChar = if isTransparent then ' '        else '*'
      in filter (text (1, vh+2) (fn ++ "(" ++ show (x p) ++ "," ++ show (y p) ++ ":" ++ show d ++ ")")) <>
         translate (1, 1) ((trim (1, 1) (vw, vh) .
                            translate (vw `div` 2 - x p' * 3 - 2, vh `div` 2 - (h' - y p') * 2 + 1))
                           (noVisitAreaR mvt d size' (z p') <> fromTextsA blankChar 'c' (showMaze size' p' $ rotate d size m))
                          <> filter (rect (0, 0) (vw + 2, vh + 2) Blank))
    _          -> mempty


noVisitAreaR :: Map.Map Coord Bool -> Direction -> Size -> Int -> Craphic
noVisitAreaR mvt newN size z = fromTextsA ' ' '6' $ makeMazeMask isVisited '?' ' ' z size
  where
    isVisited (x, y, z) = let p = Position newN x y z
                              p'= rotatePositionRev newN size p
                          in Map.lookup (coordOf p') mvt == Just True

-- ========================================================================

dunsion :: Position
        -> Bool -- ^ light effect.
        -> Bool -- ^ super light effect.
        -> Bool -- ^ show message(for darkzone/in stone) or not.
        -> Maze
        -> Craphic
dunsion p onLight superLight msg m = addSGR 'B' $ foldl1 mappend $
    (front <$> [(d, s) | d <-ds, s <- ss]) ++ (dark' <$> [(last ds + 1, s) | s <- ss])
  where
    ds = if onLight || superLight then [0..3] else [0..1]
    ss = if onLight || superLight then [0,1,-1,2,-2,3,-3] else [0,1,-1]
    nots0 = noticesInView m p 0 0
    stn0  = if Stone `elem` nots0 then inStone msg else mempty
    front (d, s) = stn0 <> sw <> drk <> fw <> upn <> dwn
      where
        fw = case visiblityAt m p d s F of
                 Wall -> frontWall d s
                 Door -> frontDoor d s
                 _    -> mempty
        sw = if s == 0 then mempty
             else let (side, s') = if s > 0 then (R, s - 1) else (L, s + 1) in
                 case visiblityAt m p d s' side of
                     Wall -> sideWall d s
                     Door -> sideDoor d s
                     _    -> mempty
        nots = noticesInView m p d s
        upn = if Up   `elem` nots then upNotice   d s else mempty
        dwn = if Down `elem` nots then downNotice d s else mempty
        drk = if Dark `elem` nots && not superLight then darkNotice msg d s else mempty
    dark' (d, s) = drk
      where
        nots = noticesInView m p d s
        drk = if Dark `elem` nots then darkNotice' msg d s else mempty

-- ========================================================================

frame :: Craphic
frame = fromTexts '*' $
  ["                                                                             "  --   1
  ," +-----------------------------------------------------------------------+   "  --   2
  ," |***********************************************************************|   "  --   3
  ," |***********************************************************************|   "  --   4
  ," |***********************************************************************|   "  --   5
  ," |***********************************************************************|   "  --   6
  ," |***********************************************************************|   "  --   7
  ," |***********************************************************************|   "  --   8
  ," |***********************************************************************|   "  --   9
  ," |***********************************************************************|   "  --  10
  ," |***********************************************************************|   "  --  11
  ," |***********************************************************************|   "  --  12
  ," |***********************************************************************|   "  --  13
  ," |***********************************************************************|   "  --  14
  ," |***********************************************************************|   "  --  15
  ," |***********************************************************************|   "  --  16
  ," |***********************************************************************|   "  --  17
  ," |***********************************************************************|   "  --  18
  ," |***********************************************************************|   "  --  19
  ," |***********************************************************************|   "  --  20
  ," |***********************************************************************|   "  --  21
  ," |***********************************************************************|   "  --  22
  ," |***********************************************************************|   "  --  23
  ," |***********************************************************************|   "  --  24
  ," |***********************************************************************|   "  --  25
  ," |***********************************************************************|   "  --  26
  ," |***********************************************************************|   "  --  27
  ," |***********************************************************************|   "  --  28
  ," |***********************************************************************|   "  --  29
  ," |***********************************************************************|   "  --  30
  ," |***********************************************************************|   "  --  31
  ," |***********************************************************************|   "  --  32
  ," |***********************************************************************|   "  --  33
  ," |***********************************************************************|   "  --  34
  ," |***********************************************************************|   "  --  35
  ," |***********************************************************************|   "  --  36
  ," |***********************************************************************|   "  --  37
  ," |***********************************************************************|   "  --  38
  ," +-----------------------------------------------------------------------+   "  --  39
  ,"                                                                             "  --  40
  ,"                                                                             "  --  41
  ,"                                                                             "] --  42
  ++
  replicate 20 "                                                                             "

guide :: Craphic
guide = fromTextsSGR '*'
  ["*********+-------------------------------------------------------+********"  --  1
  ,"*********|   C)AMP  S)TATUS  I)NSPECT  Q)UIT  O)FF   A-W-D   K   |********"  --  2
  ,"*********+-------------------------------------------------------+********"] --  3
--------------------------------------------------------------------------------
  ["                                                                          "  --  1
  ,"             W      W        W         W      W      W W W   W            "  --  2
  ,"                                                                          "] --  3

location :: String -> Craphic
location l = text (16 + (43 - len l ) `div` 2, 4) l <> fromTexts '*'
  ["**************************************************************************"  --  1
  ,"**************************************************************************"  --  2
  ,"***************+-------------------------------------------+**************"  --  3
  ,"***************|                                           |**************"  --  4
  ,"***************+-------------------------------------------+**************"] --  5

revC = windowW `div` 2 + 1
-- ========================================================================
-- Position index:
--
--    <> Side wall      <> Dark zone    <> Front wall  
--                                                      
--   depth             depth           depth            
--        -- -- --         -- -- --       2  -- -- --   
--     2 |  |  |  |      2   |  |              |  |     
--        -- -- --         -- -- --       1  -- -- --   
--     1 |  |  |  |      1   |  |              |  |     
--        -- -- --         -- -- --       0  -- -- --   
--     0 |  |@ |  |      0   |@ |              |@ |     
--                                                      
-- side -2 -1 +1 +2        -1  0 +1          -1  0 +1   
--
--
frontWall :: Int    -- ^ distance of depth.
          -> Int    -- ^ distance of side.
          -> Craphic
frontWall 0 0 = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"*************+-----------------------------------------------+*************"  --   6
  ,"*************|                                               |*************"  --   7
  ,"*************|                                               |*************"  --   8
  ,"*************|                                               |*************"  --   9
  ,"*************|                                               |*************"  --  10
  ,"*************|                                               |*************"  --  11
  ,"*************|                                               |*************"  --  12
  ,"*************|                                               |*************"  --  13
  ,"*************|                                               |*************"  --  14
  ,"*************|                                               |*************"  --  15
  ,"*************|                                               |*************"  --  16
  ,"*************|                                               |*************"  --  17
  ,"*************|                                               |*************"  --  18
  ,"*************|                                               |*************"  --  19
  ,"*************|                                               |*************"  --  20
  ,"*************|                                               |*************"  --  21
  ,"*************|                                               |*************"  --  22
  ,"*************|                                               |*************"  --  23
  ,"*************|                                               |*************"  --  24
  ,"*************|                                               |*************"  --  25
  ,"*************|                                               |*************"  --  26
  ,"*************|                                               |*************"  --  27
  ,"*************|                                               |*************"  --  28
  ,"*************|                                               |*************"  --  29
  ,"*************|                                               |*************"  --  30
  ,"*************|                                               |*************"  --  31
  ,"*************|                                               |*************"  --  32
  ,"*************|                                               |*************"  --  33
  ,"*************|                                               |*************"  --  34
  ,"*************|                                               |*************"  --  35
  ,"*************|_______________________________________________|*************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontWall 0  1   = reverseH revC $ frontWall 0 (-1)
frontWall 0 (-1) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"-------------+*************************************************************"  --   6
  ,"             |*************************************************************"  --   7
  ,"             |*************************************************************"  --   8
  ,"             |*************************************************************"  --   9
  ,"             |*************************************************************"  --  10
  ,"             |*************************************************************"  --  11
  ,"             |*************************************************************"  --  12
  ,"             |*************************************************************"  --  13
  ,"             |*************************************************************"  --  14
  ,"             |*************************************************************"  --  15
  ,"             |*************************************************************"  --  16
  ,"             |*************************************************************"  --  17
  ,"             |*************************************************************"  --  18
  ,"             |*************************************************************"  --  19
  ,"             |*************************************************************"  --  20
  ,"             |*************************************************************"  --  21
  ,"             |*************************************************************"  --  22
  ,"             |*************************************************************"  --  23
  ,"             |*************************************************************"  --  24
  ,"             |*************************************************************"  --  25
  ,"             |*************************************************************"  --  26
  ,"             |*************************************************************"  --  27
  ,"             |*************************************************************"  --  28
  ,"             |*************************************************************"  --  29
  ,"             |*************************************************************"  --  30
  ,"             |*************************************************************"  --  31
  ,"             |*************************************************************"  --  32
  ,"             |*************************************************************"  --  33
  ,"             |*************************************************************"  --  34
  ,"             |*************************************************************"  --  35
  ,"_____________|*************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40

frontWall 1 0 = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"**********************+-----------------------------+**********************"  --  15
  ,"**********************|                             |**********************"  --  16
  ,"**********************|                             |**********************"  --  17
  ,"**********************|                             |**********************"  --  18
  ,"**********************|                             |**********************"  --  19
  ,"**********************|                             |**********************"  --  20
  ,"**********************|                             |**********************"  --  21
  ,"**********************|                             |**********************"  --  22
  ,"**********************|                             |**********************"  --  23
  ,"**********************|                             |**********************"  --  24
  ,"**********************|                             |**********************"  --  25
  ,"**********************|                             |**********************"  --  26
  ,"**********************|                             |**********************"  --  27
  ,"**********************|                             |**********************"  --  28
  ,"**********************|                             |**********************"  --  29
  ,"**********************|                             |**********************"  --  30
  ,"**********************|_____________________________|**********************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontWall 1   1  = reverseH revC $ frontWall 1 (-1)
frontWall 1 (-1) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"----------------------+****************************************************"  --  15
  ,"                      |****************************************************"  --  16
  ,"                      |****************************************************"  --  17
  ,"                      |****************************************************"  --  18
  ,"                      |****************************************************"  --  19
  ,"                      |****************************************************"  --  20
  ,"                      |****************************************************"  --  21
  ,"                      |****************************************************"  --  22
  ,"                      |****************************************************"  --  23
  ,"                      |****************************************************"  --  24
  ,"                      |****************************************************"  --  25
  ,"                      |****************************************************"  --  26
  ,"                      |****************************************************"  --  27
  ,"                      |****************************************************"  --  28
  ,"                      |****************************************************"  --  29
  ,"                      |****************************************************"  --  30
  ,"______________________|****************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40

frontWall 2 0 = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************+-------------------+***************************"  --  20
  ,"***************************|                   |***************************"  --  21
  ,"***************************|                   |***************************"  --  22
  ,"***************************|                   |***************************"  --  23
  ,"***************************|                   |***************************"  --  24
  ,"***************************|                   |***************************"  --  25
  ,"***************************|                   |***************************"  --  26
  ,"***************************|                   |***************************"  --  27
  ,"***************************|                   |***************************"  --  28
  ,"***************************+-------------------+***************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontWall 2   1  = reverseH revC $ frontWall 2 (-1)
frontWall 2 (-1) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"*******+-------------------+***********************************************"  --  20
  ,"*******|                   |***********************************************"  --  21
  ,"*******|                   |***********************************************"  --  22
  ,"*******|                   |***********************************************"  --  23
  ,"*******|                   |***********************************************"  --  24
  ,"*******|                   |***********************************************"  --  25
  ,"*******|                   |***********************************************"  --  26
  ,"*******|                   |***********************************************"  --  27
  ,"*******|                   |***********************************************"  --  28
  ,"*******+-------------------+***********************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontWall 2   2  = reverseH revC $ frontWall 2 (-2)
frontWall 2 (-2) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"-------+*******************************************************************"  --  20
  ,"       |*******************************************************************"  --  21
  ,"       |*******************************************************************"  --  22
  ,"       |*******************************************************************"  --  23
  ,"       |*******************************************************************"  --  24
  ,"       |*******************************************************************"  --  25
  ,"       |*******************************************************************"  --  26
  ,"       |*******************************************************************"  --  27
  ,"       |*******************************************************************"  --  28
  ,"-------+*******************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40

frontWall 3 0 = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************************************************************"  --  20
  ,"***************************************************************************"  --  21
  ,"***************************************************************************"  --  22
  ,"******************************+-------------+******************************"  --  23
  ,"******************************|             |******************************"  --  24
  ,"******************************|             |******************************"  --  25
  ,"******************************|             |******************************"  --  26
  ,"******************************|             |******************************"  --  27
  ,"*******************************~~~~~~~~~~~~~*******************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontWall 3   1  = reverseH revC $ frontWall 3 (-1)
frontWall 3 (-1) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************************************************************"  --  20
  ,"***************************************************************************"  --  21
  ,"***************************************************************************"  --  22
  ,"****************+-------------+********************************************"  --  23
  ,"****************|             |********************************************"  --  24
  ,"****************|             |********************************************"  --  25
  ,"****************|             |********************************************"  --  26
  ,"****************|             |********************************************"  --  27
  ,"*****************~~~~~~~~~~~~~*********************************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontWall 3   2  = reverseH revC $ frontWall 3 (-2)
frontWall 3 (-2) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************************************************************"  --  20
  ,"***************************************************************************"  --  21
  ,"***************************************************************************"  --  22
  ,"**+-------------+**********************************************************"  --  23
  ,"**|             |**********************************************************"  --  24
  ,"**|             |**********************************************************"  --  25
  ,"**|             |**********************************************************"  --  26
  ,"**|             |**********************************************************"  --  27
  ,"***~~~~~~~~~~~~~***********************************************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontWall 3   3  = reverseH revC $ frontWall 3 (-3)
frontWall 3 (-3) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************************************************************"  --  20
  ,"***************************************************************************"  --  21
  ,"***************************************************************************"  --  22
  ,"--+************************************************************************"  --  23
  ,"  |************************************************************************"  --  24
  ,"  |************************************************************************"  --  25
  ,"  |************************************************************************"  --  26
  ,"  |************************************************************************"  --  27
  ,"~~*************************************************************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40


frontWall _ _ = mempty

-- ========================================================================

frontDoor 0 0 = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"*************+-----------------------------------------------+*************"  --   6
  ,"*************|                                               |*************"  --   7
  ,"*************|                                               |*************"  --   8
  ,"*************|                                               |*************"  --   9
  ,"*************|                                               |*************"  --  10
  ,"*************|                                               |*************"  --  11
  ,"*************|                                               |*************"  --  12
  ,"*************|                                               |*************"  --  13
  ,"*************|                                               |*************"  --  14
  ,"*************|      +---------------------------------+      |*************"  --  15
  ,"*************|      |                                 |      |*************"  --  16
  ,"*************|      |                                 |      |*************"  --  17
  ,"*************|      |                                 |      |*************"  --  18
  ,"*************|      |                                 |      |*************"  --  19
  ,"*************|      |                                 |      |*************"  --  20
  ,"*************|      |                                 |      |*************"  --  21
  ,"*************|      |                                 |      |*************"  --  22
  ,"*************|      |                                 |      |*************"  --  23
  ,"*************|      |                                 |      |*************"  --  24
  ,"*************|      |                                 |      |*************"  --  25
  ,"*************|      |                                 |      |*************"  --  26
  ,"*************|      |                                 |      |*************"  --  27
  ,"*************|      |                                 |      |*************"  --  28
  ,"*************|      |                                 |      |*************"  --  29
  ,"*************|      |                                 |      |*************"  --  30
  ,"*************|      |                                 |      |*************"  --  31
  ,"*************|      |                                 |      |*************"  --  32
  ,"*************|      |                                 |      |*************"  --  33
  ,"*************|      |                                 |      |*************"  --  34
  ,"*************|      |                                 |      |*************"  --  35
  ,"*************|______|_________________________________|______|*************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontDoor 0  1   = reverseH revC $ frontDoor 0 (-1)
frontDoor 0 (-1) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"-------------+*************************************************************"  --   6
  ,"             |*************************************************************"  --   7
  ,"             |*************************************************************"  --   8
  ,"             |*************************************************************"  --   9
  ,"             |*************************************************************"  --  10
  ,"             |*************************************************************"  --  11
  ,"             |*************************************************************"  --  12
  ,"             |*************************************************************"  --  13
  ,"             |*************************************************************"  --  14
  ,"--------+    |*************************************************************"  --  15
  ,"        |    |*************************************************************"  --  16
  ,"        |    |*************************************************************"  --  17
  ,"        |    |*************************************************************"  --  18
  ,"        |    |*************************************************************"  --  19
  ,"        |    |*************************************************************"  --  20
  ,"        |    |*************************************************************"  --  21
  ,"        |    |*************************************************************"  --  22
  ,"        |    |*************************************************************"  --  23
  ,"        |    |*************************************************************"  --  24
  ,"        |    |*************************************************************"  --  25
  ,"        |    |*************************************************************"  --  26
  ,"        |    |*************************************************************"  --  27
  ,"        |    |*************************************************************"  --  28
  ,"        |    |*************************************************************"  --  29
  ,"        |    |*************************************************************"  --  30
  ,"        |    |*************************************************************"  --  31
  ,"        |    |*************************************************************"  --  32
  ,"        |    |*************************************************************"  --  33
  ,"        |    |*************************************************************"  --  34
  ,"        |    |*************************************************************"  --  35
  ,"________|____|*************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40

frontDoor 1 0 = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"**********************+-----------------------------+**********************"  --  15
  ,"**********************|                             |**********************"  --  16
  ,"**********************|                             |**********************"  --  17
  ,"**********************|                             |**********************"  --  18
  ,"**********************|    +-------------------+    |**********************"  --  19
  ,"**********************|    |                   |    |**********************"  --  20
  ,"**********************|    |                   |    |**********************"  --  21
  ,"**********************|    |                   |    |**********************"  --  22
  ,"**********************|    |                   |    |**********************"  --  23
  ,"**********************|    |                   |    |**********************"  --  24
  ,"**********************|    |                   |    |**********************"  --  25
  ,"**********************|    |                   |    |**********************"  --  26
  ,"**********************|    |                   |    |**********************"  --  27
  ,"**********************|    |                   |    |**********************"  --  28
  ,"**********************|    |                   |    |**********************"  --  29
  ,"**********************|    |                   |    |**********************"  --  30
  ,"**********************|____|___________________|____|**********************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontDoor 1   1  = reverseH revC $ frontDoor 1 (-1)
frontDoor 1 (-1) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"----------------------+****************************************************"  --  15
  ,"                      |****************************************************"  --  16
  ,"                      |****************************************************"  --  17
  ,"                      |****************************************************"  --  18
  ,"+-----------------+   |****************************************************"  --  19
  ,"|                 |   |****************************************************"  --  20
  ,"|                 |   |****************************************************"  --  21
  ,"|                 |   |****************************************************"  --  22
  ,"|                 |   |****************************************************"  --  23
  ,"|                 |   |****************************************************"  --  24
  ,"|                 |   |****************************************************"  --  25
  ,"|                 |   |****************************************************"  --  26
  ,"|                 |   |****************************************************"  --  27
  ,"|                 |   |****************************************************"  --  28
  ,"|                 |   |****************************************************"  --  29
  ,"|                 |   |****************************************************"  --  30
  ,"|_________________|___|****************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40

frontDoor 2 0 = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************+-------------------+***************************"  --  20
  ,"***************************|                   |***************************"  --  21
  ,"***************************|                   |***************************"  --  22
  ,"***************************|    +---------+    |***************************"  --  23
  ,"***************************|    |         |    |***************************"  --  24
  ,"***************************|    |         |    |***************************"  --  25
  ,"***************************|    |         |    |***************************"  --  26
  ,"***************************|    |         |    |***************************"  --  27
  ,"***************************|    |         |    |***************************"  --  28
  ,"***************************+----+---------+----+***************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontDoor 2   1  = reverseH revC $ frontDoor 2 (-1)
frontDoor 2 (-1) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"*******+-------------------+***********************************************"  --  20
  ,"*******|                   |***********************************************"  --  21
  ,"*******|                   |***********************************************"  --  22
  ,"*******|    +---------+    |***********************************************"  --  23
  ,"*******|    |         |    |***********************************************"  --  24
  ,"*******|    |         |    |***********************************************"  --  25
  ,"*******|    |         |    |***********************************************"  --  26
  ,"*******|    |         |    |***********************************************"  --  27
  ,"*******|    |         |    |***********************************************"  --  28
  ,"*******+----+---------+----+***********************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontDoor 2   2  = reverseH revC $ frontDoor 2 (-2)
frontDoor 2 (-2) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"-------+*******************************************************************"  --  20
  ,"       |*******************************************************************"  --  21
  ,"       |*******************************************************************"  --  22
  ,"--+    |*******************************************************************"  --  23
  ,"  |    |*******************************************************************"  --  24
  ,"  |    |*******************************************************************"  --  25
  ,"  |    |*******************************************************************"  --  26
  ,"  |    |*******************************************************************"  --  27
  ,"  |    |*******************************************************************"  --  28
  ,"--+----+*******************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40

frontDoor 3 0 = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************************************************************"  --  20
  ,"***************************************************************************"  --  21
  ,"***************************************************************************"  --  22
  ,"******************************+-------------+******************************"  --  23
  ,"******************************|             |******************************"  --  24
  ,"******************************|   +-----+   |******************************"  --  25
  ,"******************************|   |     |   |******************************"  --  26
  ,"******************************|   |     |   |******************************"  --  27
  ,"*******************************~~~~~~~~~~~~~*******************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontDoor 3   1  = reverseH revC $ frontDoor 3 (-1)
frontDoor 3 (-1) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************************************************************"  --  20
  ,"***************************************************************************"  --  21
  ,"***************************************************************************"  --  22
  ,"****************+-------------+********************************************"  --  23
  ,"****************|             |********************************************"  --  24
  ,"****************|   +-----+   |********************************************"  --  25
  ,"****************|   |     |   |********************************************"  --  26
  ,"****************|   |     |   |********************************************"  --  27
  ,"*****************~~~~~~~~~~~~~*********************************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontDoor 3   2  = reverseH revC $ frontDoor 3 (-2)
frontDoor 3 (-2) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************************************************************"  --  20
  ,"***************************************************************************"  --  21
  ,"***************************************************************************"  --  22
  ,"**+-------------+**********************************************************"  --  23
  ,"**|             |**********************************************************"  --  24
  ,"**|   +-----+   |**********************************************************"  --  25
  ,"**|   |     |   |**********************************************************"  --  26
  ,"**|   |     |   |**********************************************************"  --  27
  ,"***~~~~~~~~~~~~~***********************************************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
frontDoor 3   3  = reverseH revC $ frontDoor 3 (-3)
frontDoor 3 (-3) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************************************************************"  --  20
  ,"***************************************************************************"  --  21
  ,"***************************************************************************"  --  22
  ,"--+************************************************************************"  --  23
  ,"  |************************************************************************"  --  24
  ,"  |************************************************************************"  --  25
  ,"  |************************************************************************"  --  26
  ,"  |************************************************************************"  --  27
  ,"~~*************************************************************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40

frontDoor _ _ = mempty

-- ========================================================================
--
repSlash = replace (Draw '／') (Draw '＼') . replace (Draw '\\') (Draw '/')

sideWall _   0  = mempty
sideWall 0   1  = reverseH revC . repSlash $ sideWall 0 (-1)
sideWall 0 (-1) = fromTexts '*'
  ["        \\******************************************************************" --   1
  ,"         \\*****************************************************************" --   2
  ,"          \\****************************************************************" --   3
  ,"           \\***************************************************************" --   4
  ,"            \\**************************************************************" --   5
  ,"             \\*************************************************************" --   6
  ,"             |*************************************************************"  --   7
  ,"             |*************************************************************"  --   8
  ,"             |*************************************************************"  --   9
  ,"             |*************************************************************"  --  10
  ,"             |*************************************************************"  --  11
  ,"             |*************************************************************"  --  12
  ,"             |*************************************************************"  --  13
  ,"             |*************************************************************"  --  14
  ,"             |*************************************************************"  --  15
  ,"             |*************************************************************"  --  16
  ,"             |*************************************************************"  --  17
  ,"             |*************************************************************"  --  18
  ,"             |*************************************************************"  --  19
  ,"             |*************************************************************"  --  20
  ,"             |*************************************************************"  --  21
  ,"             |*************************************************************"  --  22
  ,"             |*************************************************************"  --  23
  ,"             |*************************************************************"  --  24
  ,"             |*************************************************************"  --  25
  ,"             |*************************************************************"  --  26
  ,"             |*************************************************************"  --  27
  ,"             |*************************************************************"  --  28
  ,"             |*************************************************************"  --  29
  ,"             |*************************************************************"  --  30
  ,"             |*************************************************************"  --  31
  ,"             |*************************************************************"  --  32
  ,"             |*************************************************************"  --  33
  ,"             |*************************************************************"  --  34
  ,"             |*************************************************************"  --  35
  ,"             |*************************************************************"  --  36
  ,"           -~**************************************************************"  --  37
  ,"         -~****************************************************************"  --  38
  ,"       -~******************************************************************"  --  39
  ,"     -~********************************************************************"] --  40
sideWall 1   1  = reverseH revC . repSlash $ sideWall 1 (-1)
sideWall 1 (-1) = fromTexts '*'
  ["***************************************************************************"   --   1
  ,"***************************************************************************"   --   2
  ,"***************************************************************************"   --   3
  ,"***************************************************************************"   --   4
  ,"***************************************************************************"   --   5
  ,"*************\\*************************************************************"  --   6
  ,"*************|\\************************************************************"  --   7
  ,"*************| \\***********************************************************"  --   8
  ,"*************|  \\**********************************************************"  --   9
  ,"*************|   \\*********************************************************"  --  10
  ,"*************|    \\********************************************************"  --  11
  ,"*************|     \\*******************************************************"  --  12
  ,"*************|      \\******************************************************"  --  13
  ,"*************|       \\*****************************************************"  --  14
  ,"*************|        \\****************************************************"  --  15
  ,"*************|        |****************************************************"   --  16
  ,"*************|        |****************************************************"   --  17
  ,"*************|        |****************************************************"   --  18
  ,"*************|        |****************************************************"   --  19
  ,"*************|        |****************************************************"   --  20
  ,"*************|        |****************************************************"   --  21
  ,"*************|        |****************************************************"   --  22
  ,"*************|        |****************************************************"   --  23
  ,"*************|        |****************************************************"   --  24
  ,"*************|        |****************************************************"   --  25
  ,"*************|        |****************************************************"   --  26
  ,"*************|        |****************************************************"   --  27
  ,"*************|        |****************************************************"   --  28
  ,"*************|        |****************************************************"   --  29
  ,"*************|        |****************************************************"   --  30
  ,"*************|        |****************************************************"   --  31
  ,"*************|       -~****************************************************"   --  32
  ,"*************|     -~******************************************************"   --  33
  ,"*************|   -~********************************************************"   --  34
  ,"*************| -~**********************************************************"   --  35
  ,"*************-~************************************************************"   --  36
  ,"***************************************************************************"   --  37
  ,"***************************************************************************"   --  38
  ,"***************************************************************************"   --  39
  ,"***************************************************************************"]  --  40

sideWall 2   1  = reverseH revC . repSlash $ sideWall 2 (-1)
sideWall 2 (-1) = fromTexts '*'
  ["***************************************************************************"   --   1
  ,"***************************************************************************"   --   2
  ,"***************************************************************************"   --   3
  ,"***************************************************************************"   --   4
  ,"***************************************************************************"   --   5
  ,"***************************************************************************"   --   6
  ,"***************************************************************************"   --   7
  ,"***************************************************************************"   --   8
  ,"***************************************************************************"   --   9
  ,"***************************************************************************"   --  10
  ,"***************************************************************************"   --  11
  ,"***************************************************************************"   --  12
  ,"***************************************************************************"   --  13
  ,"***************************************************************************"   --  14
  ,"**********************\\****************************************************"  --  15
  ,"**********************|\\***************************************************"  --  16
  ,"**********************| \\**************************************************"  --  17
  ,"**********************|  \\*************************************************"  --  18
  ,"**********************|   \\************************************************"  --  19
  ,"**********************|    \\***********************************************"  --  20
  ,"**********************|    |***********************************************"   --  21
  ,"**********************|    |***********************************************"   --  22
  ,"**********************|    |***********************************************"   --  23
  ,"**********************|    |***********************************************"   --  24
  ,"**********************|    |***********************************************"   --  25
  ,"**********************|    |***********************************************"   --  26
  ,"**********************|    |***********************************************"   --  27
  ,"**********************|    |***********************************************"   --  28
  ,"**********************|    -***********************************************"   --  29
  ,"**********************|  -~************************************************"   --  30
  ,"**********************|-~**************************************************"   --  31
  ,"**********************~****************************************************"   --  32
  ,"***************************************************************************"   --  33
  ,"***************************************************************************"   --  34
  ,"***************************************************************************"   --  35
  ,"***************************************************************************"   --  36
  ,"***************************************************************************"   --  37
  ,"***************************************************************************"   --  38
  ,"***************************************************************************"   --  39
  ,"***************************************************************************"]  --  40
sideWall 2   2  = reverseH revC . repSlash $ sideWall 2 (-2)
sideWall 2 (-2) = fromTexts '*'
  ["***************************************************************************"   --   1
  ,"***************************************************************************"   --   2
  ,"***************************************************************************"   --   3
  ,"***************************************************************************"   --   4
  ,"***************************************************************************"   --   5
  ,"***************************************************************************"   --   6
  ,"***************************************************************************"   --   7
  ,"***************************************************************************"   --   8
  ,"***************************************************************************"   --   9
  ,"***************************************************************************"   --  10
  ,"***************************************************************************"   --  11
  ,"***************************************************************************"   --  12
  ,"***************************************************************************"   --  13
  ,"***************************************************************************"   --  14
  ,"***************************************************************************"   --  15
  ,"***************************************************************************"   --  16
  ,"***************************************************************************"   --  17
  ,"~-_************************************************************************"   --  18
  ,"   ~-_*********************************************************************"   --  19
  ,"      ~+*******************************************************************"   --  20
  ,"       |*******************************************************************"   --  21
  ,"       |*******************************************************************"   --  22
  ,"       |*******************************************************************"   --  23
  ,"       |*******************************************************************"   --  24
  ,"       |*******************************************************************"   --  25
  ,"       |*******************************************************************"   --  26
  ,"       |*******************************************************************"   --  27
  ,"       |*******************************************************************"   --  28
  ,"      _+*******************************************************************"   --  29
  ," __--~*********************************************************************"   --  30
  ,"~**************************************************************************"   --  31
  ,"***************************************************************************"   --  32
  ,"***************************************************************************"   --  33
  ,"***************************************************************************"   --  34
  ,"***************************************************************************"   --  35
  ,"***************************************************************************"   --  36
  ,"***************************************************************************"   --  37
  ,"***************************************************************************"   --  38
  ,"***************************************************************************"   --  39
  ,"***************************************************************************"]  --  40

sideWall 3   1  = reverseH revC . repSlash $ sideWall 3 (-1)
sideWall 3 (-1) = fromTexts '*'
  ["***************************************************************************"   --   1
  ,"***************************************************************************"   --   2
  ,"***************************************************************************"   --   3
  ,"***************************************************************************"   --   4
  ,"***************************************************************************"   --   5
  ,"***************************************************************************"   --   6
  ,"***************************************************************************"   --   7
  ,"***************************************************************************"   --   8
  ,"***************************************************************************"   --   9
  ,"***************************************************************************"   --  10
  ,"***************************************************************************"   --  11
  ,"***************************************************************************"   --  12
  ,"***************************************************************************"   --  13
  ,"***************************************************************************"   --  14
  ,"***************************************************************************"   --  15
  ,"***************************************************************************"   --  16
  ,"***************************************************************************"   --  17
  ,"***************************************************************************"   --  18
  ,"***************************************************************************"   --  19
  ,"***************************\\***********************************************"  --  20
  ,"***************************|\\**********************************************"  --  21
  ,"***************************| \\*********************************************"  --  22
  ,"***************************|  \\********************************************"  --  23
  ,"***************************|  |********************************************"   --  24
  ,"***************************|  |********************************************"   --  25
  ,"***************************|  |********************************************"   --  26
  ,"***************************|  |********************************************"   --  27
  ,"***************************| _~********************************************"   --  28
  ,"***************************-~**********************************************"   --  29
  ,"***************************************************************************"   --  30
  ,"***************************************************************************"   --  31
  ,"***************************************************************************"   --  32
  ,"***************************************************************************"   --  33
  ,"***************************************************************************"   --  34
  ,"***************************************************************************"   --  35
  ,"***************************************************************************"   --  36
  ,"***************************************************************************"   --  37
  ,"***************************************************************************"   --  38
  ,"***************************************************************************"   --  39
  ,"***************************************************************************"]  --  40
sideWall 3   2  = reverseH revC $ sideWall 3 (-2)
sideWall 3 (-2) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"*******+_******************************************************************"  --  20
  ,"*******| ~-_***************************************************************"  --  21
  ,"*******|    ~-_************************************************************"  --  22
  ,"*******|       ~+**********************************************************"  --  23
  ,"*******|        |**********************************************************"  --  24
  ,"*******|        |**********************************************************"  --  25
  ,"*******|        |**********************************************************"  --  26
  ,"*******|        |**********************************************************"  --  27
  ,"*******|   __---***********************************************************"  --  28
  ,"*******+-~~****************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
sideWall 3   3  = reverseH revC $ sideWall 3 (-3)
sideWall 3 (-3) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************************************************************"  --  20
  ,"***************************************************************************"  --  21
  ,"_**************************************************************************"  --  22
  ," ~+************************************************************************"  --  23
  ,"  |************************************************************************"  --  24
  ,"  |************************************************************************"  --  25
  ,"  |************************************************************************"  --  26
  ,"  |************************************************************************"  --  27
  ,"-~*************************************************************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40

sideWall _ _ = mempty

sideDoor 0   1  = reverseH revC . repSlash $ sideDoor 0 (-1)
sideDoor 0 (-1) = fromTexts '*'
  ["        \\******************************************************************"  --   1
  ,"         \\*****************************************************************"  --   2
  ,"          \\****************************************************************"  --   3
  ,"           \\***************************************************************"  --   4
  ,"            \\**************************************************************"  --   5
  ,"             \\*************************************************************"  --   6
  ,"             |*************************************************************"   --   7
  ,"             |*************************************************************"   --   8
  ,"             |*************************************************************"   --   9
  ,"             |*************************************************************"   --  10
  ,"~-_          |*************************************************************"   --  11
  ,"   ~-_       |*************************************************************"   --  12
  ,"      ~-_    |*************************************************************"   --  13
  ,"         ~-_ |*************************************************************"   --  14
  ,"           | |*************************************************************"   --  15
  ,"           | |*************************************************************"   --  16
  ,"           | |*************************************************************"   --  17
  ,"           | |*************************************************************"   --  18
  ,"           | |*************************************************************"   --  19
  ,"           | |*************************************************************"   --  20
  ,"           | |*************************************************************"   --  21
  ,"           | |*************************************************************"   --  22
  ,"           | |*************************************************************"   --  23
  ,"           | |*************************************************************"   --  24
  ,"           | |*************************************************************"   --  25
  ,"           | |*************************************************************"   --  26
  ,"           | |*************************************************************"   --  27
  ,"           | |*************************************************************"   --  28
  ,"           | |*************************************************************"   --  29
  ,"           | |*************************************************************"   --  30
  ,"           | |*************************************************************"   --  31
  ,"           | |*************************************************************"   --  32
  ,"           | |*************************************************************"   --  33
  ,"           | |*************************************************************"   --  34
  ,"           | |*************************************************************"   --  35
  ,"           | |*************************************************************"   --  36
  ,"           -~**************************************************************"   --  37
  ,"         -~****************************************************************"   --  38
  ,"       -~******************************************************************"   --  39
  ,"     -~********************************************************************"]  --  40

sideDoor 1   1  = reverseH revC . repSlash $ sideDoor 1 (-1)
sideDoor 1 (-1) = fromTexts '*'
  ["***************************************************************************"   --   1
  ,"***************************************************************************"   --   2
  ,"***************************************************************************"   --   3
  ,"***************************************************************************"   --   4
  ,"***************************************************************************"   --   5
  ,"*************\\*************************************************************"  --   6
  ,"*************|\\************************************************************"  --   7
  ,"*************| \\***********************************************************"  --   8
  ,"*************|  \\**********************************************************"  --   9
  ,"*************|   \\*********************************************************"  --  10
  ,"*************|    \\********************************************************"  --  11
  ,"*************|     \\*******************************************************"  --  12
  ,"*************|      \\******************************************************"  --  13
  ,"*************|       \\*****************************************************"  --  14
  ,"*************|        \\****************************************************"  --  15
  ,"*************|        |****************************************************"   --  16
  ,"*************| |~_    |****************************************************"   --  17
  ,"*************| |  ~_  |****************************************************"   --  18
  ,"*************| |    | |****************************************************"   --  19
  ,"*************| |    | |****************************************************"   --  20
  ,"*************| |    | |****************************************************"   --  21
  ,"*************| |    | |****************************************************"   --  22
  ,"*************| |    | |****************************************************"   --  23
  ,"*************| |    | |****************************************************"   --  24
  ,"*************| |    | |****************************************************"   --  25
  ,"*************| |    | |****************************************************"   --  26
  ,"*************| |    | |****************************************************"   --  27
  ,"*************| |    | |****************************************************"   --  28
  ,"*************| |    | |****************************************************"   --  29
  ,"*************| |    | |****************************************************"   --  30
  ,"*************| |    | |****************************************************"   --  31
  ,"*************| |    |-~****************************************************"   --  32
  ,"*************| |   -~******************************************************"   --  33
  ,"*************| | -~********************************************************"   --  34
  ,"*************| -~**********************************************************"   --  35
  ,"*************-~************************************************************"   --  36
  ,"***************************************************************************"   --  37
  ,"***************************************************************************"   --  38
  ,"***************************************************************************"   --  39
  ,"***************************************************************************"]  --  40
--  1234567890123456789012345678901234567890123456789012345678901234567890
--           1         2         3         4         5         6         7

sideDoor 2   1  = reverseH revC . repSlash $ sideDoor 2 (-1)
sideDoor 2 (-1) = fromTexts '*'
  ["***************************************************************************"   --   1
  ,"***************************************************************************"   --   2
  ,"***************************************************************************"   --   3
  ,"***************************************************************************"   --   4
  ,"***************************************************************************"   --   5
  ,"***************************************************************************"   --   6
  ,"***************************************************************************"   --   7
  ,"***************************************************************************"   --   8
  ,"***************************************************************************"   --   9
  ,"***************************************************************************"   --  10
  ,"***************************************************************************"   --  11
  ,"***************************************************************************"   --  12
  ,"***************************************************************************"   --  13
  ,"***************************************************************************"   --  14
  ,"**********************\\****************************************************"  --  15
  ,"**********************|\\***************************************************"  --  16
  ,"**********************| \\**************************************************"  --  17
  ,"**********************|  \\*************************************************"  --  18
  ,"**********************|   \\************************************************"  --  19
  ,"**********************|    \\***********************************************"  --  20
  ,"**********************||-_ |***********************************************"   --  21
  ,"**********************||  ||***********************************************"   --  22
  ,"**********************||  ||***********************************************"   --  23
  ,"**********************||  ||***********************************************"   --  24
  ,"**********************||  ||***********************************************"   --  25
  ,"**********************||  ||***********************************************"   --  26
  ,"**********************||  ||***********************************************"   --  27
  ,"**********************||  ||***********************************************"   --  28
  ,"**********************||  |-***********************************************"   --  29
  ,"**********************|| -~************************************************"   --  30
  ,"**********************|-~**************************************************"   --  31
  ,"**********************~****************************************************"   --  32
  ,"***************************************************************************"   --  33
  ,"***************************************************************************"   --  34
  ,"***************************************************************************"   --  35
  ,"***************************************************************************"   --  36
  ,"***************************************************************************"   --  37
  ,"***************************************************************************"   --  38
  ,"***************************************************************************"   --  39
  ,"***************************************************************************"]  --  40
sideDoor 2   2  = reverseH revC . repSlash $ sideDoor 2 (-2)
sideDoor 2 (-2) = fromTexts '*'
  ["***************************************************************************"   --   1
  ,"***************************************************************************"   --   2
  ,"***************************************************************************"   --   3
  ,"***************************************************************************"   --   4
  ,"***************************************************************************"   --   5
  ,"***************************************************************************"   --   6
  ,"***************************************************************************"   --   7
  ,"***************************************************************************"   --   8
  ,"***************************************************************************"   --   9
  ,"***************************************************************************"   --  10
  ,"***************************************************************************"   --  11
  ,"***************************************************************************"   --  12
  ,"***************************************************************************"   --  13
  ,"***************************************************************************"   --  14
  ,"***************************************************************************"   --  15
  ,"***************************************************************************"   --  16
  ,"***************************************************************************"   --  17
  ,"~-_************************************************************************"   --  18
  ,"   ~-_*********************************************************************"   --  19
  ,"      ~+*******************************************************************"   --  20
  ,"-__    |*******************************************************************"   --  21
  ,"   ~+  |*******************************************************************"   --  22
  ,"    |  |*******************************************************************"   --  23
  ,"    |  |*******************************************************************"   --  24
  ,"    |  |*******************************************************************"   --  25
  ,"    |  |*******************************************************************"   --  26
  ,"    |  |*******************************************************************"   --  27
  ,"    |  |*******************************************************************"   --  28
  ,"    | _+*******************************************************************"   --  29
  ," __--~*********************************************************************"   --  30
  ,"~**************************************************************************"   --  31
  ,"***************************************************************************"   --  32
  ,"***************************************************************************"   --  33
  ,"***************************************************************************"   --  34
  ,"***************************************************************************"   --  35
  ,"***************************************************************************"   --  36
  ,"***************************************************************************"   --  37
  ,"***************************************************************************"   --  38
  ,"***************************************************************************"   --  39
  ,"***************************************************************************"]  --  40

sideDoor 3   1  = reverseH revC . repSlash $ sideDoor 3 (-1)
sideDoor 3 (-1) = fromTexts '*'
  ["***************************************************************************"   --   1
  ,"***************************************************************************"   --   2
  ,"***************************************************************************"   --   3
  ,"***************************************************************************"   --   4
  ,"***************************************************************************"   --   5
  ,"***************************************************************************"   --   6
  ,"***************************************************************************"   --   7
  ,"***************************************************************************"   --   8
  ,"***************************************************************************"   --   9
  ,"***************************************************************************"   --  10
  ,"***************************************************************************"   --  11
  ,"***************************************************************************"   --  12
  ,"***************************************************************************"   --  13
  ,"***************************************************************************"   --  14
  ,"***************************************************************************"   --  15
  ,"***************************************************************************"   --  16
  ,"***************************************************************************"   --  17
  ,"***************************************************************************"   --  18
  ,"***************************************************************************"   --  19
  ,"***************************\\***********************************************"  --  20
  ,"***************************|\\**********************************************"  --  21
  ,"***************************| \\*********************************************"  --  22
  ,"***************************|  \\********************************************"  --  23
  ,"***************************|-_|********************************************"   --  24
  ,"***************************||||********************************************"   --  25
  ,"***************************||||********************************************"   --  26
  ,"***************************||||********************************************"   --  27
  ,"***************************||_~********************************************"   --  28
  ,"***************************-~**********************************************"   --  29
  ,"***************************************************************************"   --  30
  ,"***************************************************************************"   --  31
  ,"***************************************************************************"   --  32
  ,"***************************************************************************"   --  33
  ,"***************************************************************************"   --  34
  ,"***************************************************************************"   --  35
  ,"***************************************************************************"   --  36
  ,"***************************************************************************"   --  37
  ,"***************************************************************************"   --  38
  ,"***************************************************************************"   --  39
  ,"***************************************************************************"]  --  40
sideDoor 3   2  = reverseH revC $ sideDoor 3 (-2)
sideDoor 3 (-2) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"*******+_******************************************************************"  --  20
  ,"*******| ~-_***************************************************************"  --  21
  ,"*******|    ~-_************************************************************"  --  22
  ,"*******|  +-_  ~+**********************************************************"  --  23
  ,"*******|  |  ~+ |**********************************************************"  --  24
  ,"*******|  |   | |**********************************************************"  --  25
  ,"*******|  |   | |**********************************************************"  --  26
  ,"*******|  |   | |**********************************************************"  --  27
  ,"*******|  |__---***********************************************************"  --  28
  ,"*******+-~~****************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
sideDoor 3   3  = reverseH revC $ sideDoor 3 (-3)
sideDoor 3 (-3) = fromTexts '*'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"***************************************************************************"  --  17
  ,"***************************************************************************"  --  18
  ,"***************************************************************************"  --  19
  ,"***************************************************************************"  --  20
  ,"***************************************************************************"  --  21
  ,"_**************************************************************************"  --  22
  ," ~+************************************************************************"  --  23
  ,"  |************************************************************************"  --  24
  ,"+ |************************************************************************"  --  25
  ,"| |************************************************************************"  --  26
  ,"| |************************************************************************"  --  27
  ,"-~*************************************************************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40


sideDoor _ _ = mempty

upNotice :: Int    -- ^ distance of depth.
         -> Int    -- ^ distance of side.
         -> Craphic
upNotice 0 0 = fromTextsA '_' 'w'
  ["______________###############################################______________"  --   1
  ,"_______________#############################################_______________"  --   2
  ,"________________###########################################________________"  --   3
  ,"_________________#########################################_________________"  --   4
  ,"__________________#######################################__________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
upNotice 0  1   = reverseH revC $ upNotice 0 (-1)
upNotice 0 (-1) = fromTextsA '_' 'w'
  ["######_____________________________________________________________________"  --   1
  ,"#######____________________________________________________________________"  --   2
  ,"########___________________________________________________________________"  --   3
  ,"#########__________________________________________________________________"  --   4
  ,"##########_________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40

upNotice 1 0 = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"__________________#######################################__________________"  --   8
  ,"___________________#####################################___________________"  --   9
  ,"____________________###################################____________________"  --  10
  ,"_____________________#################################_____________________"  --  11
  ,"______________________###############################______________________"  --  12
  ,"_______________________#############################_______________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
upNotice 1   1  = reverseH revC $ upNotice 1 (-1)
upNotice 1 (-1) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"##############_____________________________________________________________"  --   8
  ,"###############____________________________________________________________"  --   9
  ,"################___________________________________________________________"  --  10
  ,"#################__________________________________________________________"  --  11
  ,"##################_________________________________________________________"  --  12
  ,"###################________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40

upNotice 2 0 = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________#####################___________________________"  --  16
  ,"____________________________###################____________________________"  --  17
  ,"_____________________________#################_____________________________"  --  18
  ,"______________________________##############_______________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
upNotice 2   1  = reverseH revC $ upNotice 2 (-1)
upNotice 2 (-1) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"__#####################____________________________________________________"  --  16
  ,"____####################___________________________________________________"  --  17
  ,"______###################__________________________________________________"  --  18
  ,"________##################_________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
upNotice 2   2  = reverseH revC $ upNotice 2 (-2)
upNotice 2 (-2) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"#__________________________________________________________________________"  --  17
  ,"###________________________________________________________________________"  --  18
  ,"######_____________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40

upNotice 3 0 = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"________________________________###########________________________________"  --  21
  ,"_________________________________#########_________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
upNotice 3   1  = reverseH revC $ upNotice 3 (-1)
upNotice 3 (-1) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"_______________#############_______________________________________________"  --  21
  ,"_________________############______________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
upNotice 3   2  = reverseH revC $ upNotice 3 (-2)
upNotice 3 (-2) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"_###########_______________________________________________________________"  --  21
  ,"____###########____________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40


upNotice _ _ = mempty

downNotice 0 0 = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"_____________#################################################_____________"  --  38
  ,"___________#####################################################___________"  --  39
  ,"_________#########################################################_________"] --  40

downNotice 0   1  = reverseH revC $ downNotice 0 (-1)
downNotice 0 (-1) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"#########__________________________________________________________________"  --  38
  ,"######_____________________________________________________________________"  --  39
  ,"###________________________________________________________________________"] --  40

downNotice 1 0 = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"_________________________#########################_________________________"  --  32
  ,"_______________________#############################_______________________"  --  33
  ,"_____________________#################################_____________________"  --  34
  ,"___________________#####################################___________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
downNotice 1   1  = reverseH revC $ downNotice 1 (-1)
downNotice 1 (-1) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"#####################______________________________________________________"  --  32
  ,"###################________________________________________________________"  --  33
  ,"#################__________________________________________________________"  --  34
  ,"###############____________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40

downNotice 2 0 = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"_____________________________#################_____________________________"  --  30
  ,"____________________________###################____________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
downNotice 2   1  = reverseH revC $ downNotice 2 (-1)
downNotice 2 (-1) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"_____#####################_________________________________________________"  --  30
  ,"__######################___________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
downNotice 2   2  = reverseH revC $ downNotice 2 (-2)
downNotice 2 (-2) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"###________________________________________________________________________"  --  30
  ,"#__________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40

downNotice 3 0 = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"_______________________________#############_______________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
downNotice 3   1  = reverseH revC $ downNotice 3 (-1)
downNotice 3 (-1) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"____________################_______________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
downNotice 3   2  = reverseH revC $ downNotice 3 (-2)
downNotice 3 (-2) = fromTextsA '_' 'w'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"___________________________________________________________________________"  --  24
  ,"___________________________________________________________________________"  --  25
  ,"___________________________________________________________________________"  --  26
  ,"___________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"###########________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40

downNotice _ _ = mempty


darkNotice :: Bool -> Int -> Int -> Craphic
darkNotice False 0 0 = Craphic $ const $ Draw '*'
darkNotice True  0 0 = fromTextsSGR '_'
  ["***************************************************************************"  --   1
  ,"***************************************************************************"  --   2
  ,"***************************************************************************"  --   3
  ,"***************************************************************************"  --   4
  ,"***************************************************************************"  --   5
  ,"***************************************************************************"  --   6
  ,"***************************************************************************"  --   7
  ,"***************************************************************************"  --   8
  ,"***************************************************************************"  --   9
  ,"***************************************************************************"  --  10
  ,"***************************************************************************"  --  11
  ,"***************************************************************************"  --  12
  ,"***************************************************************************"  --  13
  ,"***************************************************************************"  --  14
  ,"***************************************************************************"  --  15
  ,"***************************************************************************"  --  16
  ,"****************+-----------------------------------------+****************"  --  17
  ,"****************|                                         |****************"  --  18
  ,"****************|           ** DARK ZONE !!! **           |****************"  --  19
  ,"****************|                                         |****************"  --  20
  ,"****************+-----------------------------------------+****************"  --  21
  ,"***************************************************************************"  --  22
  ,"***************************************************************************"  --  23
  ,"***************************************************************************"  --  24
  ,"***************************************************************************"  --  25
  ,"***************************************************************************"  --  26
  ,"***************************************************************************"  --  27
  ,"***************************************************************************"  --  28
  ,"***************************************************************************"  --  29
  ,"***************************************************************************"  --  30
  ,"***************************************************************************"  --  31
  ,"***************************************************************************"  --  32
  ,"***************************************************************************"  --  33
  ,"***************************************************************************"  --  34
  ,"***************************************************************************"  --  35
  ,"***************************************************************************"  --  36
  ,"***************************************************************************"  --  37
  ,"***************************************************************************"  --  38
  ,"***************************************************************************"  --  39
  ,"***************************************************************************"] --  40
------------------------------------------------------------------------------------
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"________________WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW________________"  --  17
  ,"________________WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW________________"  --  18
  ,"________________WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW________________"  --  19
  ,"________________WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW________________"  --  20
  ,"________________WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW________________"  --  21
  ,"___________________________________________________________________________"]
darkNotice _ 0   1  = reverseH revC . repSlash $ darkNotice False 0 (-1)
darkNotice _ 0 (-1) = fromTexts '_'
  ["********___________________________________________________________________" --   1
  ,"*********__________________________________________________________________" --   2
  ,"**********_________________________________________________________________" --   3
  ,"***********________________________________________________________________" --   4
  ,"************_______________________________________________________________" --   5
  ,"*************______________________________________________________________" --   6
  ,"*************______________________________________________________________"  --   7
  ,"*************______________________________________________________________"  --   8
  ,"*************______________________________________________________________"  --   9
  ,"*************______________________________________________________________"  --  10
  ,"*************______________________________________________________________"  --  11
  ,"*************______________________________________________________________"  --  12
  ,"*************______________________________________________________________"  --  13
  ,"*************______________________________________________________________"  --  14
  ,"*************______________________________________________________________"  --  15
  ,"*************______________________________________________________________"  --  16
  ,"*************______________________________________________________________"  --  17
  ,"*************______________________________________________________________"  --  18
  ,"*************______________________________________________________________"  --  19
  ,"*************______________________________________________________________"  --  20
  ,"*************______________________________________________________________"  --  21
  ,"*************______________________________________________________________"  --  22
  ,"*************______________________________________________________________"  --  23
  ,"*************______________________________________________________________"  --  24
  ,"*************______________________________________________________________"  --  25
  ,"*************______________________________________________________________"  --  26
  ,"*************______________________________________________________________"  --  27
  ,"*************______________________________________________________________"  --  28
  ,"*************______________________________________________________________"  --  29
  ,"*************______________________________________________________________"  --  30
  ,"*************______________________________________________________________"  --  31
  ,"*************______________________________________________________________"  --  32
  ,"*************______________________________________________________________"  --  33
  ,"*************______________________________________________________________"  --  34
  ,"*************______________________________________________________________"  --  35
  ,"*************______________________________________________________________"  --  36
  ,"***********________________________________________________________________"  --  37
  ,"*********__________________________________________________________________"  --  38
  ,"*******____________________________________________________________________"  --  39
  ,"*****______________________________________________________________________"] --  40

darkNotice _ 1 0 = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"______________***********************************************______________"  --   7
  ,"______________***********************************************______________"  --   8
  ,"______________***********************************************______________"  --   9
  ,"______________***********************************************______________"  --  10
  ,"______________***********************************************______________"  --  11
  ,"______________***********************************************______________"  --  12
  ,"______________***********************************************______________"  --  13
  ,"______________***********************************************______________"  --  14
  ,"______________***********************************************______________"  --  15
  ,"______________***********************************************______________"  --  16
  ,"______________***********************************************______________"  --  17
  ,"______________***********************************************______________"  --  18
  ,"______________***********************************************______________"  --  19
  ,"______________***********************************************______________"  --  20
  ,"______________***********************************************______________"  --  21
  ,"______________***********************************************______________"  --  22
  ,"______________***********************************************______________"  --  23
  ,"______________***********************************************______________"  --  24
  ,"______________***********************************************______________"  --  25
  ,"______________***********************************************______________"  --  26
  ,"______________***********************************************______________"  --  27
  ,"______________***********************************************______________"  --  28
  ,"______________***********************************************______________"  --  29
  ,"______________***********************************************______________"  --  30
  ,"______________***********************************************______________"  --  31
  ,"______________***********************************************______________"  --  32
  ,"______________***********************************************______________"  --  33
  ,"______________***********************************************______________"  --  34
  ,"______________***********************************************______________"  --  35
  ,"______________***********************************************______________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
darkNotice _ 1   1  = reverseH revC $ darkNotice False 1 (-1)
darkNotice _ 1 (-1) = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"*************#*____________________________________________________________"  --   7
  ,"****************___________________________________________________________"  --   8
  ,"*************#***__________________________________________________________"  --   9
  ,"******************_________________________________________________________"  --  10
  ,"*************#*****________________________________________________________"  --  11
  ,"********************_______________________________________________________"  --  12
  ,"*************#*******______________________________________________________"  --  13
  ,"**********************_____________________________________________________"  --  14
  ,"*************#*********____________________________________________________"  --  15
  ,"***********************____________________________________________________"  --  16
  ,"*************#*********____________________________________________________"  --  17
  ,"***********************____________________________________________________"  --  18
  ,"*************#*********____________________________________________________"  --  19
  ,"***********************____________________________________________________"  --  20
  ,"*************#*********____________________________________________________"  --  21
  ,"***********************____________________________________________________"  --  22
  ,"*************#*********____________________________________________________"  --  23
  ,"***********************____________________________________________________"  --  24
  ,"*************#*********____________________________________________________"  --  25
  ,"***********************____________________________________________________"  --  26
  ,"*************#*********____________________________________________________"  --  27
  ,"***********************____________________________________________________"  --  28
  ,"*************#*********____________________________________________________"  --  29
  ,"***********************____________________________________________________"  --  30
  ,"*************#*********____________________________________________________"  --  31
  ,"**********************_____________________________________________________"  --  32
  ,"*************#******_______________________________________________________"  --  33
  ,"******************_________________________________________________________"  --  34
  ,"*************#**___________________________________________________________"  --  35
  ,"**************_____________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40

darkNotice _ 2 0 = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"______________________*******************************______________________"  --  16
  ,"______________________*******************************______________________"  --  17
  ,"______________________*******************************______________________"  --  18
  ,"______________________*******************************______________________"  --  19
  ,"______________________*******************************______________________"  --  20
  ,"______________________*******************************______________________"  --  21
  ,"______________________*******************************______________________"  --  22
  ,"______________________*******************************______________________"  --  23
  ,"______________________*******************************______________________"  --  24
  ,"______________________*******************************______________________"  --  25
  ,"______________________*******************************______________________"  --  26
  ,"______________________*******************************______________________"  --  27
  ,"______________________*******************************______________________"  --  28
  ,"______________________*******************************______________________"  --  29
  ,"______________________*******************************______________________"  --  30
  ,"______________________*******************************______________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
darkNotice _ 2   1  = reverseH revC . repSlash $ darkNotice False 2 (-1)
darkNotice _ 2 (-1) = fromTexts '_'
  ["___________________________________________________________________________"   --   1
  ,"___________________________________________________________________________"   --   2
  ,"___________________________________________________________________________"   --   3
  ,"___________________________________________________________________________"   --   4
  ,"___________________________________________________________________________"   --   5
  ,"___________________________________________________________________________"   --   6
  ,"___________________________________________________________________________"   --   7
  ,"___________________________________________________________________________"   --   8
  ,"___________________________________________________________________________"   --   9
  ,"___________________________________________________________________________"   --  10
  ,"___________________________________________________________________________"   --  11
  ,"___________________________________________________________________________"   --  12
  ,"___________________________________________________________________________"   --  13
  ,"___________________________________________________________________________"   --  14
  ,"___________________________________________________________________________"   --  15
  ,"*********************#*____________________________________________________"   --  16
  ,"************************___________________________________________________"   --  17
  ,"*********************#***__________________________________________________"   --  18
  ,"**************************_________________________________________________"   --  19
  ,"*********************#*****________________________________________________"   --  20
  ,"***************************________________________________________________"   --  21
  ,"*********************#*****________________________________________________"   --  22
  ,"***************************________________________________________________"   --  23
  ,"*********************#*****________________________________________________"   --  24
  ,"***************************________________________________________________"   --  25
  ,"*********************#*****________________________________________________"   --  26
  ,"***************************________________________________________________"   --  27
  ,"*********************#*****________________________________________________"   --  28
  ,"***************************________________________________________________"   --  29
  ,"*********************#***__________________________________________________"   --  30
  ,"***********************____________________________________________________"   --  31
  ,"___________________________________________________________________________"   --  32
  ,"___________________________________________________________________________"   --  33
  ,"___________________________________________________________________________"   --  34
  ,"___________________________________________________________________________"   --  35
  ,"___________________________________________________________________________"   --  36
  ,"___________________________________________________________________________"   --  37
  ,"___________________________________________________________________________"   --  38
  ,"___________________________________________________________________________"   --  39
  ,"___________________________________________________________________________"]  --  40
darkNotice _ 2   2  = reverseH revC . repSlash $ darkNotice False 2 (-2)
darkNotice _ 2 (-2) = fromTexts '_'
  ["___________________________________________________________________________"   --   1
  ,"___________________________________________________________________________"   --   2
  ,"___________________________________________________________________________"   --   3
  ,"___________________________________________________________________________"   --   4
  ,"___________________________________________________________________________"   --   5
  ,"___________________________________________________________________________"   --   6
  ,"___________________________________________________________________________"   --   7
  ,"___________________________________________________________________________"   --   8
  ,"___________________________________________________________________________"   --   9
  ,"___________________________________________________________________________"   --  10
  ,"___________________________________________________________________________"   --  11
  ,"___________________________________________________________________________"   --  12
  ,"___________________________________________________________________________"   --  13
  ,"___________________________________________________________________________"   --  14
  ,"___________________________________________________________________________"   --  15
  ,"___________________________________________________________________________"   --  16
  ,"___________________________________________________________________________"   --  17
  ,"___________________________________________________________________________"   --  18
  ,"***________________________________________________________________________"   --  19
  ,"******_____________________________________________________________________"   --  20
  ,"*******____________________________________________________________________"   --  21
  ,"*******____________________________________________________________________"   --  22
  ,"*******____________________________________________________________________"   --  23
  ,"*******____________________________________________________________________"   --  24
  ,"*******____________________________________________________________________"   --  25
  ,"*******____________________________________________________________________"   --  26
  ,"*******____________________________________________________________________"   --  27
  ,"*******____________________________________________________________________"   --  28
  ,"******_____________________________________________________________________"   --  29
  ,"**_________________________________________________________________________"   --  30
  ,"___________________________________________________________________________"   --  31
  ,"___________________________________________________________________________"   --  32
  ,"___________________________________________________________________________"   --  33
  ,"___________________________________________________________________________"   --  34
  ,"___________________________________________________________________________"   --  35
  ,"___________________________________________________________________________"   --  36
  ,"___________________________________________________________________________"   --  37
  ,"___________________________________________________________________________"   --  38
  ,"___________________________________________________________________________"   --  39
  ,"___________________________________________________________________________"]  --  40

darkNotice _ 3 0 = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________*********************___________________________"  --  21
  ,"___________________________*********************___________________________"  --  22
  ,"___________________________*********************___________________________"  --  23
  ,"___________________________*********************___________________________"  --  24
  ,"___________________________*********************___________________________"  --  25
  ,"___________________________*********************___________________________"  --  26
  ,"___________________________*********************___________________________"  --  27
  ,"___________________________*********************___________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
darkNotice _ 3   1  = reverseH revC $ darkNotice False 3 (-1)
darkNotice _ 3 (-1) = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"_______********************#_______________________________________________"  --  21
  ,"_______**********************______________________________________________"  --  22
  ,"_______********************#**_____________________________________________"  --  23
  ,"_______***********************_____________________________________________"  --  24
  ,"_______********************#**_____________________________________________"  --  25
  ,"_______***********************_____________________________________________"  --  26
  ,"_______********************#**_____________________________________________"  --  27
  ,"_______**********************______________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
darkNotice _ 3   2  = reverseH revC $ darkNotice False 3 (-2)
darkNotice _ 3 (-2) = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"*******# __________________________________________________________________"  --  21
  ,"************_______________________________________________________________"  --  22
  ,"*******#*******____________________________________________________________"  --  23
  ,"*****************__________________________________________________________"  --  24
  ,"*******#*********__________________________________________________________"  --  25
  ,"*****************__________________________________________________________"  --  26
  ,"*******#*********__________________________________________________________"  --  27
  ,"***********________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40


darkNotice _ 4 0 = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"______________________________***************______________________________"  --  24
  ,"______________________________***************______________________________"  --  25
  ,"______________________________***************______________________________"  --  26
  ,"______________________________***************______________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
darkNotice _ 4   1  = reverseH revC $ darkNotice False 4 (-1)
darkNotice _ 4 (-1) = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"________________***************____________________________________________"  --  24
  ,"________________***************____________________________________________"  --  25
  ,"________________***************____________________________________________"  --  26
  ,"________________***************____________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
darkNotice _ 4   2  = reverseH revC $ darkNotice False 4 (-2)
darkNotice _ 4 (-2) = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"__***************__________________________________________________________"  --  24
  ,"__***************__________________________________________________________"  --  25
  ,"__***************__________________________________________________________"  --  26
  ,"__***************__________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
darkNotice _ 4   3  = reverseH revC $ darkNotice False 4 (-3)
darkNotice _ 4 (-3) = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"___________________________________________________________________________"  --  21
  ,"___________________________________________________________________________"  --  22
  ,"___________________________________________________________________________"  --  23
  ,"***________________________________________________________________________"  --  24
  ,"***________________________________________________________________________"  --  25
  ,"***________________________________________________________________________"  --  26
  ,"***________________________________________________________________________"  --  27
  ,"___________________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40


darkNotice _ _ _ = mempty

darkNotice' :: Bool -> Int -> Int -> Craphic
darkNotice' _ 1   1  = reverseH revC $ darkNotice' False 1 (-1)
darkNotice' _ 1 (-1) = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"**************_____________________________________________________________"  --   7
  ,"**************_____________________________________________________________"  --   8
  ,"**************_____________________________________________________________"  --   9
  ,"**************_____________________________________________________________"  --  10
  ,"**************_____________________________________________________________"  --  11
  ,"**************_____________________________________________________________"  --  12
  ,"**************_____________________________________________________________"  --  13
  ,"**************_____________________________________________________________"  --  14
  ,"**************_____________________________________________________________"  --  15
  ,"**************_____________________________________________________________"  --  16
  ,"**************_____________________________________________________________"  --  17
  ,"**************_____________________________________________________________"  --  18
  ,"**************_____________________________________________________________"  --  19
  ,"**************_____________________________________________________________"  --  20
  ,"**************_____________________________________________________________"  --  21
  ,"**************_____________________________________________________________"  --  22
  ,"**************_____________________________________________________________"  --  23
  ,"**************_____________________________________________________________"  --  24
  ,"**************_____________________________________________________________"  --  25
  ,"**************_____________________________________________________________"  --  26
  ,"**************_____________________________________________________________"  --  27
  ,"**************_____________________________________________________________"  --  28
  ,"**************_____________________________________________________________"  --  29
  ,"**************_____________________________________________________________"  --  30
  ,"**************_____________________________________________________________"  --  31
  ,"**************_____________________________________________________________"  --  32
  ,"**************_____________________________________________________________"  --  33
  ,"**************_____________________________________________________________"  --  34
  ,"**************_____________________________________________________________"  --  35
  ,"**************_____________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
darkNotice' _ 2   1  = reverseH revC . repSlash $ darkNotice' False 2 (-1)
darkNotice' _ 2 (-1) = fromTexts '_'
  ["___________________________________________________________________________"   --   1
  ,"___________________________________________________________________________"   --   2
  ,"___________________________________________________________________________"   --   3
  ,"___________________________________________________________________________"   --   4
  ,"___________________________________________________________________________"   --   5
  ,"___________________________________________________________________________"   --   6
  ,"___________________________________________________________________________"   --   7
  ,"___________________________________________________________________________"   --   8
  ,"___________________________________________________________________________"   --   9
  ,"___________________________________________________________________________"   --  10
  ,"___________________________________________________________________________"   --  11
  ,"___________________________________________________________________________"   --  12
  ,"___________________________________________________________________________"   --  13
  ,"___________________________________________________________________________"   --  14
  ,"___________________________________________________________________________"   --  15
  ,"**********************_____________________________________________________"   --  16
  ,"**********************_____________________________________________________"   --  17
  ,"**********************_____________________________________________________"   --  18
  ,"**********************_____________________________________________________"   --  19
  ,"**********************_____________________________________________________"   --  20
  ,"**********************_____________________________________________________"   --  21
  ,"**********************_____________________________________________________"   --  22
  ,"**********************_____________________________________________________"   --  23
  ,"**********************_____________________________________________________"   --  24
  ,"**********************_____________________________________________________"   --  25
  ,"**********************_____________________________________________________"   --  26
  ,"**********************_____________________________________________________"   --  27
  ,"**********************_____________________________________________________"   --  28
  ,"**********************_____________________________________________________"   --  29
  ,"**********************_____________________________________________________"   --  30
  ,"**********************_____________________________________________________"   --  31
  ,"___________________________________________________________________________"   --  32
  ,"___________________________________________________________________________"   --  33
  ,"___________________________________________________________________________"   --  34
  ,"___________________________________________________________________________"   --  35
  ,"___________________________________________________________________________"   --  36
  ,"___________________________________________________________________________"   --  37
  ,"___________________________________________________________________________"   --  38
  ,"___________________________________________________________________________"   --  39
  ,"___________________________________________________________________________"]  --  40
darkNotice' _ 2   2  = reverseH revC . repSlash $ darkNotice' False 2 (-2)
darkNotice' _ 2 (-2) = fromTexts '_'
  ["___________________________________________________________________________"   --   1
  ,"___________________________________________________________________________"   --   2
  ,"___________________________________________________________________________"   --   3
  ,"___________________________________________________________________________"   --   4
  ,"___________________________________________________________________________"   --   5
  ,"___________________________________________________________________________"   --   6
  ,"___________________________________________________________________________"   --   7
  ,"___________________________________________________________________________"   --   8
  ,"___________________________________________________________________________"   --   9
  ,"___________________________________________________________________________"   --  10
  ,"___________________________________________________________________________"   --  11
  ,"___________________________________________________________________________"   --  12
  ,"___________________________________________________________________________"   --  13
  ,"___________________________________________________________________________"   --  14
  ,"___________________________________________________________________________"   --  15
  ,"___________________________________________________________________________"   --  16
  ,"___________________________________________________________________________"   --  17
  ,"___________________________________________________________________________"   --  18
  ,"___________________________________________________________________________"   --  19
  ,"___________________________________________________________________________"   --  20
  ,"___________________________________________________________________________"   --  21
  ,"___________________________________________________________________________"   --  22
  ,"___________________________________________________________________________"   --  23
  ,"___________________________________________________________________________"   --  24
  ,"___________________________________________________________________________"   --  25
  ,"___________________________________________________________________________"   --  26
  ,"___________________________________________________________________________"   --  27
  ,"___________________________________________________________________________"   --  28
  ,"___________________________________________________________________________"   --  29
  ,"___________________________________________________________________________"   --  30
  ,"___________________________________________________________________________"   --  31
  ,"___________________________________________________________________________"   --  32
  ,"___________________________________________________________________________"   --  33
  ,"___________________________________________________________________________"   --  34
  ,"___________________________________________________________________________"   --  35
  ,"___________________________________________________________________________"   --  36
  ,"___________________________________________________________________________"   --  37
  ,"___________________________________________________________________________"   --  38
  ,"___________________________________________________________________________"   --  39
  ,"___________________________________________________________________________"]  --  40
darkNotice' _ 3   1  = reverseH revC $ darkNotice' False 3 (-1)
darkNotice' _ 3 (-1) = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"_______*********************_______________________________________________"  --  21
  ,"_______*********************_______________________________________________"  --  22
  ,"_______*********************_______________________________________________"  --  23
  ,"_______*********************_______________________________________________"  --  24
  ,"_______*********************_______________________________________________"  --  25
  ,"_______*********************_______________________________________________"  --  26
  ,"_______*********************_______________________________________________"  --  27
  ,"_______*********************_______________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40
darkNotice' _ 3   2  = reverseH revC $ darkNotice' False 3 (-2)
darkNotice' _ 3 (-2) = fromTexts '_'
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"___________________________________________________________________________"  --  17
  ,"___________________________________________________________________________"  --  18
  ,"___________________________________________________________________________"  --  19
  ,"___________________________________________________________________________"  --  20
  ,"********___________________________________________________________________"  --  21
  ,"********___________________________________________________________________"  --  22
  ,"********___________________________________________________________________"  --  23
  ,"********___________________________________________________________________"  --  24
  ,"********___________________________________________________________________"  --  25
  ,"********___________________________________________________________________"  --  26
  ,"********___________________________________________________________________"  --  27
  ,"********___________________________________________________________________"  --  28
  ,"___________________________________________________________________________"  --  29
  ,"___________________________________________________________________________"  --  30
  ,"___________________________________________________________________________"  --  31
  ,"___________________________________________________________________________"  --  32
  ,"___________________________________________________________________________"  --  33
  ,"___________________________________________________________________________"  --  34
  ,"___________________________________________________________________________"  --  35
  ,"___________________________________________________________________________"  --  36
  ,"___________________________________________________________________________"  --  37
  ,"___________________________________________________________________________"  --  38
  ,"___________________________________________________________________________"  --  39
  ,"___________________________________________________________________________"] --  40

darkNotice' b n s = darkNotice b n s

inStone :: Bool -> Craphic
inStone False = Craphic $ const $ Draw '#'
inStone True  = fromTextsSGR '_'
  ["###########################################################################"  --   1
  ,"###########################################################################"  --   2
  ,"###########################################################################"  --   3
  ,"###########################################################################"  --   4
  ,"###########################################################################"  --   5
  ,"###########################################################################"  --   6
  ,"###########################################################################"  --   7
  ,"###########################################################################"  --   8
  ,"###########################################################################"  --   9
  ,"###########################################################################"  --  10
  ,"###########################################################################"  --  11
  ,"###########################################################################"  --  12
  ,"###########################################################################"  --  13
  ,"###########################################################################"  --  14
  ,"###########################################################################"  --  15
  ,"###########################################################################"  --  16
  ,"################+-----------------------------------------+################"  --  17
  ,"################|                                         |################"  --  18
  ,"################|     ** YOU ARE IN THE STONE !!! **      |################"  --  19
  ,"################|                                         |################"  --  20
  ,"################+-----------------------------------------+################"  --  21
  ,"###########################################################################"  --  22
  ,"###########################################################################"  --  23
  ,"###########################################################################"  --  24
  ,"###########################################################################"  --  25
  ,"###########################################################################"  --  26
  ,"###########################################################################"  --  27
  ,"###########################################################################"  --  28
  ,"###########################################################################"  --  29
  ,"###########################################################################"  --  30
  ,"###########################################################################"  --  31
  ,"###########################################################################"  --  32
  ,"###########################################################################"  --  33
  ,"###########################################################################"  --  34
  ,"###########################################################################"  --  35
  ,"###########################################################################"  --  36
  ,"###########################################################################"  --  37
  ,"###########################################################################"  --  38
  ,"###########################################################################"  --  39
  ,"###########################################################################"] --  40
----------------------------------------------------------------------------------------
  ["___________________________________________________________________________"  --   1
  ,"___________________________________________________________________________"  --   2
  ,"___________________________________________________________________________"  --   3
  ,"___________________________________________________________________________"  --   4
  ,"___________________________________________________________________________"  --   5
  ,"___________________________________________________________________________"  --   6
  ,"___________________________________________________________________________"  --   7
  ,"___________________________________________________________________________"  --   8
  ,"___________________________________________________________________________"  --   9
  ,"___________________________________________________________________________"  --  10
  ,"___________________________________________________________________________"  --  11
  ,"___________________________________________________________________________"  --  12
  ,"___________________________________________________________________________"  --  13
  ,"___________________________________________________________________________"  --  14
  ,"___________________________________________________________________________"  --  15
  ,"___________________________________________________________________________"  --  16
  ,"________________WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW________________"  --  17
  ,"________________WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW________________"  --  18
  ,"________________WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW________________"  --  19
  ,"________________WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW________________"  --  20
  ,"________________WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW________________"  --  21
  ,"___________________________________________________________________________"]


-- ========================================================================

city :: Craphic
city = fromTexts ' '
  ["HMHk,d$_.     `` ::::_~_ ~::::::~:~~~~~..`   .  ``                                    `  `"   --    1
  ,"MNHMkkI-``     __~::~_   __:~~~~~~~~~._...                                         ` `   `"   --    2
  ,"lvMNWHCXk-...   ` ~~_ .  ..~~~~~~...._......    `     `                           ` ``    "   --    3
  ,"zlldMMMmWkXHHn.   __        `__......````  ````      ` ````                              `"   --    4
  ,"zz==zTMkNXWkUMkn, `..     `    `.. .`.   ````` `     `     `   `  `  `  `  `  `         ` "   --    5
  ,"zz1z==1TNHmUNHHNkU&.         `  `              ``                             ..  `  `   ."   --    6
  ,"Nc1=?1??1TMMHMmZWNgU&.          `                 `        `             ` ._``  ` -..~~_."   --    7
  ,"vTnvzl?1z+?WMNVS?YKWWczmn.XC```                                      `  `` ...`  -_~~~_  ."   --    8
  ,"NgwzOz1++?+>?WMsdNJWqdUd?T4>`    `                  `         `  `  ``` ``.`..__~:;:~__   "   --    9
  ,"NMMMNmzzz+???<?WNdNZTHHZUGwXy,``          `            `  `           ``.  .....~~:~~~____"   --   10
  ,"NHMMMMMMNszvz>><?HHHmlr1TGJ<74A(.   ` `    `` ` `            `  ` ``  ..   .~~__~__:::<~` "   --   11
  ,"MHMN<(HdHMMMNmaJIv7TWHr(_ <(+(?Ww-.` ````      _  `  ``    `  ``      ..   ..~_   .``.__~~"   --   12
  ,"MHMM~-Sd$WM.._?HMMHmazz-(-.JdG_wz(wC_  `` `` .--..  .. `        ```    _.``..___ ``  -_.._"   --   13
  ,"VTUHkJkdmW#.._ wJM!(VVHHm&+v4WdXww+z+-._.   .1<<-__JO1. .   ```   ```  ``. . .`.`.__~~...`"   --   14
  ,"XjVJdjRdVTURa+.kJM`(XOXI_(?UHAZVXku+1+Tz.. (<<<<<jJv<(1i.`  _    `..........~~~~~~~~(__```"   --   15
  ,"KU0dhJRXI==dzdzXVU6dGzXl >..:_(USdvv++<<-?1<X(-+<II++__?zi(O1+<<(~   ....___....`` .WWHWkQ"   --   16
  ,"HHkHWWSXRUWVSWdAkd10??R(jO-.}- z0d1<?<vIaJ+-.(II1zo(+zwZtwXX>+(.(Gu_``.-...&dp3.` .WWWWWWk"   --   17
  ,"F(MNJHZXdZO7TkHkXAAXVOk-u1<(1zzInCz1vv+JdZI<zC<7WWyz+vOVOwVUo+wI(oJo(jOzUmJ+OU+. .VWkHbbHH"   --   18
  ,"bdM#(WId9H0`.#(HHdM`.Z0_??7OwGnc(+???<<??6l<?(vOCXWWAxJ<~(<<+;+<<dyOUwvSyvHHHH8WWVfWWWWbHH"   --   19
  ,"bdMNjdkdIdr..MdMHdH ,OI_ - ( _`(dd-(<_?<zzl(<(<vj0??7TTXfzk?:~<(<v0OdXXWX0XZSXXyXXXWXHkWHg"   --   20
  ,"YT#HyJRd0VWU#W9XHUWJJun/.; . _ (?vC?0><jlc>(_-<<jI+>((<+?1v1+-((jkXXXWqWWWWXXWWXHHkWqHggHH"   --   21
  ,"hWuMbg#?OwdRKuHyXOddzz$(J1vrC1>>_._.._:11z>(-(i+JD=711ju+(A-<<<jWWKXWXWHWkHNHHWHHMHHmHH@NH"   --   22
  ,"MMMMWgMHHWHHMHHHWkWfYTHk+<<<<<<?:++.._(JI+><<??<dfI$++1J76VC<++jkV'''7THHHMMMNWNNHMHHHNMMH"   --   23
  ,"99VOTUWX<<MQkc(gmozJkD~jkknJ.(J-_dv=(r?zzku(>.(>dCC0CXQd>Xm<ukHuWIXbNddkdW#HHXJMHHMMMdMHMM"   --   24
  ,"MMMMHNHXdG&dK0MMHRzdbR:(MMMNr-gk_vz. (jXzZd??jJ<dAOXwHHWIWMz1wUkJoaAzkddwwvM9M#WNMMMMH#MMN"   --   25
  ,"HXMNHMHWdXHd#kNM#HvzH0_(HMNNl-HbTUI+JO&4MtJOz(6<?WykwC7Oms.(jwwdyXWU0WddWXWHHNMHMH#HMMMMMM"   --   26
  ,"NWMM@M#NNTMHdbyMMEwwkZl(HMMMM5++(SXvWOy1C0OIJMHR/Xhgs-_?XHjgHNMMMMMMMHMMNH9VUHMMMMMHHWgqmq"   --   27
  ,"MHMMMNNNHHHMHWHHWWmsXyyzWDTM8<X+XWX-P<r`(HzA.(T9!d&WMh(AHWMMMMMMMMMMMkWNuNmg+WppppbkbHqqqq"   --   28
  ,"NNMNHHMNMMHNHHHHWW#UVY'!(% .jAdzXs1++n-<J@I(<v<<+JdWHXXmgHfVWW0WHHBUQmNNgNNMmAXWbbkkbkkkqk"   --   29
  ,"MMHWBHkXMHH9YTVO<jP<<~_.(L(&JVM9MMNNmZq2V8XWkXW0VwIdWkwUwXuXZVzOZ=!`_7>jMMMWWpWXXUVUWkHkHq"   --   30
  ,"UWkv+1<_Hi-(_(i(_(hJzY'1j@I7iJW?3udMMR(&dUUXUOAQnxzdHWwkVXOwIz?~~ -.~++XVXIwUUUUOOwUXWUUWH"   --   31
  ,"                                                                                          "   --   32
  ,"                                                                                          "   --   33
  ,"                                                                                          "   --   34
  ,"                                                                                          "   --   35
  ,"                                                                                          "   --   36
  ,"                                                                                          "   --   37
  ,"                                                                                          "   --   38
  ,"                                                                                          "   --   39
  ,"                                                                                          "]  --   40
--  123456789012345678901234567890123456789012345678901234567890123456789012345
--           1         2         3         4         5         6         7
--

city2 :: Craphic
city2 = fromTextsA ' ' 'B'
  ["O_.?                                                                              =?????dHWWWlt"   --    1
  ,"Z..(:~                                                                            (????dMMSzSwd"   --    2
  ,"3..(x:                .(,                                                         .????XMXRddH9"   --    3
  ,"..._1;                ~(>                                                         _~~+dM@XWqSWZ"   --    4
  ,"o_..(c              ;jK$v<:                                               (ROwwwrrvrrwWMXWkq0WH"   --    5
  ,"_1i__1              +dHYTz&                                               JHywuzXzzzudM#tXWH9Wr"   --    6
  ,"<._1+(+++?++++<      Xk>+J(<                                           ...dMHZzuzuuXXHMXdX9krdr"   --    7
  ,">x-.(zz====l====?+   XH>dR_~                                        ((((((NWHWrzzzuzXMNX0drXzdX"   --    8
  ,"><j:.(z1z1==v<<~~?   XH>?C((                                       ~drOrrd#WWHROwwXwHMSO0dwdHgH"   --    9
  ,"z+j~`-Z===+_:~-  . ` Xy>+1<.                                       (HZrrrdMMMHWwOrwX#8kdAWHHHkt"   --   10
  ,"?(l_.(Xy====z&_~_-...zu>~(_`                                  ;<j++gNRrrwWHWOWXkZOdM#AXmHHW0HKw"   --   11
  ,".._~.(uXy=???zW+::_~_uX>:((`                                  __dz=dM#tOwMKXwWuWXyW#MHHWkMSybKr"   --   12
  ,".....(ZuXc=??=?Uy+;:_XXC<+(`                                  ~(HI=WW9wwdNKWHHWmHMHMHHudWHkwkRd"   --   13
  ,".....(WWZUx=????Zk=+;XU><<(...._                          ;zzOOdNdd#wXHMM#WWWHHkXHMWWHXdqHWXHkW"   --   14
  ,"-_..-(uuZZXx?=1??vWz?Zu>~(~_~Ju+                          <zCttWSkWNHHVXdHWHXRdKdHNdWHudH@MMHwS"   --   15
  ,"<6i-.duZZZZWz??dmx14eXX&us-_(yuz<:;                       jwVzw#WXWSXHVdMHXKXKdHXWNIXqHHXXdKWXQ"   --   16
  ,"<<+<.dZZXZZuWsz?zWHkdHXuuI.?dHWW6<;;;                    :wXZ1d9wwUwU9GWHWdHdWyXQMMkWdSkWXXWHHH"   --   17
  ,"v1+~~qmmmmmQQQkyz?dOvHkXZI`.uZXZZZZZX<<                  (zzGOHUw0kGgHHSHHZQdHHWWHMM@QkgHgqHHHM"   --   18
  ,"<?1<~XHHHHHUUC?9WkAxz?UdWI..vrwXZZZZ0wk~~                (wzrj#udwXXWWWkHHHWyXWHXHM6WgHHHMHkXKk"   --   19
  ,".~..~XWXVOlzO>(~.._7WkAzWR.,UBYYSZZZCXWJ/                jwuXdRUZwwXUVIzHkXHXKXHwpqVWHWWH#zWHNk"   --   20
  ,"~.~._WVXyllOrI(_+_`.(_-?4X&.11><jZZZ<d0XI:             (_wZuXMAXUwwQAgWHWpXHdKdHdWHUWNwdHNwWkNV"   --   21
  ,"-.~~~<<zC1zOOz(_+_`.+_``< (7TXA;(ZZk:j6k2~            :dOKXXUudXRKWXXHHHXbXHWyyyyXHzXH0JH#UXkNI"   --   22
  ,"I-..~_~~__<~~(d__`.+.```+_-.-z-!?XH3~(+XI~  ~($(XzzzwvwwIUXwXXXWSSUVdNM8XyyUWUUXVWHOdHZdHNOdHMn"   --   23
  ,"U0I~~_~___~~_zd_...l_.`.j>(.<u(~>W#zXW7dI:  :JwdHrwOzzdzkzXZZzrtrrrrduWkwVfXffffVWKOdHHkHHUHMMN"   --   24
  ,"wZz~~~.~~~_~(rd<?_(O2...zo(`(l>`_Z0j_>&<+yXXXWXHHkkwXOWWWU0UXurwwwyrHuUSyf0VwCdkXWKOVMMMZXWMNMH"   --   25
  ,"wwz_~~~~~~~(Wydl>_zd$..(OR<.tXo.-WHdXR?$WXVWHbWWWkHkwzdZSjyWWWkWWMNwwHHMXHk<w>dWfWHXwWWZZZZMMNW"   --   26
  ,"wkz~~~<~~_~(fkdO<(tdw-.IrHI.tZC.-VSZnk((XppWWkHWHkWWdwXZ0=WWWWHH998UuZXWkkkkXmdWWWUZZwzZXWHWHNH"   --   27
  ,"<?<~(+OtwwIzpkdwx(zdko-zvZO-OOwo.dWXWXwSuwkZW9WWHHHQkHWH9AQkQKUUXzMMHHHMH@@@@HHHWkWWkWqHWHMHM@@"   --   28
  ,"(JX0XKSCQdkAOWdZt<rwWHHUUVvWNMNpHHHyWXdwkXHHWWW8HWHMHkddWAkkkmJzvCTMHWWWfbHHHfppbHNHMMMMMMMMHH@"   --   29
  ,"M0UUWHWXXWNQQkHmmnwkOOOv(_,(MNMHHHWWyXXyVfffffffpfpppWMMNNMHwdgZzdM@kzOwwXXWXWWWWWMMMMHMMHHMHHH"   --   30
  ,"                                                                                               "   --   31
  ,"                                                                                               "   --   32
  ,"                                                                                               "   --   33
  ,"                                                                                               "   --   34
  ,"                                                                                               "   --   35
  ,"                                                                                               "   --   36
  ,"                                                                                               "   --   37
  ,"                                                                                               "   --   38
  ,"                                                                                               "   --   39
  ,"                                                                                               "]  --   40
--  123456789012345678901234567890123456789012345678901234567890123456789012345
--           1         2         3         4         5         6         7
--

tavern :: Craphic
tavern = fromTextsA ' ' 'B'
  ["+?;:_~<?1zHWNWb                             ~~:::::;>:;;;<?z>1z?+?=zzz=lOtzvOtrr"   --    1
  ,"z1<<~<(judS+zZ0                            (:<(;:;;;<:;;<+=z>11???=zz?zlllzzZltt"   --    2
  ,"<+<<~~~_(j(-..~                  +         +>>>?><<<z+>>?+=z?zO???zlz?O=OlzvIllt"   --    3
  ,"++<:<<<(iXWWpS_  .              _+       ??=?1z=>?z+z<???1l??z=z?z=tl==zZ=OrZllO"   --    4
  ,"=+;;<(+twkWXpI+-..              .z?===zzl==zzl?l??z+z>?==zOz?zl=??=tl?=lO=OzOllz"   --    5
  ,"1+;<:<1zdHWpWI+-_~              _z1lllzwrllwItzl1z=+I?===zOz+zOl===tl===llwzIlvz"   --    6
  ,"++<<<;zOw0OrwzZ-_~~~_~~~~-_~~_~-(OzrrtwzvIOwZrOl===zw====zOz=ztt=?zrl=lll=wwIlzw"   --    7
  ,"WWUUUUUUUUUUUWgH<~<<_~_____<_~~(<wOzwrwuuOwwwXtw=lzzax1llzOI=ztt==zwl=zOt=wZOl=w"   --    8
  ,"IlOwvOOOOwrwZd@@I<:<<(<___(<:::<<wwuXwXXuZwwwXOOllzdHWztOwwI=Otr=zzrO==lr=OOtl=w"   --    9
  ,"vwwwZOzzzzzzldMHn++z+jHHkHHc::(z+ZXyXXXyZwuwXutrzltdHHzrtwXIlOtvllzXwz=ttlwOrt=w"   --   10
  ,"yrOwwwwOXXvOOdHO1=llOHMMMHHN+<<zluXXXwXXXZXXzZwwz<1dWDRwtwXIlrOullOkvz=rrlrOttlw"   --   11
  ,"zuzzzrwvZC(+dWKzwZzrwXHHHMMSz+1ZzyZVWXfyZuXuXwwOjJuHWXRXOwuZwkwullwwvzlOrtttttlX"   --   12
  ,"ww&AAAgQQQQkWWDOwVXVwdmHHHg01zzwzVyVWWpfWuuuzzZtdbdWKdk0OOVZtrwullvwvvzOwtOzXOOX"   --   13
  ,"MMMM@@@ggHHHWWKUOwUMMMMMNMMMWB=77TC1zOOOrOtOOO3zdbdNX#wwwwzuXwzwz=wXwzzwwtOtzwwZ"   --   14
  ,"NNNMMMMH@MMkWWSwXXWM#HWMMHaJJ---(+<1+;<;;<+11?zzXHMNMHXwwwwwXwZ0XXZuwwwwwwwwzvwX"   --   15
  ,"NN#MMHHHHMMMHHWWHHM#NXXMMkSdH#MHHMMMMHMMMMMMMMHHHH@HHHWwwwuzuwwzXOdSrXXXZyZZXwwX"   --   16
  ,"MMHMM#HMMHMMMMMMMMMMNNNNMN#HNNNHWkgNMMMMMMNkWfWWWH@HHHHXwwZuuwZzwZztvOzwrwuXUUZZ"   --   17
  ,"MHgM#HHMMM#HHH@HMHMNNNNMNNM#N#HNWqMMmqMMNkbMHppWpHHMMHHXZXZZZXZ0wZvOzwzwzruwzzrt"   --   18
  ,"@@gHHHMM#MMHMM@MHMM@@gggM@g@@@MggMNHHWMM#UUT9TTC7z11><11-((++++((-((?+1zwwXvzrvr"   --   19
  ,"@@@HHMHHMMHMNpbHWMMHHHpWWHHkHHqHqHNHKCC<<;;;;>>>??>+<1111zzzz=llllOOIzwwrrrwzzuz"   --   20
  ,"MH@MH@MHMMMMH@MMMMNWyyXZZXWWWWfWpHN@o:<;;<<>>>??????>+zlll?==ztltrvzvrrwOOltrrvz"   --   21
  ,"HHHMH@MH#MHHWmHgH@HUuXXzvvzXXXUWWWM@Mme+<_(+??????11??1==lz1zzz1zzrwztOOttzOttrz"   --   22
  ,"MHHHHMNHMMM#M@@HHMWuvrrOtttrrrvuXMMHUHMMNmge&++;>>>>>>??????????=<<<<<<<<~~_~~(1"   --   23
  ,"MMMMH@HMMMMMMNNNNNuwtllOllltOwQkMHMMMNHHMMMMMMNNmmmQAwzzOzzzttlltz-(++z&-_~~~~~:"   --   24
  ,"HHHkW@M@MNNMH#MMMHuOll=z=11udHBWUZXWWW#bHWHWHmmHMMMMMMM#MMNNNHHQQQQmmQQQQQmmmQQQ"   --   25
  ,"HHggg@MMMMMMMMMHMHuOl=??1?dMIOOvuuXHWMNHHMHHHMH@MMMMMMMMMH#MMHHMMMMMMMMMMMMMMMMM"   --   26
  ,"NNMMMNNM@MHHmHHyyZwO=??>+>j#TWQmmwXNHMHbHMgHHM@MM@@HHHHHMMMNNMMNMMNMNNmkbkpkkkbm"   --   27
  ,"MMMHMMMMHgqkppfWyXwOz??>>zj#=zwrrXXMMMMMHM@mHMH@MMHHfpffWMMMHMMMMMMNWWMMNHbHqHkM"   --   28
  ,"HM@gHHWggqkkpffXuwttl???>+J#?lOwuZXpMM@MHMHHMMMHVpkkbbkWMMkWHMMMMMNNHWkqMMNggHqm"   --   29
  ,"HHMHHpH@qHbbWHWWyXwtllz===d#llOvXZWpHMMMMNkWyWWHHHHHHHMMHgggMMHNMMNMNHHHHMMMgggg"   --   30
  ,"MMMHHHH@HHmHkHpWWuuwttOz=?zHzzzXUyWWWNHMNWHMMMHMg@HHHMNNNMNNMMNMMMNMMHH#MMMM@gg@"   --   31
  ,"                                                                                "   --   32
  ,"                                                                                "   --   33
  ,"                                                                                "   --   34
  ,"                                                                                "   --   35
  ,"                                                                                "   --   36
  ,"                                                                                "   --   37
  ,"                                                                                "   --   38
  ,"                                                                                "   --   39
  ,"                                                                                "]  --   40
--  123456789012345678901234567890123456789012345678901234567890123456789012345
--           1         2         3         4         5         6         7
--

edgeOfTown :: Craphic
edgeOfTown = fromTextsA ' ' 'B'
  ["MMNNMM##MMHMN2~.                                    ~<-&.                 _:~~~<"   --    1
  ,"NNMNM####MHHMHmdHR!                                 (WjZZX..               .`` ."   --    2
  ,"MNMNNNNMHHMHHMHXY3_                               _JV$wXXZZk,         ____~~~~~~"   --    3
  ,"MNNMNNNNNNMMHMMMHp...                            .XWfjXXXZZXXk,_. _-~~~~:~_.__~~"   --    4
  ,"NMNMNNNNNNN#HMHHHk;.~                           .yQmWWkkQkkkXXXG,~~.~~~~~_~~::(:"   --    5
  ,"MNNMNMNNNM#M##HMMMM2~_                        _JWHHW0XuXuXwzXUUV9WkaJ~__:;;;;<;>"   --    6
  ,"MNMNNNN#MMM#MHHMMMM><:                      -(dHVWZX><?+771myzOrrwO<::_:(<?+<+<?"   --    7
  ,"MMMNNNNM###M#HHHf=                         (XWXZXWZ0<_~~:~(M$-__~~.(<<<<<~(J-<<<"   --    8
  ,"NMNNMMMN###HHM#T/                         <~(ZXSuUZC<~~:~~~___:~_~_(<1jJZTz==+(<"   --    9
  ,"NNMNNMNNN#MM##HNs~                         _(XXuVC=z<_~_~:_<~~~:~(JZC<<<<++1=z1z"   --   10
  ,"MMMMNNNMMNM#HMHB3_                         _(VC?????<~~_~__~~~~::::;+<+??<><>>zl"   --   11
  ,"NMMMMMMMMMMN?T<                         >+wOz???1?z1<.(~_~~:~~~~<_;<>>+><>1<1?+="   --   12
  ,"NNNMNNNNNM#H8<                 uAdXXwz??zOwXy+??z?1z<__~__.~~~_~:~<<>>?>?<++?+?1"   --   13
  ,"NMNMMNMNMMMHN?               dWWbWWHkkOOZXXbR>?>?<>1<~~_~(.____~~~;;>?><><><<1?z"   --   14
  ,"MNMMMMMNMMMmzl             twXpHWgHHHSz11OUHD?<<?<<+<____.__~.~_~_:;;;><<?+<<<><"   --   15
  ,"MHMMMMNM#HNOtt           =zdXWHHHHHbkHI(XSdW0<><<<+1<__~_~~~__~~__::;<>+<<<+><<;"   --   16
  ,"MMMNNMMN#MHVOC       <1v?1+zXHHWwZzdXZdZZWyZ0>>>??+=>_____~~-~_~(_~(:;;;;;;>>>1+"   --   17
  ,"MM#H#MNNMM3(;      dWWWWkZXSWHHUUZOtwwwZXZyW0+++<>?1<.__~~~~__~__~_:<;;v;;;><>>?"   --   18
  ,"MMMNMMMN1?1zz1uJHWWXwXyywzOOUUSVOzzzwwwXuwXXI<>;<>++<__-~._~-~~.(~<:<++><>><<+??"   --   19
  ,"HMMMMmxzgdvCUXVXIO0OzwXrr<1zOOI1<>>zwOtwXX0OC<><<?1+<--~__<__.(_~_(<<<;?<>;>>+?+"   --   20
  ,"MNMMMMSOWS><OOv?I1Zz+zzI<:<<+O?<+++lOzdWmHdvz++<+<1zC______~~~_ ~_._---~-.____-."   --   21
  ,"HMMMMMMWkR-((<:jz=+<<1Xkc+&zzOZ<jzdWKwWMMMWHR>><><??<-__.~~__~~~_______-___~___-"   --   22
  ,"HMMMNMMHagZCOuSyzuz<><dZjIXWuZ1+vVC4WwdMHWHH#<<;+>+?<_.~_~__~__:_(~__~~_.___~:_-"   --   23
  ,"NHNNMHQkWHHM@mJv7=<:~(wwXwWWfW&vddsZwwX8OdHHk<<><><<>__~~._(-- -..._-.._____.___"   --   24
  ,"N#N#HHMHHHMHkWWHHpmx-.~?77'Tv7(WWSrOWXwOwHHWD<><>>>1<.-~~_~~<!_...-_ __._.--._._"   --   25
  ,"NN#NNN#MMMHMMHNHWVyZyk>`  `_._~!~~1w0wkXVKXky(<<<?><<..~~_(_-_-.._..`~.__-(2___~"   --   26
  ,"MNNNN####HHHHMHMHbWWkk>_       ~_<-(?wWHkWHW$>><<<;<<_.~____.._(<<__~__~___<___~"   --   27
  ,"NNNNNNNNN###HHHHHHMHHkH&+_____~_<i1+++1OZXWXS;><>;>><.~.._....__.____._________~"   --   28
  ,"MMNNMNN##MH###H#HH#HHHHbkHkae++dkyWXWXWkHHH0D<>>;><<<....._..._.______._._~____~"   --   29
  ,"NNMNNNNNNN###HMNMM#H##HMHkpHXXkbkHWWWWWWWH8C!_<<J<>><-..`_..-_. ~.._..._...._-~~"   --   30
  ,"MMNMMNNNNNNN#MMMHMMMMMMgMMHWWWWWWHbHHWHkRyr.  ..<_!<<_._-_.._....__..__....____~"   --   31
  ,"                                                                                "   --   32
  ,"                                                                                "   --   33
  ,"                                                                                "   --   34
  ,"                                                                                "   --   35
  ,"                                                                                "   --   36
  ,"                                                                                "   --   37
  ,"                                                                                "   --   38
  ,"                                                                                "   --   39
  ,"                                                                                "]  --   40
--  123456789012345678901234567890123456789012345678901234567890123456789012345
--           1         2         3         4         5         6         7
--

-- ========================================================================

treasureChest :: Craphic
treasureChest = fromTextsSGR ' '
  ["                                                                           "   --   1
  ,"                                                                           "   --   2
  ,"                                                                           "   --   3
  ,"                                                                           "   --   4
  ,"                                                                           "   --   5
  ,"                                                                           "   --   6
  ,"                                                                           "   --   7
  ,"                                                                           "   --   8
  ,"                                                                           "   --   9
  ,"                                                                           "   --   10
  ,"                                                                           "   --   11
  ,"                                                                           "   --   12
  ,"                                                                           "   --   13
  ,"                                                                           "   --   14
  ,"                                                                           "   --   15
  ,"                                                                           "   --   16
  ,"                                                                           "   --   17
  ,"                                                                           "   --   18
  ,"                                                                           "   --   19
  ,"                                                                           "   --   20
  ,"                                                                           "   --   21
  ,"                                                                           "   --   22
  ,"                                MM@MHHbkk+?4bbWHh_7                        "   --   23
  ,"                    MNggK7TH@@Hm+JWHWfWpfWW,,yWWWW,d                       "   --   24
  ,"                  NNN#HHHHa-?pHWfhYdkfWWfWWW2dpWWpb_                       "   --   25
  ,"                  #H#MHHHHMN,,WfWpk1dHHppWpbN-jkWkM~,                      "   --   26
  ,"                 MMHH#H@HHH@N??pbHq[-WHNAJjZ9-,1uggl(                      "   --   27
  ,"                 M#MMMkMMMMM#[.WH9Y9..JdW3JWM-(HHpbr-                      "   --   28
  ,"                 MHMMHHN#MNNH8?1QggH_(HMNW+WH<WWkWW$(                      "   --   29
  ,"                 #MHMMMMMM##MH+jkWbW:(MHWHWWH_ZHHHW]-                      "   --   30
  ,"                 #HHMMNNMMM#MMHdKHpW<(HbWVVWHdwHNkWHM                      "   --   31
  ,"                 #H#MHMMHHHHHXjdWWWHe(HkfWHWH0xMMHHSd                      "   --   32
  ,"                 M#H#HMHMH@@@VWdQHkHzWNHpWHMM(RSm2C~J                      "   --   33
  ,"                   ##M@MMM@@M0HdMNHHjXHBkg?7>((NggggggH                    "   --   34
  ,"                     NKMMMMMM0HdMMkd9RQQ.gN      NNNN                      "   --   35
  ,"                        NNMHM#WHdmg  gggg                                  "   --   36
  ,"                          Ngggggggggggggmgm                                "   --   37
  ,"                               NNgggggHHggggg                              "   --   38
  ,"                                                                           "   --   39
  ,"                                                                           "]  --   40
  ------------------------------------------------------------------------------------
  ["                                                                           "   --   1
  ,"                                                                           "   --   2
  ,"                                                                           "   --   3
  ,"                                                                           "   --   4
  ,"                                                                           "   --   5
  ,"                                                                           "   --   6
  ,"                                                                           "   --   7
  ,"                                                                           "   --   8
  ,"                                                                           "   --   9
  ,"                                                                           "   --   10
  ,"                                                                           "   --   11
  ,"                                                                           "   --   12
  ,"                                                                           "   --   13
  ,"                                                                           "   --   14
  ,"                                                                           "   --   15
  ,"                                                                           "   --   16
  ,"                                                                           "   --   17
  ,"                                                                           "   --   18
  ,"                                                                           "   --   19
  ,"                                                                           "   --   20
  ,"                                                                           "   --   21
  ,"                                                                           "   --   22
  ,"                                ###################                        "   --   23
  ,"                    ################################                       "   --   24
  ,"                  ##################################                       "   --   25
  ,"                  ##################################B                      "   --   26
  ,"                 ####################################                      "   --   27
  ,"                 ####################################                      "   --   28
  ,"                 ####################################                      "   --   29
  ,"                 ####################################                      "   --   30
  ,"                 ####################################                      "   --   31
  ,"                 ####################################                      "   --   32
  ,"                 BB#############################BBBBB                      "   --   33
  ,"                   BB#######################BBBBBBBBBBB                    "   --   34
  ,"                     BB##################BB      BBBB                      "   --   35
  ,"                        BB###########BBBB                                  "   --   36
  ,"                          BBBBBBBBBBBBBBBBB                                "   --   37
  ,"                               BBBBBBBBBBBBBB                              "   --   38
  ,"                                                                           "   --   39
  ,"                                                                           "]  --   40
--  123456789012345678901234567890123456789012345678901234567890123456789012345
--           1         2         3         4         5         6         7


treasure :: Craphic
treasure = fromTexts ' '
  ["                                                                           "   --   1
  ,"                                                                           "   --   2
  ,"                                                                           "   --   3
  ,"                                                                           "   --   4
  ,"                                                                           "   --   5
  ,"                                                                           "   --   6
  ,"                                                                           "   --   7
  ,"                                                                           "   --   8
  ,"                                                                           "   --   9
  ,"                                                                           "   --   10
  ,"                                                                           "   --   11
  ,"                                                                           "   --   12
  ,"                                                                           "   --   13
  ,"                                                                           "   --   14
  ,"                                                                           "   --   15
  ,"                                                                           "   --   16
  ,"                                                                           "   --   17
  ,"                                                                           "   --   18
  ,"                                                                           "   --   19
  ,"                                                                           "   --   20
  ,"                                                                           "   --   21
  ,"                                                                           "   --   22
  ,"                                                                           "   --   23
  ,"                                                                           "   --   24
  ,"                                                                           "   --   25
  ,"                                                                           "   --   26
  ,"                                         .JMN...                           "   --   27
  ,"                                        ..MMMMMMNn,                        "   --   28
  ,"                                     .MMMMMMMMMHUd#                        "   --   29
  ,"                                     .NM#MMMMMB#XyR                        "   --   30
  ,"                            (Wk..  ` .MM@MMMMMNkM#H,                       "   --   31
  ,"                             (HHW&+ggmMMHMMMMMMM#dkr                       "   --   32
  ,"                          ..JJMMNMMNMMMNNMMMMMMMQgKX:                      "   --   33
  ,"                   qgHMMMMMNNMHMMMNMMNMNHMMNmQe    ?M@Wm-                  "   --   34
  ,"                  7TNMMMMMMMMMNWHHMMMMMMMMMMMNNmNk:w?SdMN,                 "   --   35
  ,"              7HY^ 7'= dNKoJMMBVMMMNMMMNNMNMMNKYWMMMMHMMM'4Nm              "   --   36
  ,"                    dMNMMNWN#5MMMHWHMMNMNNMMNMMNMMMNMMmJ, ....             "   --   37
  ,"                `         ''! jNgMMMNMMMmMMNMMMMMHN#B77?                   "   --   38
  ,"                            JMMMMMMMMMMHMMMHMMM                            "   --   39
  ,"                             _TMM=?MMMNQ}`     gMN+                        "   --   40
  ,"                                   (WM#'`      7''`                        "]  --   41
--  123456789012345678901234567890123456789012345678901234567890123456789012345
--           1         2         3         4         5         6         7


