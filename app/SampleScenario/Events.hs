module SampleScenario.Events where

import qualified Data.Map as Map
import qualified Data.GameEvent as Ev

import Data.Primitive
import Data.Maze
import Data.Formula

mazeEvents :: Ev.DB
mazeEvents = Map.fromList [
      (GameEventID 010100, Ev.Events [
         Ev.Select "there is climbing stairs.\n...climbing?\n\n(^Y/^N)" Nothing [
           ("y", Ev.ReturnCastle), ("n", Ev.Escape)]
       ])
    , (GameEventID 020400, Ev.Events [
         Ev.Select "there is ladder to go down.\n...go down?\n\n(^Y/^N)" Nothing [
           ("y", Ev.StairsToLower (1, 3, 1) <> Ev.End)
         , ("n", Ev.Escape)
         ]
       ])
    , (GameEventID 020401, Ev.Events [
         Ev.Select "there is ladder to go up.\n...go up?\n\n(^Y/^N)" Nothing [
           ("y", Ev.StairsToUpper (1, 3, 0) <> Ev.End)
         , ("n", Ev.Escape)
         ]
       ])
    , (GameEventID 010101, Ev.Events [
         Ev.Ask "what's your name?" (Just $ PictureID 1001) [
           ("werdna", Ev.Message "OH MY GOD!" (Just $ PictureID 1002))
         , ("", Ev.Message "who?" (Just $ PictureID 1001))
         ]
       ])

    -- like NPC
    , (GameEventID 010102, 
         Ev.Message "何者かが近づいてきた。" Nothing
      <> Ev.MessageT (-15) "私はデバッグ用NPC\n\nHaskellを賛美せよ!!" (Just $ PictureID 1002)
      <> Ev.Reference (GameEventID 010104)
       )
    , (GameEventID 010104, 
         Ev.Select "Party's Option\n  ^T)alk  ^L)eave" (Just $ PictureID 1002)
         [("l", Ev.MessageT (-15) "さらばだ！！" (Just $ PictureID 1002))
         ,("t", Ev.Reference (GameEventID 010103))
         ]
       )
    , (GameEventID 010103, Ev.Ask "何について話す？ (say \"bye\" to exit.)" (Just $ PictureID 1002)
         [ ("hello\nhi", Ev.MessageT (-15) "私はデバッグ用NPC\n\nHaskellを賛美せよ!!" (Just $ PictureID 1002)
                      <> Ev.Reference (GameEventID 010103))
         , ("name" , Ev.MessageT (-15) "名前はまだない。" (Just $ PictureID 1002)
                  <> Ev.Reference (GameEventID 010103))
         , ("haskell", Ev.MessageT (-15) "Haskellはこの世界を作っている言語だ。\nつまり神の言語だ!!" (Just $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("god\n神", Ev.MessageT (-15) "まぁ私もよく分からず言っている。" (Just $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("fight", Ev.MessageT (-15) "私は平和主義者だ。\n戦いは好まない。" (Just $ PictureID 1002)
                  <> Ev.Reference (GameEventID 010103))
         , ("dance"  , Ev.MessageTimeT (-15) "\nそれなら知っている.\n" (Just $ PictureID 1002) (-500)
                    <> Ev.MessageTime        "\nそれなら知っている..\n" (Just $ PictureID 1002) (-500)
                    <> Ev.MessageTime        "\nそれなら知っている...\n" (Just $ PictureID 1002) (-500)
                    <> Ev.MessageTime        "\nWNWSEENE\n\nだ。" (Just $ PictureID 1002) 500
                    <> Ev.MessageT (-15) "これをある場所で踏むのだ。" (Just $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("place" ,  Ev.MessageT (-15) "自分で探すのだ!" (Just $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))

         , ("goodbye\nbye", Ev.MessageT (-15) "またいつでも来ると良い!!" (Just $ PictureID 1002) <> Ev.Reference (GameEventID 010104))
         , ("castle" , Ev.SelectT (-15) "なんだ、城に帰りたいのか？\n(^Y/^N)" (Just $ PictureID 1002)
                       [("y",
                            Ev.MessageTimeT (-15) "\nちょっと待っとれ."   (Just $ PictureID 1002) (500)
                         <> Ev.MessageTime        "\nちょっと待っとれ.."  (Just $ PictureID 1002) (500)
                         <> Ev.MessageTime        "\nちょっと待っとれ..." (Just $ PictureID 1002) (500)
                         <> Ev.MessageTimeT (-10) "\nMAPILO MAHAMA DILOMAT!!" (Just $ PictureID 1002) 750
                         <> Ev.MessageTime        "\nMAPILO MAHAMA DILOMAT!! だったかな?" (Just $ PictureID 1002) 300 <> Ev.ReturnCastle)
                       ,("n",
                           Ev.MessageT (-15) "そうなの?" (Just $ PictureID 1002) <> Ev.Reference (GameEventID 010103))
                       ])
         , ("", Ev.MessageT (-15) "それは知らない..." (Just $ PictureID 1002)
             <> Ev.Reference (GameEventID 010103))
         ]
      )

    -- dance event on (1, 5, 0)
    --         WNWSE->NE
    --    step:123456 78
    -- reset flags
    , (GameEventID 01050100, Ev.Events [
         Ev.ChangeEventFlag 1 (parse' "0")
       , Ev.ChangeEventFlag 2 (parse' "0")
       ])
    -- dance stap1, 3
    , (GameEventID 01050101, Ev.Switch [
         ( Ev.FormulaCheckParty (parse' "(evf.1=2)*100")
         , Ev.ChangeEventFlag 1 (parse' "3")
         ),
         ( Ev.Otherwise
         , Ev.ChangeEventFlag 1 (parse' "1")
         )
      ])
    -- dance stap2
    , (GameEventID 01050102, Ev.Switch [
         ( Ev.FormulaCheckParty (parse' "(evf.1=1)*100")
         , Ev.ChangeEventFlag 1 (parse' "2")
         ),
         ( Ev.Otherwise, Ev.Reference (GameEventID 01050100))
       ])
    -- dance stap4
    , (GameEventID 01050103, Ev.Switch [
         ( Ev.FormulaCheckParty (parse' "(evf.1=3)*100")
         , Ev.ChangeEventFlag 1 (parse' "4")
         ),
         ( Ev.Otherwise, Ev.Reference (GameEventID 01050100))
       ])
    -- dance stap5
    , (GameEventID 01050104, Ev.Switch [
         ( Ev.FormulaCheckParty (parse' "(evf.1=4)*100")
         , Ev.ChangeEventFlag 1 (parse' "5")
         ),
         ( Ev.Otherwise, Ev.Reference (GameEventID 01050100))
       ])
    -- dance stap6,8
    , (GameEventID 01050105, Ev.Switch [
         ( Ev.FormulaCheckParty (parse' "(evf.1=5)*100")
         , Ev.ChangeEventFlag 1 (parse' "6")
         ),
         ( Ev.FormulaCheckParty (parse' "(evf.1=7)*100")
         , Ev.Events [
             Ev.Message "Your dance is NICE!!!" (Just $ PictureID 1002)
           , Ev.Reference (GameEventID 01050100)
         ]
         ),
         ( Ev.Otherwise, Ev.Reference (GameEventID 01050100))
       ])
    -- dance stap7
    , (GameEventID 01050106, Ev.Switch [
         ( Ev.FormulaCheckParty (parse' "(evf.1=6)*100")
         , Ev.ChangeEventFlag 1 (parse' "7")
         ),
         ( Ev.Otherwise, Ev.Reference (GameEventID 01050100))
       ])

    ]

eventMap :: Map.Map Coord GameEventID
eventMap = Map.fromList [
      ((0, 0, 0), GameEventID 010100)
    , ((1, 3, 0), GameEventID 020400)
    , ((1, 3, 1), GameEventID 020401)
    , ((0, 0, 1), GameEventID 010101)
    , ((3, 0, 1), GameEventID 010102)
    ]

eventMapDir :: Map.Map Position GameEventID
eventMapDir = Map.fromList [
      (Position W 0 4 0, GameEventID 01050101)
    , (Position N 0 4 0, GameEventID 01050102)
    , (Position S 0 4 0, GameEventID 01050103)
    , (Position E 0 4 0, GameEventID 01050104)
    , (Position E 1 4 0, GameEventID 01050105)
    , (Position N 1 4 0, GameEventID 01050106)
    ]

