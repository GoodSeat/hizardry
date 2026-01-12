module SampleScenario.Events where

import PreludeL
import qualified Data.Map as Map
import qualified Data.GameEvent as Ev

import Data.Primitive
import Data.Maze
import Data.Formula
import Data.PlayEvent

mazeEvents :: Ev.DB
mazeEvents = Map.fromList [
      (GameEventID 000100, Ev.Events [
         Ev.ChangeJob Ev.Leader "Ninja"
      ])

    , (GameEventID 000102, Ev.Events [
         Ev.ChangeHP Ev.All (parse' "10d10")
      ])

    , (GameEventID 010100, Ev.Events [
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
         Ev.Ask "what's your name?" (Just $ Single $ PictureID 1001) [
           ("werdna", Ev.Message "OH MY GOD!" (Just $ Single $ PictureID 1002))
         , ("", Ev.Message "who?" (Just $ Single $ PictureID 1001))
         ]
       ])

    -- like NPC
    , (GameEventID 010102, 
--       Ev.Message "何者かが近づいてきた。" Nothing
         Ev.PlayBGM Encounter <> Ev.FlashMessageTime  "    Encounter!!    " (-1000)
      <> Ev.PlayBGM (EventBGM "themeOfSoleil")
      <> Ev.MessageT (-15) "私はデバッグ用NPC\n\nHaskellを賛美せよ!!" (Just $ Single $ PictureID 1002)
      <> Ev.Reference (GameEventID 010104)
       )
    , (GameEventID 010104, 
         Ev.Select "Party's Option\n  ^T)alk  ^L)eave" (Just $ Single $ PictureID 1002)
         [("l", Ev.MessageT (-15) "さらばだ！！" (Just $ Single $ PictureID 1002) <> Ev.PlayBGM Ambient)
         ,("t", Ev.Reference (GameEventID 010103))
         ]
       )
    , (GameEventID 010103, Ev.Ask "何について話す？ (say \"bye\" to exit.)" (Just $ Single $ PictureID 1002)
         [ ("hello\nhi\nこんにちは", Ev.MessageT (-15) "私はデバッグ用NPC\n\nHaskellを賛美せよ!!" (Just $ Single $ PictureID 1002)
                      <> Ev.Reference (GameEventID 010103))
         , ("name" , Ev.MessageT (-15) "名前はまだない。" (Just $ Single $ PictureID 1002)
                  <> Ev.Reference (GameEventID 010103))
         , ("haskell", Ev.MessageT (-15) "Haskellはこの世界を作っている言語だ。\nつまり神の言語だ!!" (Just $ Single $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("god\n神\nかみ", Ev.MessageT (-15) "まぁ私もよく分からず言っている。" (Just $ Single $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("fight", Ev.MessageT (-15) "私は平和主義者だ。\n戦いは好まない。" (Just $ Single $ PictureID 1002)
                  <> Ev.Reference (GameEventID 010103))
         , ("dance"  , Ev.MessageTimeT (-15) "\nそれなら知っている.\n" (Just $ Single $ PictureID 1002) (-500)
                    <> Ev.MessageTime        "\nそれなら知っている..\n" (Just $ Single $ PictureID 1002) (-500)
                    <> Ev.MessageTime        "\nそれなら知っている...\n" (Just $ Single $ PictureID 1002) (-500)
                    <> Ev.MessageTime        "\nWNWSEENE\n\nだ。" (Just $ Single $ PictureID 1002) 500
                    <> Ev.MessageT (-15) "これをある場所で踏むのだ。" (Just $ Single $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("place" ,  Ev.MessageT (-15) "自分で探すのだ!" (Just $ Single $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("くれ" ,  Ev.MessageT (-15) "強欲な奴だ、これをやろう。" (Just $ Single $ PictureID 1002)
                    <> Ev.GetItem Ev.Leader (read "3") True [
                         Ev.PlayBGM (EventBGMOnce "getitem")
                      <> Ev.Message "あなたは水を手に入れた。"
                         (Just $ List [Clip (Trans 0 (-10) (Single $ PictureID 0002)) (Single $ PictureID 0051), Single (PictureID 0051), Single (PictureID 1002)])
                      <> Ev.PlayBGM (EventBGM "themeOfSoleil")
                       , Ev.MessageT (-15) "お前、もう持てないぞ、強欲すぎるだろ" (Just $ Single $ PictureID 1002)
                       ]
                    <> Ev.Reference (GameEventID 010103))
         , ("みず" ,  Ev.MessageT (-15) "強欲な奴だ、これをやろう。" (Just $ Single $ PictureID 1002)
                    <> Ev.GetItem Ev.All (read "3") True [
                         Ev.PlayBGM (EventBGMOnce "getitem")
                      <> Ev.Message "あなたは水を手に入れた。"
                         (Just $ List [Clip (Trans 0 (-10) (Single $ PictureID 0002)) (Single $ PictureID 0051), Single (PictureID 0051), Single (PictureID 1002)])
                      <> Ev.PlayBGM (EventBGM "themeOfSoleil")
                       , Ev.MessageT (-15) "お前、もう持てないぞ、強欲すぎるだろ" (Just $ Single $ PictureID 1002)
                       ]
                    <> Ev.Reference (GameEventID 010103))

         , ("goodbye\nbye", Ev.MessageT (-15) "またいつでも来ると良い!!" (Just $ Single $ PictureID 1002) <> Ev.Reference (GameEventID 010104))
         , ("castle\nしろ" , Ev.SelectT (-15) "なんだ、城に帰りたいのか？\n(^Y/^N)" (Just $ Single $ PictureID 1002)
                       [("y",
                            Ev.MessageTimeT (-15) "\nちょっと待っとれ."   (Just $ Single $ PictureID 1002) (500)
                         <> Ev.MessageTime        "\nちょっと待っとれ.."  (Just $ Single $ PictureID 1002) (500)
                         <> Ev.MessageTime        "\nちょっと待っとれ..." (Just $ Single $ PictureID 1002) (500)
                         <> Ev.MessageTimeT (-10) "\nMAPILO MAHAMA DILOMAT!!" (Just $ Single $ PictureID 1002) 750
                         <> Ev.MessageTime        "\nMAPILO MAHAMA DILOMAT!! だったかな?" (Just $ Single $ PictureID 1002) 300
                         <> Ev.PlayBGM Ambient <> Ev.ReturnCastle)
                       ,("n",
                           Ev.MessageT (-15) "そうなの?" (Just $ Single $ PictureID 1002) <> Ev.Reference (GameEventID 010103))
                       ])
         , ("\n", Ev.MessageT (-15) "またいつでも来ると良い!!" (Just $ Single $ PictureID 1002) <> Ev.Reference (GameEventID 010104))
         , ("", Ev.MessageT (-15) "それは知らない..." (Just $ Single $ PictureID 1002) <> Ev.Reference (GameEventID 010103))
         ]
      )

    -- spell
    , (GameEventID 070001, 
         Ev.MessageT 10 "you call me?" (Just $ Single $ PictureID 1002)
      <> Ev.Select "you call me?\n  ^Y)es  ^N)o" (Just $ Single $ PictureID 1002)
         [("n", Ev.MessageT (-15) "good by!" (Just $ Single $ PictureID 1002) <> Ev.End)
         ,("y", Ev.Switch [(Ev.PartyPositionIs [Position E 1 2 0], 
                            Ev.AsSpell (SpellID 73) <> Ev.MessageT (-15) "how about this spell!?       \n...bye!" (Just $ Single $ PictureID 1002)
                            )
                          ,(Ev.Otherwise, Ev.MessageT 10 "this position is Invalid..." (Just $ Single $ PictureID 1002))]
          )
         ]
       )
    , (GameEventID 070002, 
         Ev.MessageT 10 "you call me?" (Just $ Single $ PictureID 1002)
      <> Ev.Select "you call me?\n  ^Y)es  ^N)o" (Just $ Single $ PictureID 1002)
         [("n", Ev.MessageT (-15) "good by!" (Just $ Single $ PictureID 1002) <> Ev.End)
         ,("y", Ev.Switch [(Ev.PartyPositionIs [Position E 1 2 0], 
                            Ev.MessageT (-15) "how about this spell!?       \n...bye!" (Just $ Single $ PictureID 0) <> Ev.ReturnCastle
                            )
                          ,(Ev.Otherwise, Ev.MessageT 10 "this position is Invalid..." (Just $ Single $ PictureID 1002))]
          )
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
             Ev.Message "Your dance is NICE!!!" (Just $ Single $ PictureID 1002)
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

    -- find secret door
    , (GameEventID 03000101, Ev.ChangeEventFlag 2 (parse' "1") <> Ev.Message "Wow!?" Nothing)
    , (GameEventID 02010101, Ev.Switch [
         ( Ev.FormulaCheckParty (parse' "(evf.2=0)*100"), Ev.ChangeEventFlag 2 (parse' "1") <> Ev.Message "You found door." Nothing)
        ,( Ev.Otherwise, Ev.Message "You found nothing." Nothing)
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
    , (Position W 3 0 0, GameEventID 03000101)
    ]

