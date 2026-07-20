{-# LANGUAGE OverloadedStrings #-}
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
         Ev.Select (EnJp "there is climbing stairs.\n...climbing?\n\n(^Y/^N)"
                         "上に登る階段がある。\n...登りますか？\n\n(^Y/^N)") Nothing [
           ("y", Ev.ReturnCastle), ("n", Ev.Escape)]
       ])
    , (GameEventID 020400, Ev.Events [
         Ev.Select (EnJp "there is ladder to go down.\n...go down?\n\n(^Y/^N)"
                         "下へ降りる梯子がある。\n...降りますか？\n\n(^Y/^N)") Nothing [
           ("y", Ev.StairsToLower (1, 3, 1) <> Ev.End)
         , ("n", Ev.Escape)
         ]
       ])
    , (GameEventID 020401, Ev.Events [
         Ev.Select (EnJp "there is ladder to go up.\n...go up?\n\n(^Y/^N)"
                         "上へ登る梯子がある。\n...登りますか？\n\n(^Y/^N)") Nothing [
           ("y", Ev.StairsToUpper (1, 3, 0) <> Ev.End)
         , ("n", Ev.Escape)
         ]
       ])

    , (GameEventID 000401, Ev.Events [
         Ev.Select (EnJp "there is ladder to go down.\n...go down?\n\n(^Y/^N)"
                         "下へ降りる梯子がある。\n...降りますか？\n\n(^Y/^N)") Nothing [
           ("y", Ev.StairsToLower (0, 4, 2) <> Ev.End)
         , ("n", Ev.Escape)
         ]
       ])
    , (GameEventID 000402, Ev.Events [
         Ev.Select (EnJp "there is ladder to go up.\n...go up?\n\n(^Y/^N)"
                         "上へ登る梯子がある。\n...登りますか？\n\n(^Y/^N)") Nothing [
           ("y", Ev.StairsToUpper (0, 4, 1) <> Ev.End)
         , ("n", Ev.Escape)
         ]
       ])

    , (GameEventID 010101, Ev.Events [
         Ev.Ask (EnJp "what's your name?" "お前の名前は？") (Just $ Single $ PictureID 1001) [
           ("werdna", Ev.Message (EnJp "OH MY GOD!" "おお、何ということだ！") (Just $ Single $ PictureID 1002))
         , ("", Ev.Message (EnJp "who?" "誰だ？") (Just $ Single $ PictureID 1001))
         ]
       ])

    , (GameEventID 000003, 
         Ev.Message (EnJp "It's pure water." "綺麗な水だ。")
         (Just $ List [Clip (Trans 0 (-10) (Single $ PictureID 0002)) (Single $ PictureID 0051), Single (PictureID 0051)])
      )

    -- like NPC
    , (GameEventID 010102, 
--       Ev.Message "何者かが近づいてきた。" Nothing
         Ev.PlayBGM Encounter <> Ev.FlashMessageTime (EnJp "    Encounter!!    " "    遭遇！！    ") (-1000)
      <> Ev.PlayBGM (EventBGM "themeOfSoleil")
      <> Ev.MessageT (-15) (EnJp "I am a debug NPC.\n\nPraise Haskell!!"
                                 "私はデバッグ用NPC\n\nHaskellを賛美せよ!!") (Just $ Single $ PictureID 1002)
      <> Ev.Reference (GameEventID 010104)
       )
    , (GameEventID 010104, 
         Ev.SelectC (EnJp "Debug NPC" "デバッグ用NPC")
                    (EnJp "Party's Option\n\n  ^T)alk\n  ^G)ive Item\n  ^L)eave"
                          "パーティの行動\n\n  ^T)話す\n  ^G)アイテムを渡す\n  ^L)立ち去る") (Just $ Single $ PictureID 1002)
         [("l", Ev.MessageT (-15) (EnJp "Farewell!!" "さらばだ！！") (Just $ Single $ PictureID 1002) <> Ev.PlayBGM Ambient <> Ev.Escape)
         ,("g", Ev.Reference (GameEventID 010105))
         ,("t", Ev.Reference (GameEventID 010103))
         ]
       )
    , (GameEventID 010105, 
         Ev.SelectItem (EnJp "Debug NPC" "デバッグ用NPC") (Just $ Single $ PictureID 1002)
         [(Just (read "3")
              , Ev.MessageT (-15) (EnJp "Isn't this the water I gave you?\nWell, I'll take it."
                                         "私があげた水じゃないか・・・\nまぁ、貰っておくけど。") (Just $ Single $ PictureID 1002)
             <> Ev.LostItem Ev.Leader (read "3") [Ev.Reference (GameEventID 010105)]
             <> Ev.Reference (GameEventID 010105))
         ,(Nothing
              , Ev.MessageT (-15) (EnJp "What about it?" "それがどうしたのだ？") (Just $ Single $ PictureID 1002)
             <> Ev.Reference (GameEventID 010105))
         ]
         <> Ev.Reference (GameEventID 010104)
       )
    , (GameEventID 010103, Ev.Ask (EnJp "What do you want to talk about? (say \"bye\" to exit.)"
                                        "何について話す？ (終了するには \"bye\" と入力)") (Just $ Single $ PictureID 1002)
         [ ("hello\nhi\nこんにちは", Ev.MessageT (-15) (EnJp "I am a debug NPC.\n\nPraise Haskell!!"
                                                               "私はデバッグ用NPC\n\nHaskellを賛美せよ!!") (Just $ Single $ PictureID 1002)
                      <> Ev.Reference (GameEventID 010103))
         , ("name" , Ev.MessageT (-15) (EnJp "I have no name yet." "名前はまだない。") (Just $ Single $ PictureID 1002)
                  <> Ev.Reference (GameEventID 010103))
         , ("haskell", Ev.MessageT (-15) (EnJp "Haskell is the language this world is made of.\nIn other words, the language of God!!"
                                                "Haskellはこの世界を作っている言語だ。\nつまり神の言語だ!!") (Just $ Single $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("god\n神\nかみ", Ev.MessageT (-15) (EnJp "Well, I'm saying it without knowing much myself."
                                                 "まぁ私もよく分からず言っている。") (Just $ Single $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("fight", Ev.MessageT (-15) (EnJp "I am a pacifist.\nI do not like fighting."
                                               "私は平和主義者だ。\n戦いは好まない。") (Just $ Single $ PictureID 1002)
                  <> Ev.Reference (GameEventID 010103))
         , ("battle", Ev.MessageT (-15) (EnJp "If you wish to fight, so be it."
                                                "戦いたいのなら仕方ない。") (Just $ Single $ PictureID 1002)
                   <> Ev.StartBattle (read "3")
                      (Ev.PlayBGM TurnOff <>
                       Ev.MessageT (-15) (EnJp "Against violence!" "暴力反対!") (Just $ Single $ PictureID 1002) <>
                       Ev.PlayBGM Ambient <>
                       Ev.Escape
                       )
                      (Ev.PlayBGM TurnOff <>
                       Ev.MessageT (-15) (EnJp "Don't challenge if you're just going to run away."
                                                "逃げるくらいなら挑むな。") (Just $ Single $ PictureID 1002) <>
                       Ev.PlayBGM (EventBGM "themeOfSoleil") <>
                       Ev.Reference (GameEventID 010103))
                       )
         , ("dance"  , Ev.MessageTimeT (-15) (EnJp "\nI know about that.\n" "\nそれなら知っている.\n") (Just $ Single $ PictureID 1002) (-500)
                    <> Ev.MessageTime        (EnJp "\nI know about that..\n" "\nそれなら知っている..\n") (Just $ Single $ PictureID 1002) (-500)
                    <> Ev.MessageTime        (EnJp "\nI know about that...\n" "\nそれなら知っている...\n") (Just $ Single $ PictureID 1002) (-500)
                    <> Ev.MessageTime        (EnJp "\nIt's WNWSEENE.\n" "\nWNWSEENE\n\nだ。") (Just $ Single $ PictureID 1002) 500
                    <> Ev.MessageT (-15) (EnJp "Step on it at a certain place."
                                               "これをある場所で踏むのだ。") (Just $ Single $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("place" ,  Ev.MessageT (-15) (EnJp "Find it yourself!" "自分で探すのだ!") (Just $ Single $ PictureID 1002)
                    <> Ev.Reference (GameEventID 010103))
         , ("くれ" ,  Ev.MessageT (-15) (EnJp "What a greedy fellow, take this."
                                               "強欲な奴だ、これをやろう。") (Just $ Single $ PictureID 1002)
                    <> Ev.GetItem Ev.Leader (read "3") True [
                         Ev.PlayBGM (EventBGMOnce "getitem")
                      <> Ev.Message (EnJp "You got water." "あなたは水を手に入れた。")
                         (Just $ List [Clip (Trans 0 (-10) (Single $ PictureID 0002)) (Single $ PictureID 0051), Single (PictureID 0051), Single (PictureID 1002)])
                      <> Ev.PlayBGM (EventBGM "themeOfSoleil")
                       , Ev.MessageT (-15) (EnJp "You can't carry any more. Too greedy!"
                                                 "お前、もう持てないぞ、強欲すぎるだろ") (Just $ Single $ PictureID 1002)
                       ]
                    <> Ev.Reference (GameEventID 010103))
         , ("みず" ,  Ev.MessageT (-15) (EnJp "What a greedy fellow, take this."
                                               "強欲な奴だ、これをやろう。") (Just $ Single $ PictureID 1002)
                    <> Ev.GetItem Ev.All (read "3") True [
                         Ev.PlayBGM (EventBGMOnce "getitem")
                      <> Ev.Message (EnJp "You got water." "あなたは水を手に入れた。")
                         (Just $ List [Clip (Trans 0 (-10) (Single $ PictureID 0002)) (Single $ PictureID 0051), Single (PictureID 0051), Single (PictureID 1002)])
                      <> Ev.PlayBGM (EventBGM "themeOfSoleil")
                       , Ev.MessageT (-15) (EnJp "You can't carry any more. Too greedy!"
                                                 "お前、もう持てないぞ、強欲すぎるだろ") (Just $ Single $ PictureID 1002)
                       ]
                    <> Ev.Reference (GameEventID 010103))

         , ("goodbye\nbye", Ev.MessageT (-15) (EnJp "You may come back anytime!!" "またいつでも来ると良い!!") (Just $ Single $ PictureID 1002) <> Ev.Reference (GameEventID 010104))
         , ("castle\nしろ" , Ev.SelectT (-15) (EnJp "What, do you want to return to the castle?\n(^Y/^N)"
                                                    "なんだ、城に帰りたいのか？\n(^Y/^N)") (Just $ Single $ PictureID 1002)
                       [("y",
                            Ev.MessageTimeT (-15) (EnJp "\nWait a moment." "\nちょっと待っとれ.") (Just $ Single $ PictureID 1002) (500)
                         <> Ev.MessageTime        (EnJp "\nWait a moment.." "\nちょっと待っとれ..") (Just $ Single $ PictureID 1002) (500)
                         <> Ev.MessageTime        (EnJp "\nWait a moment..." "\nちょっと待っとれ...") (Just $ Single $ PictureID 1002) (500)
                         <> Ev.MessageTimeT (-10) "\nMAPILO MAHAMA DILOMAT!!" (Just $ Single $ PictureID 1002) 750
                         <> Ev.MessageTime        (EnJp "\nWas it MAPILO MAHAMA DILOMAT!! ?"
                                                        "\nMAPILO MAHAMA DILOMAT!! だったかな?") (Just $ Single $ PictureID 1002) 300
                         <> Ev.PlayBGM Ambient <> Ev.ReturnCastle)
                       ,("n",
                           Ev.MessageT (-15) (EnJp "Is that so?" "そうなの?") (Just $ Single $ PictureID 1002) <> Ev.Reference (GameEventID 010103))
                       ])
         , ("\n", Ev.MessageT (-15) (EnJp "You may come back anytime!!" "またいつでも来ると良い!!") (Just $ Single $ PictureID 1002) <> Ev.Reference (GameEventID 010104))
         , ("", Ev.MessageT (-15) (EnJp "I don't know about that..." "それは知らない...") (Just $ Single $ PictureID 1002) <> Ev.Reference (GameEventID 010103))
         ]
      )

    -- spell
    , (GameEventID 070001, 
         Ev.MessageT 10 (EnJp "you call me?" "呼んだか？") (Just $ Single $ PictureID 1002)
      <> Ev.Select (EnJp "you call me?\n  ^Y)es  ^N)o" "呼んだか？\n  ^Y)はい  ^N)いいえ") (Just $ Single $ PictureID 1002)
         [("n", Ev.MessageT (-15) (EnJp "good by!" "さらばだ！") (Just $ Single $ PictureID 1002) <> Ev.End)
         ,("y", Ev.Switch [(Ev.PartyPositionIs [Position E 1 2 0], 
                            Ev.AsSpell (SpellID 73) <> Ev.MessageT (-15) (EnJp "how about this spell!?       \n...bye!" "この呪文はどうだ！？       \n...さらばだ！") (Just $ Single $ PictureID 1002)
                            )
                          ,(Ev.Otherwise, Ev.MessageT 10 (EnJp "this position is Invalid..." "この場所は無効だ...") (Just $ Single $ PictureID 1002))]
          )
         ]
       )
    , (GameEventID 070002, 
         Ev.MessageT 10 (EnJp "you call me?" "呼んだか？") (Just $ Single $ PictureID 1002)
      <> Ev.Select (EnJp "you call me?\n  ^Y)es  ^N)o" "呼んだか？\n  ^Y)はい  ^N)いいえ") (Just $ Single $ PictureID 1002)
         [("n", Ev.MessageT (-15) (EnJp "good by!" "さらばだ！") (Just $ Single $ PictureID 1002) <> Ev.End)
         ,("y", Ev.Switch [(Ev.PartyPositionIs [Position E 1 2 0], 
                            Ev.MessageT (-15) (EnJp "how about this spell!?       \n...bye!" "この呪文はどうだ！？       \n...さらばだ！") (Just $ Single $ PictureID 0) <> Ev.ReturnCastle
                            )
                          ,(Ev.Otherwise, Ev.MessageT 10 (EnJp "this position is Invalid..." "この場所は無効だ...") (Just $ Single $ PictureID 1002))]
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
             Ev.Message (EnJp "Your dance is NICE!!!" "見事な踊りだ！！！") (Just $ Single $ PictureID 1002)
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
    , (GameEventID 03000101, Ev.ChangeEventFlag 2 (parse' "1") <> Ev.Message (EnJp "Wow!?" "おや！？") Nothing)
    , (GameEventID 02010101, Ev.Switch [
         ( Ev.FormulaCheckParty (parse' "(evf.2=0)*100"), Ev.ChangeEventFlag 2 (parse' "1") <> Ev.Message (EnJp "You found door." "扉を見つけた。") Nothing)
        ,( Ev.Otherwise, Ev.Message (EnJp "You found nothing." "何も見つからなかった。") Nothing)
      ])
      ]

eventMap :: Map.Map Coord GameEventID
eventMap = Map.fromList [
      ((0, 0, 0), GameEventID 010100)
    , ((1, 3, 0), GameEventID 020400)
    , ((1, 3, 1), GameEventID 020401)
    , ((0, 4, 1), GameEventID 000401)
    , ((0, 4, 2), GameEventID 000402)
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

