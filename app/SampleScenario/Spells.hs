module SampleScenario.Spells where

import PreludeL
import qualified Data.Spells as Spell
import qualified Data.Map as Map

import Data.Primitive
import Data.Formula

spells :: Spell.DB
spells = Map.fromList [
    (SpellID 11, Spell.Define {
          Spell.name        = "halito"
        , Spell.kind        = Spell.M
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "fire"]
        , Spell.target      = Spell.OpponentSingle
        , Spell.effect      = Spell.Damage (parse' "1d8")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1体に1～8ダメージ(火炎)"
    })
    ,
    (SpellID 12, Spell.Define {
          Spell.name        = "mogref"
        , Spell.kind        = Spell.M
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "mage"]
        , Spell.target      = Spell.AllyOwn
        , Spell.effect      = Spell.ChangeParam (emptyAdParam { adAC = read "-2" }) OnlyInBattle "'s AC decreased by 2"
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "唱えた者のACを2低下させる"
    })
    ,
    (SpellID 13, Spell.Define {
          Spell.name        = "katino"
        , Spell.kind        = Spell.M
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "sleep"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.AddStatusError [(Sleep, read "50", "fall in sleep")]
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループの行動を止める(睡眠)\n眠った敵には直接攻撃のダメージが2倍"
    })
    ,
    (SpellID 14, Spell.Define {
          Spell.name        = "dumapic"
        , Spell.kind        = Spell.M
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "mage"]
        , Spell.target      = Spell.AllyAll
        --, Spell.effect      = Spell.CheckLocation Spell.OnlyCoord
        , Spell.effect      = Spell.CheckLocation Spell.ViewMap
        , Spell.enableIn   = [Spell.InCamp]
        , Spell.information = "城への階段からの座標を東、北、高さで示す"
    })
    ,
    (SpellID 15, Spell.Define {
          Spell.name        = "te"
        , Spell.kind        = Spell.M
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "mage"]
        , Spell.target      = Spell.OpponentAll
        --, Spell.effect      = Spell.CheckLocation Spell.OnlyCoord
        , Spell.effect      = Spell.Event (GameEventID 070001)
        , Spell.enableIn   = [Spell.InBattle]
        , Spell.information = "イベント呪文(in battle)"
    })
    ,
    (SpellID 16, Spell.Define {
          Spell.name        = "return"
        , Spell.kind        = Spell.M
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "mage"]
        , Spell.target      = Spell.Party
        --, Spell.effect      = Spell.CheckLocation Spell.OnlyCoord
        , Spell.effect      = Spell.Event (GameEventID 070002)
        , Spell.enableIn   = [Spell.InCamp]
        , Spell.information = "イベント呪文(in camp)"
    })
    ,
    (SpellID 21, Spell.Define {
          Spell.name        = "dilto"
        , Spell.kind        = Spell.M
        , Spell.lv          = 2
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "ac-attack"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.ChangeParam (emptyAdParam { adAC = read "2" }) OnlyInBattle "'s AC increased by 2"
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループのACを2上昇させる"
    })
    ,
    (SpellID 22, Spell.Define {
          Spell.name        = "sopic"
        , Spell.kind        = Spell.M
        , Spell.lv          = 2
        , Spell.attrLabels  = [EffectLabel "mage"]
        , Spell.target      = Spell.AllyOwn
        , Spell.effect      = Spell.ChangeParam (emptyAdParam { adAC = read "-4" }) OnlyInBattle "'s AC decreased by 4"
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "唱えた者のACを4低下させる"
    })
    ,
    (SpellID 31, Spell.Define {
          Spell.name        = "mahalito"
        , Spell.kind        = Spell.M
        , Spell.lv          = 3
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "fire"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.Damage (parse' "4d6")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループに4～24ダメージ(火炎)"
    })
    ,
    (SpellID 32, Spell.Define {
          Spell.name        = "molito"
        , Spell.kind        = Spell.M
        , Spell.lv          = 3
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "non-elemental"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.Damage (parse' "3d6")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループに3～18ダメージ"
    })
    ,
    (SpellID 41, Spell.Define {
          Spell.name        = "morlis"
        , Spell.kind        = Spell.M
        , Spell.lv          = 4
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "ac-attack"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.ChangeParam (emptyAdParam { adAC = read "4" }) OnlyInBattle "'s AC increased by 4"
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループのACを4上昇させる"
    })
    ,
    (SpellID 42, Spell.Define {
          Spell.name        = "dalto"
        , Spell.kind        = Spell.M
        , Spell.lv          = 4
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "frost"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.Damage (parse' "6d6")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループに6～36ダメージ(冷気)"
    })
    ,
    (SpellID 43, Spell.Define {
          Spell.name        = "lahalito"
        , Spell.kind        = Spell.M
        , Spell.lv          = 4
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "fire"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.Damage (parse' "6d6")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループに6～36ダメージ(火炎)"
    })
    ,
    (SpellID 51, Spell.Define {
          Spell.name        = "mamorlis"
        , Spell.kind        = Spell.M
        , Spell.lv          = 5
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "ac-attack"]
        , Spell.target      = Spell.OpponentAll
        , Spell.effect      = Spell.ChangeParam (emptyAdParam { adAC = read "4" }) OnlyInBattle "'s AC increased by 4"
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵全てのACを4上昇させる"
    })
    ,
    (SpellID 52, Spell.Define {
          Spell.name        = "makanito"
        , Spell.kind        = Spell.M
        , Spell.lv          = 5
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "death"]
        , Spell.target      = Spell.OpponentAll
        , Spell.effect      = Spell.AddStatusError [(Dead, read "(o.lv<=7)*50", "")]
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "レベル7以下の敵全てを即死させる。ただし不死系には無効"
    })
    ,
    (SpellID 53, Spell.Define {
          Spell.name        = "madalto"
        , Spell.kind        = Spell.M
        , Spell.lv          = 5
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "frost"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.Damage (parse' "8d8")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループに8～64ダメージ(冷気)"
    })
    ,
    (SpellID 61, Spell.Define {
          Spell.name        = "lakanito"
        , Spell.kind        = Spell.M
        , Spell.lv          = 6
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "no-air"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.AddStatusError [(Dead, read "min(70,(lv-o.lv)*10+30)", "")]
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループを即死させる"
    })
    ,
    (SpellID 62, Spell.Define {
          Spell.name        = "zilwan"
        , Spell.kind        = Spell.M
        , Spell.lv          = 6
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "purgation"]
        , Spell.target      = Spell.OpponentSingle
        , Spell.effect      = Spell.AddStatusError [(Dead, read "-1*min(80,(lv-o.lv)*10+50)", "is purged.")]
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "不死系1体を即死させる"
    })
    ,
    (SpellID 63, Spell.Define {
          Spell.name        = "masopic"
        , Spell.kind        = Spell.M
        , Spell.lv          = 6
        , Spell.attrLabels  = [EffectLabel "mage"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.ChangeParam (emptyAdParam { adAC = read "-4" }) OnlyInBattle "'s AC decreased by 4"
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "パーティ全員のACを4低下させる"
    })
    ,
    (SpellID 64, Spell.Define {
          Spell.name        = "haman"
        , Spell.kind        = Spell.M
        , Spell.lv          = 6
        , Spell.attrLabels  = [EffectLabel "mage"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.Event (GameEventID 10064)
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "何かが起こる"
    })
    ,
    (SpellID 71, Spell.Define {
          Spell.name        = "malor"
        , Spell.kind        = Spell.M
        , Spell.lv          = 7
        , Spell.attrLabels  = [EffectLabel "mage"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.Event (GameEventID 10071)
        , Spell.enableIn    = [Spell.InBattle, Spell.InCamp]
        , Spell.information = "座標指定テレポート。戦闘時はランダムテレポート"
    })
    ,
    (SpellID 72, Spell.Define {
          Spell.name        = "mahaman"
        , Spell.kind        = Spell.M
        , Spell.lv          = 7
        , Spell.attrLabels  = [EffectLabel "mage"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.Event (GameEventID 10072)
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "何かが起こる"
    })
    ,
    (SpellID 73, Spell.Define {
          Spell.name        = "tiltowait"
        , Spell.kind        = Spell.M
        , Spell.lv          = 7
        , Spell.attrLabels  = [EffectLabel "mage", EffectLabel "non-elemental"]
        , Spell.target      = Spell.OpponentAll
        , Spell.effect      = Spell.Damage (parse' "10d10")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵全てに10～100ダメージ"
    })

    ,
    (SpellID 111, Spell.Define {
          Spell.name        = "kalki"
        , Spell.kind        = Spell.P
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.ChangeParam (emptyAdParam { adAC = read "-1" }) OnlyInBattle "'s AC decreased by 1"
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "パーティ全員のACを1低下させる"
    })
    ,
    (SpellID 112, Spell.Define {
          Spell.name        = "dios"
        , Spell.kind        = Spell.P
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllySingle
        , Spell.effect      = Spell.Cure (parse' "1d8") []
        , Spell.enableIn    = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "味方一人のHPを1～8回復する"
    })
    ,
    (SpellID 113, Spell.Define {
          Spell.name        = "badios"
        , Spell.kind        = Spell.P
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "priest", EffectLabel "holy"]
        , Spell.target      = Spell.OpponentSingle
        , Spell.effect      = Spell.Damage (parse' "1d8")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1体に1～8ダメージ"
    })
    ,
    (SpellID 114, Spell.Define {
          Spell.name        = "milwa"
        , Spell.kind        = Spell.P
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.AddLight 30 False
        , Spell.enableIn    = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "30歩の間3ブロック先まで迷宮を照らし、隠し扉も見える"
    })
    ,
    (SpellID 115, Spell.Define {
          Spell.name        = "porfic"
        , Spell.kind        = Spell.P
        , Spell.lv          = 1
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllyOwn
        , Spell.effect      = Spell.ChangeParam (emptyAdParam { adAC = read "-4" }) OnlyInBattle "'s AC decreased by 4"
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "唱えた者のACを4低下させる"
    })
    ,
    (SpellID 121, Spell.Define {
          Spell.name        = "matu"
        , Spell.kind        = Spell.P
        , Spell.lv          = 2
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.ChangeParam (emptyAdParam { adAC = read "-2" }) OnlyInBattle "'s AC decreased by 2"
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "パーティ全員のACを2低下させる"
    })
    ,
    (SpellID 122, Spell.Define {
          Spell.name        = "calfo"
        , Spell.kind        = Spell.P
        , Spell.lv          = 2
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.Event (GameEventID 0) -- TODO:not implmented.
        , Spell.enableIn    = []
        , Spell.information = "宝箱に仕掛けられた罠の種類を95%で見抜く"
    })
    ,
    (SpellID 123, Spell.Define {
          Spell.name        = "manifo"
        , Spell.kind        = Spell.P
        , Spell.lv          = 2
        , Spell.attrLabels  = [EffectLabel "priest", EffectLabel "fear"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.AddStatusError [(Fear 3, read "min(60,(lv-o.lv)*10+30)", "")]
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループの行動を止める(凝固)"
    })
    ,
    (SpellID 124, Spell.Define {
          Spell.name        = "montino"
        , Spell.kind        = Spell.P
        , Spell.lv          = 2
        , Spell.attrLabels  = [EffectLabel "priest", EffectLabel "silent"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.AddStatusError [(Silence, read "min(60,(lv-o.lv)*10+30)", "")]
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループの呪文を封じる"
    })
    ,
    (SpellID 131, Spell.Define {
          Spell.name        = "lomilwa"
        , Spell.kind        = Spell.P
        , Spell.lv          = 3
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.AddLight 100000 False
        , Spell.enableIn    = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "3ブロック先まで迷宮を照らし、隠し扉も見える"
    })
    ,
    (SpellID 132, Spell.Define {
          Spell.name        = "dialko"
        , Spell.kind        = Spell.P
        , Spell.lv          = 3
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllySingle
        , Spell.effect      = Spell.Cure (read "0") [Paralysis]
        , Spell.enableIn    = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "味方一人の麻痺を治療する"
    })
    ,
    (SpellID 133, Spell.Define {
          Spell.name        = "latumapic"
        , Spell.kind        = Spell.P
        , Spell.lv          = 3
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.Party
        , Spell.effect      = Spell.ChangeParam emptyAdParam OnlyInMaze "identify"
        , Spell.enableIn    = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "冒険中敵の正体を確定させる"
    })
    ,
    (SpellID 134, Spell.Define {
          Spell.name        = "bamatu"
        , Spell.kind        = Spell.P
        , Spell.lv          = 3
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.ChangeParam (emptyAdParam { adAC = read "-4" }) OnlyInBattle ""
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "パーティ全員のACを4低下させる"
    })
    ,
    (SpellID 141, Spell.Define {
          Spell.name        = "dial"
        , Spell.kind        = Spell.P
        , Spell.lv          = 4
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllySingle
        , Spell.effect      = Spell.Cure (parse' "2d8") []
        , Spell.enableIn    = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "味方一人のHPを2～16回復する"
    })
    ,
    (SpellID 142, Spell.Define {
          Spell.name        = "badial"
        , Spell.kind        = Spell.P
        , Spell.lv          = 4
        , Spell.attrLabels  = [EffectLabel "priest", EffectLabel "holy"]
        , Spell.target      = Spell.OpponentSingle
        , Spell.effect      = Spell.Damage (parse' "2d8")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1体に2～16ダメージ"
    })
    ,
    (SpellID 143, Spell.Define {
          Spell.name        = "latumofis"
        , Spell.kind        = Spell.P
        , Spell.lv          = 4
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllySingle
        , Spell.effect      = Spell.Cure (read "0") $ Poison <$> [1..100]
        , Spell.enableIn    = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "味方一人の毒を治療する"
    })
    ,
    (SpellID 144, Spell.Define {
          Spell.name        = "maporfic"
        , Spell.kind        = Spell.P
        , Spell.lv          = 4
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.Party
        , Spell.effect      = Spell.ChangeParam (emptyAdParam {
            adAC = read "-2 ", adName = "protection"
            }) OnlyInMaze "is protected."
        , Spell.enableIn   = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "冒険中パーティ全員のACを2低下させる"
    })
    ,
    (SpellID 151, Spell.Define {
          Spell.name        = "dialma"
        , Spell.kind        = Spell.P
        , Spell.lv          = 5
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllySingle
        , Spell.effect      = Spell.Cure (parse' "3d8") []
        , Spell.enableIn    = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "味方一人のHPを3～24回復する"
    })
    ,
    (SpellID 152, Spell.Define {
          Spell.name        = "badialma"
        , Spell.kind        = Spell.P
        , Spell.lv          = 5
        , Spell.attrLabels  = [EffectLabel "priest", EffectLabel "holy"]
        , Spell.target      = Spell.OpponentSingle
        , Spell.effect      = Spell.Damage (parse' "3d8")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1体に3～24ダメージ"
    })
    ,
    (SpellID 153, Spell.Define {
          Spell.name        = "litokan"
        , Spell.kind        = Spell.P
        , Spell.lv          = 5
        , Spell.attrLabels  = [EffectLabel "priest", EffectLabel "fire"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.Damage (parse' "3d8")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループに3～24ダメージ(火炎)"
    })
    ,
    (SpellID 154, Spell.Define {
          Spell.name        = "kandi"
        , Spell.kind        = Spell.P
        , Spell.lv          = 5
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.Party
        , Spell.effect      = Spell.Event (GameEventID 0) -- TODO:not implmented.
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "行方不明者の迷宮内の位置を大雑把に示す"
    })
    ,
    (SpellID 155, Spell.Define {
          Spell.name        = "di"
        , Spell.kind        = Spell.P
        , Spell.lv          = 5
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllySingle
        , Spell.effect      = Spell.Resurrection (read "1") [(Dead, read "(o.vit+1)*4")]
        , Spell.enableIn    = [Spell.InCamp]
        , Spell.information = "死んだ味方1人をHP1で蘇生させる"
    })
    ,
    (SpellID 156, Spell.Define {
          Spell.name        = "badi"
        , Spell.kind        = Spell.P
        , Spell.lv          = 5
        , Spell.attrLabels  = [EffectLabel "priest", EffectLabel "death"]
        , Spell.target      = Spell.OpponentSingle
        , Spell.effect      = Spell.AddStatusError [(Dead, read "min(70,(lv-o.lv)*10+30)", "")]
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1体を即死させる。ただし不死系や特定の敵には無効"
    })
    ,
    (SpellID 161, Spell.Define {
          Spell.name        = "lorto"
        , Spell.kind        = Spell.P
        , Spell.lv          = 6
        , Spell.attrLabels  = [EffectLabel "priest", EffectLabel "holy"]
        , Spell.target      = Spell.OpponentGroup
        , Spell.effect      = Spell.Damage (parse' "6d6")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1グループに6～36ダメージ"
    })
    ,
    (SpellID 162, Spell.Define {
          Spell.name        = "madi"
        , Spell.kind        = Spell.P
        , Spell.lv          = 6
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllySingle
        , Spell.effect      = Spell.Cure (read "9999999") $ [Silence, Paralysis, Stoned, Sleep, Poison 0, Fear 0]
        , Spell.enableIn    = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "味方1人のHPを全回復し、死・灰・ロスト以外を全て治療する"
    })
    ,
    (SpellID 163, Spell.Define {
          Spell.name        = "mabadi"
        , Spell.kind        = Spell.P
        , Spell.lv          = 6
        , Spell.attrLabels  = [EffectLabel "priest", EffectLabel "special"]
        , Spell.target      = Spell.OpponentSingle
        , Spell.effect      = Spell.Damage (read "o.hp-1d8")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵1体のHPを1～8残して奪う"
    })
    ,
    (SpellID 164, Spell.Define {
          Spell.name        = "loktofeit"
        , Spell.kind        = Spell.P
        , Spell.lv          = 6
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.Party
        , Spell.effect      = Spell.Event (GameEventID 100164) -- TODO:not implmented.
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "全ての装備とほとんどの金を失って城に戻る"
    })
    ,
    (SpellID 171, Spell.Define {
          Spell.name        = "malikto"
        , Spell.kind        = Spell.P
        , Spell.lv          = 7
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.OpponentAll
        , Spell.effect      = Spell.Damage (parse' "12d6")
        , Spell.enableIn    = [Spell.InBattle]
        , Spell.information = "敵全てに12～72ダメージ"
    })
    ,
    (SpellID 172, Spell.Define {
          Spell.name        = "kadorto"
        , Spell.kind        = Spell.P
        , Spell.lv          = 7
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllySingle
        , Spell.effect      = Spell.Resurrection (read "o.maxhp") [(Dead, read "(o.vit+1)*4"), (Ash, read "(o.vit+1)*4")]
        , Spell.enableIn    = [Spell.InCamp]
        , Spell.information = "死または灰の味方1人をHPフルで蘇生させる"
    })

    ,
    (SpellID 201, Spell.Define {
          Spell.name        = "smilwa"
        , Spell.kind        = Spell.P
        , Spell.lv          = 2
        , Spell.attrLabels  = [EffectLabel "priest"]
        , Spell.target      = Spell.AllyAll
        , Spell.effect      = Spell.AddLight 30 True
        , Spell.enableIn    = [Spell.InCamp, Spell.InBattle]
        , Spell.information = "30歩の間3ブロック先まで迷宮を照らし、ダークゾーンをかき消す"
    })
    ]
