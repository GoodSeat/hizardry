module Data.PlayEvent
where

import Data.Primitive
import Data.Characters

-- ==========================================================================

data Input = Key String
           | AnyKey
           | Clock
           | Abort
    deriving (Show, Eq, Read)

data InputType = SingleKey
               | SequenceKey
               | WaitClock Int -- ^ time to wait(ms), negative wait time means enable to skip by key input
    deriving (Show, Eq)

data Event = None
           | Exit
           | SaveGame Int String -- ^ slot id, tag
           | LoadGame Int        -- ^ slot id
           | General    Display
           | ShowStatus CharacterID (Maybe [ItemPos]) Display  -- ^ target character ID, highlight items, display.
           | ShowMap    String (Int, Int)   -- ^ message, translete
           | Resume     (Event -> Event)
--  deriving (Show, Eq)

data Display = Display {
      messageBox :: !(Maybe String)
    , commandBox :: !(Maybe String)
    , flashBox   :: !(Maybe String)
    , waitTime   :: !(Maybe Int)       -- ^ negative wait time means enable to skip by key input
    , picture    :: !(Maybe PictureInf)
    , needPhrase :: !Bool
    , typeSE     :: !SEType
    , typeBGM    :: !BGMType
} deriving (Show, Eq)

data SEType = NoSE
            | Walk
            | TurnLeftOrRight
            | HitWall
            | KickDoor
            | Spelled
            | FightHitToE
            | FightHitToP
            | SpellHitToE
            | SpellHitToP
  deriving (Show, Eq)

data BGMType = NoBGM
             | TurnOff
             | Encounter
             | WinBattle
             | AllDead
             | LevelUp
  deriving (Show, Eq)

-- ==========================================================================

changeMessage :: String -> Event -> Event
changeMessage m (General d) = General $ d { messageBox = Just m }
changeMessage m (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { messageBox = Just m }
changeMessage _ e = e

changeFlash :: String -> Event -> Event
changeFlash f (General d) = General $ d { flashBox = adjustText f }
changeFlash f (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { flashBox = adjustText f }
changeFlash _ e = e

changeFlashTime :: String -> Int -> Event -> Event
changeFlashTime f t (General d) = General $ d { flashBox = adjustText f, waitTime = Just t }
changeFlashTime f t (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { flashBox = adjustText f, waitTime = Just t }
changeFlashTime _ _ e = e

withSE :: SEType -> Event -> Event
withSE se (General d) = General $ d { typeSE = se }
withSE se (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { typeSE = se }
withSE _ e = e

withBGM :: BGMType -> Event -> Event
withBGM bgm (General d) = General $ d { typeBGM = bgm }
withBGM bgm (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { typeBGM = bgm }
withBGM _ e = e

-- ==========================================================================

adjustText :: String -> Maybe String
adjustText = Just . ('\n':) . (++ "  ") . unlines . fmap ((++"  ") . ("  "++)) . lines

message s = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }
messagePic s p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = p
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }
ask s p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = p
    , needPhrase = True
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }
messageTime t s p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Just t
    , picture    = p
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }
askFlashAndMessage s f p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = adjustText f
    , waitTime   = Nothing
    , picture    = p
    , needPhrase = True
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }
flashAndMessageTime t s f p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = adjustText f
    , waitTime   = Just t
    , picture    = p
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }
wait t p = General $ Display {
      messageBox = Nothing
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Just t
    , picture    = p
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }
flashMessage t s = General $ Display {
      messageBox = Nothing
    , commandBox = Nothing
    , flashBox   = adjustText s
    , waitTime   = Just t
    , picture    = Nothing
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }
flashMessage' t s = General $ Display {
      messageBox = Nothing
    , commandBox = Nothing
    , flashBox   = Just s
    , waitTime   = Just t
    , picture    = Nothing
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }
battleCommand s = General $ Display {
      messageBox = Nothing
    , commandBox = Just s
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }
spellCommand s = General $ Display {
      messageBox = Nothing
    , commandBox = Just s
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = True
    , typeSE     = NoSE
    , typeBGM    = NoBGM
    }

showStatus cid msg = ShowStatus cid Nothing $ Display {
      messageBox = Just msg
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
}
showStatusFlash cid msg fmsg = ShowStatus cid Nothing $ Display {
      messageBox = Just msg
    , commandBox = Nothing
    , flashBox   = adjustText fmsg
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
}
showStatusAlt cid msg alt = ShowStatus cid Nothing $ Display {
      messageBox = Just msg
    , commandBox = Just alt
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
}
showStatusAlt' cid msg alt help = ShowStatus cid Nothing $ Display {
      messageBox = Just msg
    , commandBox = Just alt
    , flashBox   = adjustText help
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
}
askInStatus cid msg = ShowStatus cid Nothing $ Display {
      messageBox = Just msg
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = True
    , typeSE     = NoSE
    , typeBGM    = NoBGM
}
showStatusEquip cid his msg = ShowStatus cid (Just his) $ Display {
      messageBox = Just msg
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = NoBGM
}

