module Data.PlayEvent
where

import PreludeL
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
           | ShowMap    String (Int, Int) Int Bool   -- ^ message, translate, z index, display mark or not
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

emptyDisplay :: Display
emptyDisplay = Display {
      messageBox = Nothing
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    , typeSE     = NoSE
    , typeBGM    = Ambient
}

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
            | EventSE String
  deriving (Show, Eq)

data BGMType = Ambient
             | TurnOff
             | Encounter
             | WinBattle
             | AllDead
             | LevelUp
             | EventBGM     String
             | EventBGMOnce String
  deriving (Show, Eq)

-- ==========================================================================

changeMessage :: String -> Event -> Event
changeMessage m (General d) = General $ d { messageBox = Just m }
changeMessage m (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { messageBox = Just m }
changeMessage m None = message m
changeMessage m (Resume c) = Resume (c . changeMessage m)
changeMessage _ e = e

changeWaitTime :: Int -> Event -> Event
changeWaitTime t (General d) = General $ d { waitTime = Just t }
changeWaitTime t (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { waitTime = Just t }
changeWaitTime t None = General $ Display Nothing Nothing Nothing (Just t) Nothing False NoSE Ambient
changeWaitTime t (Resume c) = Resume (c . changeWaitTime t)
changeWaitTime _ e = e

changeFlash :: String -> Event -> Event
changeFlash f (General d) = General $ d { flashBox = adjustText f }
changeFlash f (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { flashBox = adjustText f }
changeFlash f None = flashMessageInf f
changeFlash f (Resume c) = Resume (c . changeFlash f)
changeFlash _ e = e

changeFlashTime :: String -> Int -> Event -> Event
changeFlashTime f t (General d) = General $ d { flashBox = adjustText f, waitTime = Just t }
changeFlashTime f t (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { flashBox = adjustText f, waitTime = Just t }
changeFlashTime f t None = flashMessage t f
changeFlashTime f t (Resume c) = Resume (c . changeFlashTime f t)
changeFlashTime _ _ e = e

withPhrase :: Event -> Event
withPhrase (General d) = General $ d { needPhrase = True }
withPhrase (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { needPhrase = True }
withPhrase None = withPhrase $ General emptyDisplay
withPhrase (Resume c) = Resume (c . withPhrase)
withPhrase e = e

withNoPhrase :: Event -> Event
withNoPhrase (General d) = General $ d { needPhrase = False }
withNoPhrase (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { needPhrase = False }
withNoPhrase None = withNoPhrase $ General emptyDisplay
withNoPhrase (Resume c) = Resume (c . withNoPhrase)
withNoPhrase e = e

withSE :: SEType -> Event -> Event
withSE se (General d) = General $ d { typeSE = se }
withSE se (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { typeSE = se }
withSE se None = onlySE se
withSE se (Resume f) = Resume (f . withSE se)
withSE _ e = e

withBGM :: BGMType -> Event -> Event
withBGM bgm (General d) = General $ d { typeBGM = bgm }
withBGM bgm (ShowStatus cid ipos d) = ShowStatus cid ipos $ d { typeBGM = bgm }
withBGM bgm None = onlyBGM bgm
withBGM bgm (Resume f) = Resume (f . withBGM bgm)
withBGM _ e = e

-- ==========================================================================

adjustText :: String -> Maybe String
adjustText = Just . ('\n':) . (++ "  ") . unlines . fmap ((++"  ") . ("  "++)) . lines

message s      = General $ emptyDisplay {
      messageBox = Just s
    }
messagePic s p = General $ emptyDisplay {
      messageBox = Just s
    , picture    = p
    }
ask s p = General $ emptyDisplay {
      messageBox = Just s
    , picture    = p
    , needPhrase = True
    }
messageTime t s p = General $ emptyDisplay {
      messageBox = Just s
    , waitTime   = Just t
    , picture    = p
    }
askFlashAndMessage s f p = General $ emptyDisplay {
      messageBox = Just s
    , flashBox   = adjustText f
    , picture    = p
    , needPhrase = True
    }
flashAndMessageTime t s f p = General $ emptyDisplay {
      messageBox = Just s
    , flashBox   = adjustText f
    , waitTime   = Just t
    , picture    = p
    }
wait t p = General $ emptyDisplay {
      waitTime   = Just t
    , picture    = p
    }
flashMessageInf s = General $ emptyDisplay {
      flashBox   = adjustText s
    }
flashMessage t s = General $ emptyDisplay {
      flashBox   = adjustText s
    , waitTime   = Just t
    }
flashMessage' t s = General $ emptyDisplay {
      flashBox   = Just s
    , waitTime   = Just t
    }
battleCommand s = General $ emptyDisplay {
      commandBox = Just s
    }
spellCommand s = General $ emptyDisplay {
      commandBox = Just s
    , needPhrase = True
    }

showStatus cid msg = ShowStatus cid Nothing $ emptyDisplay {
      messageBox = Just msg
}
showStatusFlash cid msg fmsg = ShowStatus cid Nothing $ emptyDisplay {
      messageBox = Just msg
    , flashBox   = adjustText fmsg
}
showStatusAlt cid msg alt = ShowStatus cid Nothing $ emptyDisplay {
      messageBox = Just msg
    , commandBox = Just alt
}
showStatusAlt' cid msg alt help = ShowStatus cid Nothing $ emptyDisplay {
      messageBox = Just msg
    , commandBox = Just alt
    , flashBox   = adjustText help
}
askInStatus cid msg = ShowStatus cid Nothing $ emptyDisplay {
      messageBox = Just msg
    , needPhrase = True
}
showStatusEquip cid his msg = ShowStatus cid (Just his) $ emptyDisplay {
      messageBox = Just msg
}

onlySE se = General $ emptyDisplay {
      typeSE     = se
    }
onlyBGM bgm = General $ emptyDisplay {
      typeBGM    = bgm
    }
