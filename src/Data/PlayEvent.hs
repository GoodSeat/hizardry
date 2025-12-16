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
           | General       Display
           | ShowStatus    CharacterID (Maybe [ItemPos]) Display  -- ^ target character ID, highlight items, display.
           | ShowMap       String (Int, Int)   -- ^ message, translete
    deriving (Show, Eq)

data Display = Display {
      messageBox :: !(Maybe String)
    , commandBox :: !(Maybe String)
    , flashBox   :: !(Maybe String)
    , waitTime   :: !(Maybe Int)       -- ^ negative wait time means enable to skip by key input
    , picture    :: !(Maybe PictureInf)
    , needPhrase :: Bool
} deriving (Show, Eq)

-- ==========================================================================

message s = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    }
messagePic s p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = p
    , needPhrase = False
    }
ask s p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = p
    , needPhrase = True
    }
messageTime t s p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Just t
    , picture    = p
    , needPhrase = False
    }
flashAndMessageTime t s f p = General $ Display {
      messageBox = Just s
    , commandBox = Nothing
    , flashBox   = Just f
    , waitTime   = Just t
    , picture    = p
    , needPhrase = False
    }
wait t p = General $ Display {
      messageBox = Nothing
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Just t
    , picture    = p
    , needPhrase = False
    }
flashMessage t s = General $ Display {
      messageBox = Nothing
    , commandBox = Nothing
    , flashBox   = Just s
    , waitTime   = Just t
    , picture    = Nothing
    , needPhrase = False
    }
battleCommand s = General $ Display {
      messageBox = Nothing
    , commandBox = Just s
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
    }
spellCommand s = General $ Display {
      messageBox = Nothing
    , commandBox = Just s
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = True
    }

showStatus cid msg = ShowStatus cid Nothing $ Display {
      messageBox = Just msg
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
}
showStatusFlash cid msg fmsg = ShowStatus cid Nothing $ Display {
      messageBox = Just msg
    , commandBox = Nothing
    , flashBox   = Just fmsg
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
}
showStatusAlt cid msg alt = ShowStatus cid Nothing $ Display {
      messageBox = Just msg
    , commandBox = Just alt
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
}
showStatusAlt' cid msg alt help = ShowStatus cid Nothing $ Display {
      messageBox = Just msg
    , commandBox = Just alt
    , flashBox   = Just help
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
}
askInStatus cid msg = ShowStatus cid Nothing $ Display {
      messageBox = Just msg
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = True
}
showStatusEquip cid his msg = ShowStatus cid (Just his) $ Display {
      messageBox = Just msg
    , commandBox = Nothing
    , flashBox   = Nothing
    , waitTime   = Nothing
    , picture    = Nothing
    , needPhrase = False
}

