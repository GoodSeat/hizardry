module Data.Primitive
where

import Data.Formula

import GHC.Stack (HasCallStack)
import Data.List (find)
import Data.Char (ord)
import Data.Function ((&))

-- ==========================================================================
-- ID
-- --------------------------------------------------------------------------

newtype CharacterID = CharacterID {
    characterId :: Int
} deriving (Show, Eq, Ord, Read)


newtype ItemID = ItemID {
      itemId     :: Int  -- ^ identify number.
} deriving (Show, Eq, Ord, Read)

data ItemInf = ItemInf {
      itemID     :: ItemID
    , identified :: Bool -- ^ already identified or not.
} deriving (Show, Eq, Ord, Read)


newtype EnemyID = EnemyID {
    enemyID :: Int
} deriving (Show, Eq, Ord, Read)


newtype SpellID = SpellID {
    spellId :: Int -- ^ identify number.
} deriving (Show, Eq, Ord, Read)


newtype GameEventID = GameEventID {
    mazeEventID :: Int
} deriving (Show, Eq, Ord, Read)


newtype PictureID = PictureID {
    pictureID :: Int
} deriving (Show, Eq, Ord, Read)


-- --------------------------------------------------------------------------
data PictureInf = Null
                | Single PictureID
                | Trans  Int Int PictureInf
                | Xor    PictureInf PictureInf
                | Clip   PictureInf PictureInf
                | Diff   PictureInf PictureInf
                | List   [PictureInf]
  deriving (Show, Eq, Read)


-- ==========================================================================
-- label
-- --------------------------------------------------------------------------

newtype EnemyLabel = EnemyLabel { enemyLabel :: String } deriving (Show, Eq, Read)

newtype EffectLabel = EffectLabel { effectLabel :: String } deriving (Show, Eq, Read)


-- ==========================================================================
-- object
-- --------------------------------------------------------------------------

-- | type of status error.
data StatusError = Silence
                 | Paralysis
                 | Stoned
                 | Poison Int
                 | Fear Int   -- valid time
                 | Sleep
                 | Hidden
                 | Found
                 | Drain Int
                 | Dead
                 | Ash
                 | Lost
    deriving (Ord, Show, Eq, Read)

-- | define of character's parameter.
data Parameter = Parameter {
      strength :: !Int -- ^ strength
    , iq       :: !Int -- ^ I.Q.
    , piety    :: !Int -- ^ piety
    , vitality :: !Int -- ^ vitality
    , agility  :: !Int -- ^ agility
    , luck     :: !Int -- ^ luck
} deriving (Show, Eq, Read)

instance Semigroup Parameter where
    p1 <> p2 = Parameter {
      strength = strength p1 + strength p2
    , iq       = iq       p1 + iq       p2
    , piety    = piety    p1 + piety    p2
    , vitality = vitality p1 + vitality p2
    , agility  = agility  p1 + agility  p2
    , luck     = luck     p1 + luck     p2
    }

instance Monoid Parameter where
    mempty = emptyParam
    mappend = (<>)

emptyParam :: Parameter
emptyParam = Parameter { strength = 0
                       , iq       = 0
                       , piety    = 0
                       , vitality = 0
                       , agility  = 0
                       , luck     = 0
                       }

-- | define of temporary parameter change.
data ParamChange = ParamChange {
      deltaParam :: !Parameter
    , deltaAC    :: !Int
    , effectName :: !String -- ^ effect name. if this name isn't empty, can't apply multiple.
} deriving (Show, Eq, Read)

emptyParamChange = ParamChange emptyParam 0 ""

parryParamChange = ParamChange emptyParam (-2) "Parry"

data AdParam = AdParam {
      adStrength :: !Formula -- ^ strength
    , adIq       :: !Formula -- ^ I.Q.
    , adPiety    :: !Formula -- ^ piety
    , adVitality :: !Formula -- ^ vitality
    , adAgility  :: !Formula -- ^ agility
    , adLuck     :: !Formula -- ^ luck
    , adAC       :: !Formula -- ^ AC
    , adName     :: !String -- ^ effect name. if this name isn't empty, can't apply multiple.
} deriving (Show, Eq, Read)

emptyAdParam = AdParam {
      adStrength = read "0"
    , adIq       = read "0"
    , adPiety    = read "0"
    , adVitality = read "0"
    , adAgility  = read "0"
    , adLuck     = read "0"
    , adAC       = read "0"
    , adName     = ""
}


-- | effect valid term.
data Term = OnlyInBattle | OnlyInMaze | TillPastTime Int
    deriving (Show, Eq, Read)

-- ==========================================================================

-- | position in party.
data PartyPos = F1 | F2 | F3 | B4 | B5 | B6
  deriving (Show, Ord, Eq)

toPartyPos :: HasCallStack => Int -> PartyPos
toPartyPos 1 = F1
toPartyPos 2 = F2
toPartyPos 3 = F3
toPartyPos 4 = B4
toPartyPos 5 = B5
toPartyPos 6 = B6
toPartyPos _ = error "invalid index of PartyPos"

partyPosToNum :: PartyPos -> Int
partyPosToNum F1 = 1
partyPosToNum F2 = 2
partyPosToNum F3 = 3
partyPosToNum B4 = 4
partyPosToNum B5 = 5
partyPosToNum B6 = 6

nextPartyPos :: PartyPos -> Maybe PartyPos
nextPartyPos F1 = Just F2
nextPartyPos F2 = Just F3
nextPartyPos F3 = Just B4
nextPartyPos B4 = Just B5
nextPartyPos B5 = Just B6
nextPartyPos B6 = Nothing

prevPartyPos :: PartyPos -> Maybe PartyPos
prevPartyPos F1 = Nothing
prevPartyPos F2 = Just F1
prevPartyPos F3 = Just F2
prevPartyPos B4 = Just F3
prevPartyPos B5 = Just B4
prevPartyPos B6 = Just B5


-- | position in enemy line groups.
data EnemyLine = L1 | L2 | L3 | L4
  deriving (Show, Ord, Eq, Read)

toEnemyLine :: HasCallStack => Int -> EnemyLine
toEnemyLine 1 = L1
toEnemyLine 2 = L2
toEnemyLine 3 = L3
toEnemyLine 4 = L4
toEnemyLine _ = error "invalid index of EnemyLine"

enemyLineToNum :: EnemyLine -> Int
enemyLineToNum L1 = 1
enemyLineToNum L2 = 2
enemyLineToNum L3 = 3
enemyLineToNum L4 = 4

nextEnemyLine :: EnemyLine -> Maybe EnemyLine
nextEnemyLine L1 = Just L2
nextEnemyLine L2 = Just L3
nextEnemyLine L3 = Just L4
nextEnemyLine L4 = Nothing

prevEnemyLine :: EnemyLine -> Maybe EnemyLine
prevEnemyLine L1 = Nothing
prevEnemyLine L2 = Just L1
prevEnemyLine L3 = Just L2
prevEnemyLine L4 = Just L3


type SpellTarget = Either PartyPos EnemyLine

-- ==========================================================================

class Eq o => Object o where
  nameOf          :: o -> String
  hpOf            :: o -> Int
  maxhpOf         :: o -> Int
  lvOf            :: o -> Int
  statusErrorsOf  :: o -> [StatusError]  -- ^ status errors.
  whenTimePast    :: o -> o

  setHp           :: Int -> o -> o
  setStatusErrors :: [StatusError] -> o -> o

addStatusError :: Object o => StatusError -> o -> o
addStatusError s o = let o' = if s >= Dead && hpOf o > 0 then setHp 0 o else o
                     in setStatusErrors (s : statusErrorsOf (removeStatusError s o')) o'

removeStatusError :: Object o => StatusError -> o -> o
removeStatusError s o = setStatusErrors (filter (not . areSameStatusError s) $ statusErrorsOf o) o

addPoison :: Object o => Int -> o -> o
addPoison d s = let ss = statusErrorsOf s in
    case find isPoison ss of Just (Poison n) -> addStatusError (Poison $ n + d) . removeStatusError (Poison n) $ s
                             _               -> addStatusError (Poison d) s
  where
    isPoison (Poison _) = True
    isPoison _          = False

damageHp :: Object o => Int -> o -> o
damageHp dmg s = setHp (hpOf s - dmg) s




whenReturnCastle :: Object o => o -> o
whenReturnCastle c = foldl (&) c (whenReturnCastle' <$> statusErrorsOf c) 
  where
    whenReturnCastle' :: Object o => StatusError -> o -> o
    whenReturnCastle' (Poison n) = removeStatusError (Poison n)
    whenReturnCastle' (Fear   n) = removeStatusError (Fear   n)
    whenReturnCastle' Sleep      = removeStatusError Sleep
    whenReturnCastle' Hidden     = removeStatusError Hidden
    whenReturnCastle' Found      = removeStatusError Found
    whenReturnCastle' _          = id

whenToNextTurn :: Object o
               => Int    -- ^ random integer 1~100
               -> Int    -- ^ count of last enemies
               -> Parameter
               -> o
               -> o
whenToNextTurn n ne param o = foldl (&) (whenTimePast o) (whenToNextTurn' n <$> statusErrorsOf o)
  where
    whenToNextTurn' :: Object o => Int -> StatusError -> o -> o
    whenToNextTurn' _ (Poison n) o = setHp (hpOf o - n) o
    whenToNextTurn' n (Sleep   ) o = if n < 50 then o else removeStatusError Sleep o
    whenToNextTurn' _ (Fear   t) o = (if t > 1 then addStatusError (Fear $ t - 1) else id) $ removeStatusError (Fear t) o
    whenToNextTurn' n (Hidden  ) o = (if n + 2*ne - 2*agility param > 50 then addStatusError Found . removeStatusError Hidden else id) o
    whenToNextTurn' _ _ o = o

whenWalking :: Object o => o -> o
whenWalking c = foldl (&) (whenTimePast c) (whenWalking' <$> statusErrorsOf c) 
  where
    whenWalking' :: Object o => StatusError -> o -> o
    whenWalking' (Poison n) o = setHp (hpOf o - n) o
    whenWalking' (Fear   t) o = (if t > 1 then addStatusError (Fear $ t - 1) else id) $ removeStatusError (Fear t) o
    whenWalking' _ o = o

whenBattleEnd :: Object o => o -> o
whenBattleEnd c = foldl (&) c (whenBattleEnd' <$> statusErrorsOf c) 
  where
    whenBattleEnd' :: Object o => StatusError -> o -> o
    whenBattleEnd' Silence = removeStatusError Silence
    whenBattleEnd' Sleep   = removeStatusError Sleep
    whenBattleEnd' Hidden  = removeStatusError Hidden
    whenBattleEnd' Found   = removeStatusError Found
    whenBattleEnd' _       = id

areSameStatusError :: StatusError -> StatusError -> Bool
areSameStatusError (Poison _) (Poison _) = True
areSameStatusError (Fear _) (Fear _)     = True
areSameStatusError s1 s2                 = s1 == s2

hasStatusError :: Object o => o -> StatusError -> Bool
hasStatusError o s = any (areSameStatusError s) (statusErrorsOf o)

isCantFight :: Object o => o -> Bool
isCantFight o = any (hasStatusError o) cantFightStatus

mustGotoTemple :: Object o => o -> Bool
mustGotoTemple = any (\s -> s >= Dead || s == Paralysis || s == Stoned) . statusErrorsOf

cantFightStatus :: [StatusError]
cantFightStatus = [ Paralysis
                  , Stoned
                  , Fear 0
                  , Sleep
                  , Dead
                  , Ash
                  , Lost]

cantSpellStatus :: [StatusError]
cantSpellStatus = [ Silence
                  , Paralysis
                  , Stoned
                  , Fear 0
                  , Sleep
                  , Dead
                  , Ash
                  , Lost]


-- ==========================================================================
-- functions
-- --------------------------------------------------------------------------

leftTxt :: Show a => Int -> a -> String
leftTxt n = leftString n . show

leftString :: Int -> String -> String
leftString n t1 = takeChar n $ t1 ++ repeat ' '

rightTxt :: Show a => Int -> a -> String
rightTxt n = rightString n . show

rightString :: Int -> String -> String
rightString n t1 = reverse . takeChar n $ reverse t1 ++ repeat ' '

takeChar :: Int -> String -> String
takeChar 0 [] = []
takeChar n [] = error "takeChar to empty string."
takeChar n (c:cs)
   | n <= 0       = []
   | isHalfChar c = c : takeChar (n - 1) cs
   | otherwise    = c : takeChar (n - 2) cs

dropChar :: Int -> String -> String
dropChar 0 [] = []
dropChar n [] = error "dropChar to empty string."
dropChar n (c:cs)
   | n <= 0       = c:cs
   | isHalfChar c = dropChar (n - 1) cs
   | otherwise    = dropChar (n - 2) cs

isHalfChar :: Char -> Bool
--isHalfChar c = 0xff61 <= n && n <= 0xff9f -- utf8
isHalfChar c = n <= 0xdf -- cp932
  where n = ord c

