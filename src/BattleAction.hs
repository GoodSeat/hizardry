{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BattleAction
where

import Control.Monad.State
import Control.Monad.Reader

import World
import qualified Enemies as Enemy
import qualified Characters as Character


data Effect = AffectCharacter (Character.ID, Character.Character -> Character.Character)
            | AffectEnemy     (Int, Enemy.Instance, Enemy.Instance -> Enemy.Instance)

-- | State used in game.
newtype Action o = Action { exec :: StateT World (Reader Enemy.DB) o }
    deriving (Functor, Applicative, Monad, MonadReader Enemy.DB, MonadState World)

runAction :: World
          -> Enemy.DB
          -> Action [Effect]
          -> ([Effect], World)
runAction w edb act = runReader (runStateT (exec act) w) edb
