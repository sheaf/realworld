{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.State.RealWorld
  ( StateS#(..) )
  where

-- base
import Data.Kind
  ( Type )
import GHC.Exts
  ( RuntimeRep(..), State#, TYPE )

--------------------------------------------------------------------------------

type StateS# :: Type -> forall ( rep :: RuntimeRep ). TYPE rep -> Type
newtype StateS# s a = StateS# { runStateS# :: State# s -> (# State# s, a #) }
