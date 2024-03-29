{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: RealWorld.Monad.State

Representation-polymorphic 'RealWorld' token state-passing monad.
-}

module RealWorld.Monad.State
  ( StateS#(..) )
  where

-- base
import Data.Kind
  ( Type )
import GHC.Exts
  ( RuntimeRep(..), State#, TYPE )
import GHC.TypeLits
  ( TypeError, ErrorMessage(..) )

-- realworld
import RealWorld.Monad

--------------------------------------------------------------------------------

-- | A representation-polymorphic state monad for 'State#' tokens.
type StateS# :: Type -> forall ( rep :: RuntimeRep ). TYPE rep -> Type
newtype StateS# s a = StateS# { runStateS# :: State# s -> (# State# s, a #) }

instance {-# OVERLAPPABLE #-}
  TypeError
    (    Text "No instance for"
    :$$: Text "  " :<>: ShowType ( Pure# rep ( StateS# s ) )
    :$$: Text "due to representation-polymorphism restrictions."
    :$$: Text ""
    :$$: Text "If " :<>: ShowType rep :<>: Text " is not polymorphic,"
    :$$: Text "you can define this instance manually, using the following Template Haskell splice:"
    :$$: Text ""
    :$$: Text "Control.Monad.State.RealWorld.Instances.TH.oneRepInstances"
    :$$: Text "    [t|" :<>: ShowType rep :<>: Text "|]"
    :$$: Text ""
    :$$: Text "Warning: this will define orphan instances."
    :$$: Text ""
    )
  => Pure# rep ( StateS# s ) where
  pure = error "no implementation for 'Pure#(pure)'"

instance {-# OVERLAPPABLE #-}
  TypeError
    (    Text "No instance for"
    :$$: Text "  " :<>: ShowType ( RunRWS# rep StateS# )
    :$$: Text "due to representation-polymorphism restrictions."
    :$$: Text ""
    :$$: Text "If " :<>: ShowType rep :<>: Text " is not polymorphic,"
    :$$: Text "you can define this instance manually, using the following Template Haskell splice:"
    :$$: Text ""
    :$$: Text "Control.Monad.State.RealWorld.Instances.TH.oneRepInstances"
    :$$: Text "    [t|" :<>: ShowType rep :<>: Text "|]"
    :$$: Text ""
    :$$: Text "Warning: this will define orphan instances."
    :$$: Text ""
    )
  => RunRWS# rep StateS# where
  runRWS# = error "no implementation for 'RunRWS#(runRWS#)'"

instance {-# OVERLAPPABLE #-}
  TypeError
    (    Text "No instance for"
    :$$: Text "  " :<>: ShowType ( Fmap# rep1 rep2 ( StateS# s ) )
    :$$: Text "due to representation-polymorphism restrictions."
    :$$: Text ""
    :$$: Text "If neither " :<>: ShowType rep1 :<>: Text " nor " :<>: ShowType rep2 :<>: Text " are polymorphic,"
    :$$: Text "you can define this instance manually, using the following Template Haskell splice:"
    :$$: Text ""
    :$$: Text "Control.Monad.State.RealWorld.Instances.TH.twoRepInstances"
    :$$: Text "    [t|" :<>: ShowType rep1 :<>: Text "|] [t|" :<>: ShowType rep2 :<>: Text "|]"
    :$$: Text ""
    :$$: Text "Warning: this will define orphan instances."
    :$$: Text ""
    )
  => Fmap# rep1 rep2 ( StateS# s ) where
  fmap = error "no implementation for 'Fmap#(fmap)'"

instance {-# OVERLAPPABLE #-}
  TypeError
    (    Text "No instance for"
    :$$: Text "  " :<>: ShowType ( Bind# rep1 rep2 ( StateS# s ) )
    :$$: Text "due to representation-polymorphism restrictions."
    :$$: Text ""
    :$$: Text "If neither " :<>: ShowType rep1 :<>: Text " nor " :<>: ShowType rep2 :<>: Text " are polymorphic,"
    :$$: Text "you can define this instance manually, using the following Template Haskell splice:"
    :$$: Text ""
    :$$: Text "Control.Monad.State.RealWorld.Instances.TH.twoRepInstances"
    :$$: Text "    [t|" :<>: ShowType rep1 :<>: Text "|] [t|" :<>: ShowType rep2 :<>: Text "|]"
    :$$: Text ""
    :$$: Text "Warning: this will define orphan instances."
    :$$: Text ""
    )
  => Bind# rep1 rep2 ( StateS# s ) where
  (>>=) = error "no implementation for 'Bind#((>>=))'"
  (>>)  = error "no implementation for 'Bind#((>>))'"

instance Fail# rep ( StateS# s ) where
  fail str = error ( "StateS# fail: " <> str )
