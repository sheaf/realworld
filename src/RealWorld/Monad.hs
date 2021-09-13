{-|
Module: RealWorld.Monad

Typeclasses for representation-polymorphic monadic operations, to circumvent representation polymorphism restrictions.

Import this module with @QualifiedDo@ or @RebindableSyntax@ to use these for monadic @do@ blocks.
-}

module RealWorld.Monad where

-- base
import Prelude hiding ( Functor(..), Applicative(..), Monad(..) )
import Data.Kind
  ( Constraint, Type )
import GHC.Exts
  ( RuntimeRep(..), TYPE, RealWorld )

--------------------------------------------------------------------------------

-- | Representation-polymorphic 'Control.Applicative.pure'.
type Pure# :: RuntimeRep -> ( forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class Pure# rep1 f where
  pure
    :: forall ( a :: TYPE rep1 )
    .  a -> f a

-- | Representation-polymorphic 'Data.Functor.fmap'.
type Fmap# :: RuntimeRep -> RuntimeRep -> ( forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class Fmap# rep1 rep2 f where
  fmap
    :: forall ( a :: TYPE rep1 ) ( b :: TYPE rep2 )
    .  ( a -> b ) -> ( f a -> f b )

infixl 4 <$>
(<$>)
  :: forall
        ( rep1 :: RuntimeRep ) ( a :: TYPE rep1 )
        ( rep2 :: RuntimeRep ) ( b :: TYPE rep2 )
        ( f :: forall ( rep :: RuntimeRep ). TYPE rep -> Type )
  .  Fmap# rep1 rep2 f
  => ( a -> b ) -> ( f a -> f b )
(<$>) = fmap

infixl 1 >>=
-- | Representation-polymorphic '(Control.Monad.>>=)'.
type Bind# :: RuntimeRep -> RuntimeRep -> ( forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class Bind# rep1 rep2 f where
  (>>=)
    :: forall ( a :: TYPE rep1 ) ( b :: TYPE rep2 )
    .  f a -> ( a -> f b ) -> f b
  (>>)
    :: forall ( a :: TYPE rep1 ) ( b :: TYPE rep2 )
    .  f a -> f b -> f b

-- | Representation-polymorphic '(Control.Monad.Fail.fail)'.
type Fail# :: RuntimeRep -> ( forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class Fail# rep1 f where
  fail
    :: forall ( a :: TYPE rep1 )
    .  String -> f a

-- | Version of 'GHC.Exts.runRW#' for state-passing monads.
type RunRWS# :: RuntimeRep -> ( Type -> forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class RunRWS# rep1 f where
  runRWS#
    :: forall ( a :: TYPE rep1 )
    .  f RealWorld a -> a

--------------------------------------------------------------------------------
-- Utility functions.

when, unless :: forall ( f :: forall ( rep :: RuntimeRep ). TYPE rep -> Type )
             .  Pure# ( TupleRep '[] ) f
             => Bool -> f (# #) -> f (# #)
when c a
  | c
  = a
  | otherwise
  = pure (# #)
unless c a
  | c
  = pure (# #)
  | otherwise
  = a

void :: forall ( f :: forall ( rep :: RuntimeRep ). TYPE rep -> Type )
               { rep :: RuntimeRep } ( a :: TYPE rep)
     .  ( Pure# ( TupleRep '[] ) f
        , Bind# rep ( TupleRep '[] ) f
        )
     => f a -> f (# #)
void a = a >> pure (# #)
