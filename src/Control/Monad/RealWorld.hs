{-|
Module: Control.Monad.RealWorld

Typeclasses for levity-polymorphic monadic operations, to circumvent levity polymorphism restrictions.

Import this module with @QualifiedDo@ or @RebindableSyntax@ to use these for monadic @do@ blocks.
-}

module Control.Monad.RealWorld where

-- base
import Prelude hiding ( Functor(..), Applicative(..), Monad(..) )
import Data.Kind
  ( Constraint, Type )
import GHC.Exts
  ( RuntimeRep(..), TYPE, RealWorld )

--------------------------------------------------------------------------------

-- | Levity polymorphic 'Control.Applicative.pure'.
type Pure# :: RuntimeRep -> ( forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class Pure# rep1 f where
  pure
    :: forall ( a :: TYPE rep1 )
    .  a -> f a

-- | Levity polymorphic 'Data.Functor.fmap'.
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
-- | Levity polymorphic '(Control.Monad.>>=)'.
type Bind# :: RuntimeRep -> RuntimeRep -> ( forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class Bind# rep1 rep2 f where
  (>>=)
    :: forall ( a :: TYPE rep1 ) ( b :: TYPE rep2 )
    .  f a -> ( a -> f b ) -> f b
  (>>)
    :: forall ( a :: TYPE rep1 ) ( b :: TYPE rep2 )
    .  f a -> f b -> f b

-- | Version of 'GHC.Exts.runRW#' for state-passing monads.
type RunRWS# :: RuntimeRep -> ( Type -> forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class RunRWS# rep1 f where
  runRWS#
    :: forall ( a :: TYPE rep1 )
    .  f RealWorld a -> a
