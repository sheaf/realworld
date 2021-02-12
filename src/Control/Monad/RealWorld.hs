module Control.Monad.RealWorld where

-- base
import Prelude hiding ( Functor(..), Applicative(..), Monad(..) )
import Data.Kind
  ( Constraint, Type )
import GHC.Exts
  ( RuntimeRep(..), TYPE, RealWorld )

--------------------------------------------------------------------------------

type Pure# :: RuntimeRep -> ( forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class Pure# rep1 f where
  pure
    :: forall ( a :: TYPE rep1 )
    .  a -> f a

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
type Bind# :: RuntimeRep -> RuntimeRep -> ( forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class Bind# rep1 rep2 f where
  (>>=)
    :: forall ( a :: TYPE rep1 ) ( b :: TYPE rep2 )
    .  f a -> ( a -> f b ) -> f b
  (>>)
    :: forall ( a :: TYPE rep1 ) ( b :: TYPE rep2 )
    .  f a -> f b -> f b

type RunRWS# :: RuntimeRep -> ( Type -> forall ( rep :: RuntimeRep ). TYPE rep -> Type ) -> Constraint
class RunRWS# rep1 f where
  runRWS#
    :: forall ( a :: TYPE rep1 )
    .  f RealWorld a -> a
