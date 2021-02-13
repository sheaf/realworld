{-# LANGUAGE TemplateHaskell #-}

{-|
Module: Control.Monad.State.RealWorld.Instances.TH

Template Haskell functionality for defining levity polymorphic monadic operations.
-}

module Control.Monad.State.RealWorld.Instances.TH where

-- base
import qualified Prelude
import Prelude hiding ( Functor(..), Applicative(..), Monad(..) )
import Data.Traversable
  ( for )
import GHC.Exts
  ( RuntimeRep(..), runRW# )

-- template-haskell
import qualified Language.Haskell.TH.Syntax as TH

-- realworld
import Control.Monad.RealWorld
  ( Pure#(..), Fmap#(..), Bind#(..), RunRWS#(..) )
import Control.Monad.State.RealWorld
  ( StateS#(..) )

--------------------------------------------------------------------------------

oneRepInstances :: TH.Q TH.Type -> TH.Q [TH.Dec]
oneRepInstances rep =
  [d|
    instance Pure# $rep ( StateS# s ) where
      pure a = StateS# \ s -> (# s, a #)

    instance RunRWS# $rep StateS# where
      runRWS# ( StateS# s_to_sa ) =
        $( [e|runRW#|] ) \ s ->
          case s_to_sa s of
            (# _, a #) -> a
  |]

twoRepInstances :: TH.Q TH.Type -> TH.Q TH.Type -> TH.Q [TH.Dec]
twoRepInstances rep1 rep2 =
  [d|
    instance Fmap# $rep1 $rep2 ( StateS# s ) where
      fmap f ( StateS# g ) =
        StateS# \ s1 ->
          case g s1 of
            (# s2, a #) -> (# s2, f a #)
    
    instance Bind# $rep1 $rep2 ( StateS# s ) where
      ( StateS# s1_to_s2a ) >>= f =
        StateS# \ s1 ->
          case s1_to_s2a s1 of
            (# s2, a #) -> case f a of
              StateS# s2_to_s3b -> s2_to_s3b s2
      ( StateS# s1_to_s2a ) >> ( StateS# s2_to_s3b ) =
        StateS# \ s1 ->
          case s1_to_s2a s1 of
            (# s2, _ #) -> s2_to_s3b s2
  |]

declareRuntimeRepInstances :: TH.Q [TH.Dec]
declareRuntimeRepInstances = do
  oneDecls <- concat <$> for reps oneRepInstances
  twoDecls <- concat <$> for [ ( rep1, rep2 ) | rep1 <- reps, rep2 <- reps ] ( uncurry twoRepInstances )

  Prelude.pure ( oneDecls <> twoDecls )

  where
    basicReps :: [ TH.Q TH.Type ]
    basicReps =
      [ [t|LiftedRep|]
      , [t|UnliftedRep|]
      , [t|IntRep|]
      , [t|WordRep|]
      , [t|Int64Rep|]
      , [t|Word64Rep|]
      , [t|AddrRep|]
      , [t|FloatRep|]
      , [t|DoubleRep|]
      , [t|TupleRep '[]|]
      , [t|SumRep '[]|] 
      ]

    bakedInReps :: [ TH.Q TH.Type ]
    bakedInReps =
      [ [t|TupleRep '[IntRep, LiftedRep]|]
      , [t|TupleRep '[IntRep, UnliftedRep]|]
      , [t|TupleRep '[LiftedRep, LiftedRep]|]
      , [t|TupleRep '[UnliftedRep, LiftedRep]|]
      , [t|TupleRep '[LiftedRep, UnliftedRep]|]
      , [t|TupleRep '[UnliftedRep, UnliftedRep]|]
      , [t|TupleRep '[IntRep, IntRep, IntRep]|]
      , [t|TupleRep '[AddrRep, WordRep]|]
      , [t|TupleRep '[UnliftedRep, AddrRep]|]
      ]

    reps :: [ TH.Q TH.Type ]
    reps = basicReps <> bakedInReps

  --  tupleRep :: [ TH.Q TH.Type ] -> TH.Q TH.Type
  --  tupleRep [a1] = [t|TupleRep '[ $a1 ]|]
  --  tupleRep [a1,a2] = [t|TupleRep '[ $a1, $a2 ]|]
  --  tupleRep [a1,a2,a3] = [t|TupleRep '[ $a1, $a2, $a3 ]|]
  --  tupleRep [a1,a2,a3,a4] = [t|TupleRep '[ $a1, $a2, $a3, $a4 ]|]
  --  tupleRep [a1,a2,a3,a4,a5] = [t|TupleRep '[ $a1, $a2, $a3, $a4, $a5 ]|]
  --  tupleRep [a1,a2,a3,a4,a5,a6] = [t|TupleRep '[ $a1, $a2, $a3, $a4, $a5, $a6 ]|]
  --  tupleRep _ = undefined

  --  sumRep :: [ TH.Q TH.Type ] -> TH.Q TH.Type
  --  sumRep [a1,a2] = [t|SumRep '[ $a1, $a2 ]|]
  --  sumRep [a1,a2,a3] = [t|SumRep '[ $a1, $a2, $a3 ]|]
  --  sumRep [a1,a2,a3,a4] = [t|SumRep '[ $a1, $a2, $a3, $a4 ]|]
  --  sumRep _ = undefined

  --one :: [ TH.Q TH.Type ]
  --one
  --  =  [ tupleRep [a1] | a1 <- zero ]
  --  <> [ tupleRep [a1,a2] | a1 <- zero, a2 <- zero ]
  --  <> [ tupleRep [a1,a2,a3] | a1 <- zero, a2 <- zero, a3 <- zero ]
  --  <> [ tupleRep [a1,a2,a3,a4] | a1 <- zero, a2 <- zero, a3 <- zero, a4 <- zero ]
  --  <> [ tupleRep [a1,a2,a3,a4,a5] | a1 <- zero, a2 <- zero, a3 <- zero, a4 <- zero, a5 <- zero ]
  --  <> [ tupleRep [a1,a2,a3,a4,a5,a6] | a1 <- zero, a2 <- zero, a3 <- zero, a4 <- zero, a5 <- zero, a6 <- zero ]
  --  <> [ sumRep [a1,a2] | a1 <- zero, a2 <- zero ]
  --  <> [ sumRep [a1,a2,a3] | a1 <- zero, a2 <- zero, a3 <- zero ]
  --  <> [ sumRep [a1,a2,a3,a4] | a1 <- zero, a2 <- zero, a3 <- zero, a4 <- zero ]


    --two :: [ TH.Q TH.Type ]
    --two
    --  =  [ tupleRep [a1] | a1 <- one ]
    --  <> [ tupleRep [a1,a2] | a1 <- one, a2 <- one ]
    --  <> [ tupleRep [a1,a2,a3] | a1 <- one, a2 <- one, a3 <- one ]
    --  <> [ tupleRep [a1,a2,a3,a4] | a1 <- one, a2 <- one, a3 <- one, a4 <- one ]
    --  <> [ sumRep [a1,a2] | a1 <- one, a2 <- one ]
    --  <> [ sumRep [a1,a2,a3] | a1 <- one, a2 <- one, a3 <- one ]

    --three :: [ TH.Q TH.Type ]
    --three
    --  =  [ tupleRep [a1] | a1 <- two ]
    --  <> [ tupleRep [a1,a2] | a1 <- two, a2 <- two ]
    --  <> [ tupleRep [a1,a2,a3] | a1 <- two, a2 <- two, a3 <- two ]
    --  <> [ sumRep [a1,a2] | a1 <- two, a2 <- two ]

    --reps :: [ TH.Q TH.Type ]
    --reps = zero <> one <> two <> three
