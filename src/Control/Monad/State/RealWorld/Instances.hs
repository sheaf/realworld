{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.State.RealWorld.Instances
  ( )
  where

-- base
import Prelude hiding ( Functor(..), Applicative(..), Monad(..) )
import GHC.Exts
  ( RuntimeRep(..) )

-- realworld
import Control.Monad.State.RealWorld.Instances.TH

--------------------------------------------------------------------------------

--declareRuntimeRepInstances

oneRepInstances [t|LiftedRep|]
oneRepInstances [t|IntRep|]
twoRepInstances [t|UnliftedRep|] [t|LiftedRep|]
twoRepInstances [t|TupleRep '[]|] [t|LiftedRep|]
twoRepInstances [t|TupleRep '[]|] [t|IntRep|]
twoRepInstances [t|WordRep|] [t|LiftedRep|]
