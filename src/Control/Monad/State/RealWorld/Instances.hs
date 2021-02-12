{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.State.RealWorld.Instances
  ( )
  where

-- base
import Prelude hiding ( Functor(..), Applicative(..), Monad(..) )
import GHC.Exts
  ( RuntimeRep(..), runRW# )

-- realworld
import Control.Monad.State.RealWorld.Instances.TH

--------------------------------------------------------------------------------

--declareRuntimeRepInstances
