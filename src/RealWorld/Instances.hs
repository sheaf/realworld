{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module RealWorld.Instances
  ( )
  where

-- base
import Prelude hiding ( Functor(..), Applicative(..), Monad(..) )

-- realworld
import RealWorld.Instances.TH
  ( declareRuntimeRepInstances )

--------------------------------------------------------------------------------

declareRuntimeRepInstances
