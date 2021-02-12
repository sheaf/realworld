{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O2 -ddump-simpl -dsuppress-all -ddump-to-file #-}

module Inspection where

-- base
import Numeric.Natural
  ( Natural )
import GHC.Exts
  ( RuntimeRep(..), ByteArray#, Int(..)
  , word2Int#, (-#)
  )
import qualified GHC.Exts as GHC

-- ghc-bignum
import qualified GHC.Num.Natural as GHC

---- inspection-testing
--import Test.Inspection
--  ( inspectTest, (===) )
--import qualified Test.Inspection as Inspection
--  ( Result(..) )

-- realworld
import qualified Control.Monad.RealWorld                  as RW
import qualified Control.Monad.State.RealWorld.GHC        as RW
import qualified Control.Monad.State.RealWorld.GHC.BigNum as RW

import Control.Monad.State.RealWorld.Instances.TH
  ( oneRepInstances, twoRepInstances )

--------------------------------------------------------------------------------

oneRepInstances [t|LiftedRep|]
twoRepInstances [t|UnliftedRep|] [t|LiftedRep|]
twoRepInstances [t|TupleRep '[]|] [t|LiftedRep|]
twoRepInstances [t|WordRep|] [t|LiftedRep|]

data ByteArray = ByteArray ByteArray#

intoBytes_explicit :: Int -> Natural -> ByteArray
intoBytes_explicit (I# nbBytes) i = GHC.runRW# \ s1 ->
  case GHC.newByteArray# nbBytes s1 of
    (# s2, mba #) -> case GHC.naturalToMutableByteArray# i mba 0## 0# s2 of
      (# s3, bytesWritten' #) ->
        let bytesWritten = word2Int# bytesWritten' in
          case GHC.setByteArray# mba bytesWritten ( nbBytes -# bytesWritten ) 0# s3 of
            s4 -> case GHC.unsafeFreezeByteArray# mba s4 of
              (# _, ba #) -> ByteArray ba

intoBytes_realworld :: Int -> Natural -> ByteArray
intoBytes_realworld (I# nbBytes) i = RW.runRWS# RW.do
  mba <- RW.newByteArray# nbBytes
  bytesWritten' <- RW.naturalToMutableByteArray# i mba 0## 0#
  let bytesWritten = word2Int# bytesWritten'
  RW.setByteArray# mba bytesWritten ( nbBytes -# bytesWritten ) 0#
  ba <- RW.unsafeFreezeByteArray# mba
  RW.pure ( ByteArray ba )

{-
--------------------------------------------------------------------------------
-- Cabal test setup.

tests :: IO [ Cabal.Test ]
tests = pure $ map mkCabalTest inspectionResults

inspectionResults :: [ ( String, Inspection.Result ) ]
inspectionResults =
  [ ( "intoBytes", $( inspectTest $ 'intoBytes_explicit === 'intoBytes_realworld ) )
  ]

mkCabalTest :: ( String, Inspection.Result ) -> Cabal.Test
mkCabalTest ( testName, inspectionResult ) =
  Cabal.Test $
    Cabal.TestInstance
      { Cabal.run  = pure . Cabal.Finished . cabalResult $ inspectionResult
      , Cabal.name = testName
      , Cabal.tags = []
      , Cabal.options   = []
      , Cabal.setOption = \ _ _ -> Left "Test does not have any options."
      }
-}
