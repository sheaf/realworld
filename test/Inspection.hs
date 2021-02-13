{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -O2 -ddump-simpl -dsuppress-all -ddump-to-file #-}

module Inspection where

-- base
import Numeric.Natural
  ( Natural )
import GHC.Exts
  ( RuntimeRep(..)
  , ByteArray#, Int(..), Int#, RealWorld
  , Word#
  , (+#), (-#), (==#), (>=#)
  , eqWord#, int2Word#, isTrue#, subWordC#, word2Int#
  )
import qualified GHC.Exts as GHC

-- ghc-bignum
import qualified GHC.Num.Natural   as GHC
import qualified GHC.Num.WordArray as GHC

---- inspection-testing
--import Test.Inspection
--  ( inspectTest, (===) )
--import qualified Test.Inspection as Inspection
--  ( Result(..) )

-- realworld
import qualified RealWorld.Monad       as RW
import qualified RealWorld.Monad.State as RW
import qualified RealWorld.GHC         as RW
import qualified RealWorld.GHC.BigNum  as RW
import RealWorld.GHC.BigNum 
  ( Bool#, WordArray#, MutableWordArray# )

--------------------------------------------------------------------------------

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

bignat_sub_word_explicit
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> GHC.State# RealWorld
   -> (# GHC.State# RealWorld, Bool# #)
bignat_sub_word_explicit mwa wa b = go b 0#
   where
      !sz = GHC.wordArraySize# wa
      go carry i s
         | isTrue# (i >=# sz)
         = (# s, carry `eqWord#` 0## #)

         | 0## <- carry
         = case GHC.mwaArrayCopy# mwa i wa i (sz -# i) s of
            s' -> (# s', 1# #) -- no underflow

         | True
         = case subWordC# (GHC.indexWordArray# wa i) carry of
            (# 0##, 0# #)
               | isTrue# (i ==# sz) -> case GHC.mwaShrink# mwa 1# s of
                                          s' -> (# s', 1# #) -- no underflow

            (# l  , c  #) -> case GHC.mwaWrite# mwa i l s of
                              s1 -> go (int2Word# c) (i +# 1#) s1

bignat_sub_word_realworld
   :: MutableWordArray# RealWorld
   -> WordArray#
   -> Word#
   -> RW.StateS# RealWorld Bool#
bignat_sub_word_realworld mwa wa b = go b 0#
   where
      !sz = RW.wordArraySize# wa
      go carry i
         | isTrue# (i >=# sz)
         = RW.pure ( carry `eqWord#` 0## )

         | 0## <- carry
         = RW.do
              RW.mwaArrayCopy# mwa i wa i (sz -# i)
              RW.pure 1# -- no underflow

         | True
         = case subWordC# (RW.indexWordArray# wa i) carry of
            (# 0##, 0# #)
              | isTrue# (i ==# sz)
              -> RW.do
                RW.mwaShrink# mwa 1#
                RW.pure 1# -- no underflow
            (# l  , c  #)
              -> RW.do
                RW.mwaWrite# mwa i l
                go (int2Word# c) (i +# 1#)

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
