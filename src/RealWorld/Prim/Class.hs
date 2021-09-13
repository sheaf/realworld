{-# LANGUAGE TemplateHaskell #-}

module RealWorld.Prim.Class
  ( Prim(..)
  , primInstance
  ) where

-- base
import Data.Kind
  ( Constraint )
import GHC.Exts
  ( ByteArray#, MutableByteArray#
  , Int#, Proxy#, RuntimeRep, TYPE
  )

-- template-haskell
import qualified Language.Haskell.TH.Syntax as TH

-- realworld
import RealWorld.Monad.State
  ( StateS# )

--------------------------------------------------------------------------------

type  Prim :: forall {rep :: RuntimeRep}. TYPE rep -> Constraint
class Prim a where

  -- | Size of the type 'a' in bytes.
  sizeOf# :: Proxy# a -> Int#

  -- | ALignment of the type 'a' in bytes.
  alignment# :: Proxy# a -> Int#

  -- | Index into a 'ByteArray#' at the given byte offset.
  index# :: ByteArray#
         -> Int# -- offset (in bytes)
         -> a

  -- | Read a value of type 'a' at the given byte offset.
  read# :: MutableByteArray# s
        -> Int# -- offset (in bytes)
        -> StateS# s a

  -- | Write a value of type 'a' at the given byte offset.
  write# :: MutableByteArray# s
         -> Int# -- offset (in bytes)
         -> a
         -> StateS# s (##)

--------------------------------------------------------------------------------

primInstance
  :: TH.Q TH.Type
  -> Int#
  -> Int#
  -> TH.Q TH.Exp
  -> TH.Q TH.Exp
  -> TH.Q TH.Exp
  -> TH.Q [TH.Dec]
primInstance ty sz ali idx rd write =
  [d|
    instance Prim $ty where
      sizeOf# _ = sz
      alignment# _ = ali
      index# ba  off   = $idx   ba  off
      read#  mba off   = $rd    mba off
      write# mba off v = $write mba off v
  |]
