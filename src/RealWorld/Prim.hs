{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

#include "MachDeps.h"

module RealWorld.Prim
  ( Prim(..) )
  where

-- realworld
import RealWorld.GHC
import RealWorld.Prim.Class
  ( Prim(..), primInstance )

--------------------------------------------------------------------------------

primInstance
  [t|Int8#|] 1# 1#
  [e|indexInt8Array#|]
  [e|readInt8Array#|]
  [e|writeInt8Array#|]
primInstance
  [t|Int16#|] 2# 2#
  [e|indexWord8ArrayAsInt16#|]
  [e|readWord8ArrayAsInt16#|]
  [e|writeWord8ArrayAsInt16#|]
primInstance
  [t|Int32#|] 4# 4#
  [e|indexWord8ArrayAsInt32#|]
  [e|readWord8ArrayAsInt32#|]
  [e|writeWord8ArrayAsInt32#|]
#if WORD_SIZE_IN_BITS < 64
primInstance
  [t|Int64#|] 8# 8#
  [e|indexWord8ArrayAsInt64#|]
  [e|readWord8ArrayAsInt64#|]
  [e|writeWord8ArrayAsInt64#|]
#else
primInstance
  [t|Int#|] 8# 8#
  [e|indexWord8ArrayAsInt#|]
  [e|readWord8ArrayAsInt#|]
  [e|writeWord8ArrayAsInt#|]
#endif

primInstance
  [t|Word8#|] 1# 1#
  [e|indexWord8Array#|]
  [e|readWord8Array#|]
  [e|writeWord8Array#|]
primInstance
  [t|Word16#|] 2# 2#
  [e|indexWord8ArrayAsWord16#|]
  [e|readWord8ArrayAsWord16#|]
  [e|writeWord8ArrayAsWord16#|]
primInstance
  [t|Word32#|] 4# 4#
  [e|indexWord8ArrayAsWord32#|]
  [e|readWord8ArrayAsWord32#|]
  [e|writeWord8ArrayAsWord32#|]
#if WORD_SIZE_IN_BITS < 64
primInstance
  [t|Word64#|] 8# 8#
  [e|indexWord8ArrayAsWord64#|]
  [e|readWord8ArrayAsWord64#|]
  [e|writeWord8ArrayAsWord64#|]
#else
primInstance
  [t|Word#|] 8# 8#
  [e|indexWord8ArrayAsWord#|]
  [e|readWord8ArrayAsWord#|]
  [e|writeWord8ArrayAsWord#|]
#endif

primInstance
  [t|Char#|] 4# 4#
  [e|indexWord8ArrayAsWideChar#|]
  [e|readWord8ArrayAsWideChar#|]
  [e|writeWord8ArrayAsWideChar#|]

primInstance
  [t|Float#|] 4# 4#
  [e|indexWord8ArrayAsFloat#|]
  [e|readWord8ArrayAsFloat#|]
  [e|writeWord8ArrayAsFloat#|]
primInstance
  [t|Double#|] 8# 8#
  [e|indexWord8ArrayAsDouble#|]
  [e|readWord8ArrayAsDouble#|]
  [e|writeWord8ArrayAsDouble#|]
