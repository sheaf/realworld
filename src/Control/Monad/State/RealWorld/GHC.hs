{-# LANGUAGE CPP #-}

#include "MachDeps.h"
#define INT32 Int#
#define WORD32 Word#

#if WORD_SIZE_IN_BITS < 64
#define INT64 Int64#
#define WORD64 Word64#
#else
#define INT64 Int#
#define WORD64 Word#
#endif

module Control.Monad.State.RealWorld.GHC
  (
  -- * Functions from "GHC.Exts" adapted to use 'StateS#'
    newArray#
  , writeArray#
  , unsafeFreezeArray#
  , unsafeThawArray#
  , copyArray#
  , copyMutableArray#
  , cloneMutableArray#
  , freezeArray#
  , thawArray#
  , casArray#
  , newSmallArray#
  , readSmallArray#
  , writeSmallArray#
  , getSizeofSmallMutableArray#
  , unsafeFreezeSmallArray#
  , unsafeThawSmallArray#
  , copySmallArray#
  , copySmallMutableArray#
  , cloneSmallMutableArray#
  , freezeSmallArray#
  , thawSmallArray#
  , casSmallArray#
  , newByteArray#
  , newPinnedByteArray#
  , newAlignedPinnedByteArray#
  , shrinkMutableByteArray#
  , resizeMutableByteArray#
  , unsafeFreezeByteArray#
  , getSizeofMutableByteArray#
  , readCharArray#
  , readWideCharArray#
  , readIntArray#
  , readWordArray#
  , readAddrArray#
  , readFloatArray#
  , readDoubleArray#
  , readStablePtrArray#
  , readInt8Array#
  , readInt16Array#
  , readInt32Array#
  , readInt64Array#
  , readWord8Array#
  , readWord16Array#
  , readWord32Array#
  , readWord64Array#
  , readWord8ArrayAsChar#
  , readWord8ArrayAsWideChar#
  , readWord8ArrayAsAddr#
  , readWord8ArrayAsFloat#
  , readWord8ArrayAsDouble#
  , readWord8ArrayAsStablePtr#
  , readWord8ArrayAsInt16#
  , readWord8ArrayAsInt32#
  , readWord8ArrayAsInt64#
  , readWord8ArrayAsInt#
  , readWord8ArrayAsWord16#
  , readWord8ArrayAsWord32#
  , readWord8ArrayAsWord64#
  , readWord8ArrayAsWord#
  , writeCharArray#
  , writeWideCharArray#
  , writeIntArray#
  , writeWordArray#
  , writeAddrArray#
  , writeFloatArray#
  , writeDoubleArray#
  , writeStablePtrArray#
  , writeInt8Array#
  , writeInt16Array#
  , writeInt32Array#
  , writeInt64Array#
  , writeWord8Array#
  , writeWord16Array#
  , writeWord32Array#
  , writeWord64Array#
  , writeWord8ArrayAsChar#
  , writeWord8ArrayAsWideChar#
  , writeWord8ArrayAsAddr#
  , writeWord8ArrayAsFloat#
  , writeWord8ArrayAsDouble#
  , writeWord8ArrayAsStablePtr#
  , writeWord8ArrayAsInt16#
  , writeWord8ArrayAsInt32#
  , writeWord8ArrayAsInt64#
  , writeWord8ArrayAsInt#
  , writeWord8ArrayAsWord16#
  , writeWord8ArrayAsWord32#
  , writeWord8ArrayAsWord64#
  , writeWord8ArrayAsWord#
  , copyByteArray#
  , copyMutableByteArray#
  , copyByteArrayToAddr#
  , copyMutableByteArrayToAddr#
  , copyAddrToByteArray#
  , setByteArray#
  , atomicReadIntArray#
  , atomicWriteIntArray#
  , casIntArray#
  , fetchAddIntArray#
  , fetchSubIntArray#
  , fetchAndIntArray#
  , fetchNandIntArray#
  , fetchOrIntArray#
  , fetchXorIntArray#
  , newArrayArray#
  , readByteArrayArray#
  , readMutableByteArrayArray#
  , readArrayArrayArray#
  , readMutableArrayArrayArray#
  , writeByteArrayArray#
  , writeMutableByteArrayArray#
  , writeArrayArrayArray#
  , writeMutableArrayArrayArray#
  , copyArrayArray#
  , copyMutableArrayArray#
  , readCharOffAddr#
  , readWideCharOffAddr#
  , readIntOffAddr#
  , readWordOffAddr#
  , readAddrOffAddr#
  , readFloatOffAddr#
  , readDoubleOffAddr#
  , readStablePtrOffAddr#
  , readInt8OffAddr#
  , readInt16OffAddr#
  , readInt32OffAddr#
  , readInt64OffAddr#
  , readWord8OffAddr#
  , readWord16OffAddr#
  , readWord32OffAddr#
  , readWord64OffAddr#
  , writeCharOffAddr#
  , writeWideCharOffAddr#
  , writeIntOffAddr#
  , writeWordOffAddr#
  , writeAddrOffAddr#
  , writeFloatOffAddr#
  , writeDoubleOffAddr#
  , writeStablePtrOffAddr#
  , writeInt8OffAddr#
  , writeInt16OffAddr#
  , writeInt32OffAddr#
  , writeInt64OffAddr#
  , writeWord8OffAddr#
  , writeWord16OffAddr#
  , writeWord32OffAddr#
  , writeWord64OffAddr#
  , atomicExchangeAddrAddr#
  , atomicExchangeWordAddr#
  , atomicCasAddrAddr#
  , atomicCasWordAddr#
  , newMutVar#
  , readMutVar#
  , writeMutVar#
  , atomicModifyMutVar2#
  , atomicModifyMutVar_#
  , casMutVar#
  , newTVar#
  , readTVar#
  , readTVarIO#
  , writeTVar#
  , newMVar#
  , takeMVar#
  , tryTakeMVar#
  , putMVar#
  , tryPutMVar#
  , readMVar#
  , tryReadMVar#
  , newIOPort#
  , readIOPort#
  , writeIOPort#
  , delay#
  , waitRead#
  , waitWrite#
  , noDuplicate#
  , spark#
  , seq#
  , getSpark#
  , numSparks#
  , newBCO#
  , getCCSOf#
  , getCurrentCCS#
  , clearCCS#
  , traceEvent#
  , traceBinaryEvent#
  , traceMarker#
  , prefetchByteArray3#
  , prefetchMutableByteArray3#
  , prefetchAddr3#
  , prefetchValue3#
  , prefetchByteArray2#
  , prefetchMutableByteArray2#
  , prefetchAddr2#
  , prefetchValue2#
  , prefetchByteArray1#
  , prefetchMutableByteArray1#
  , prefetchAddr1#
  , prefetchValue1#
  , prefetchByteArray0#
  , prefetchMutableByteArray0#
  , prefetchAddr0#
  , prefetchValue0#

  -- * Re-exporting the rest of "GHC.Exts"
  , module GHC.Exts
  )
  where

-- base
import qualified GHC.Exts
import GHC.Exts hiding
  ( newArray#
  , writeArray#
  , unsafeFreezeArray#
  , unsafeThawArray#
  , copyArray#
  , copyMutableArray#
  , cloneMutableArray#
  , freezeArray#
  , thawArray#
  , casArray#
  , newSmallArray#
  , readSmallArray#
  , writeSmallArray#
  , getSizeofSmallMutableArray#
  , unsafeFreezeSmallArray#
  , unsafeThawSmallArray#
  , copySmallArray#
  , copySmallMutableArray#
  , cloneSmallMutableArray#
  , freezeSmallArray#
  , thawSmallArray#
  , casSmallArray#
  , newByteArray#
  , newPinnedByteArray#
  , newAlignedPinnedByteArray#
  , shrinkMutableByteArray#
  , resizeMutableByteArray#
  , unsafeFreezeByteArray#
  , getSizeofMutableByteArray#
  , readCharArray#
  , readWideCharArray#
  , readIntArray#
  , readWordArray#
  , readAddrArray#
  , readFloatArray#
  , readDoubleArray#
  , readStablePtrArray#
  , readInt8Array#
  , readInt16Array#
  , readInt32Array#
  , readInt64Array#
  , readWord8Array#
  , readWord16Array#
  , readWord32Array#
  , readWord64Array#
  , readWord8ArrayAsChar#
  , readWord8ArrayAsWideChar#
  , readWord8ArrayAsAddr#
  , readWord8ArrayAsFloat#
  , readWord8ArrayAsDouble#
  , readWord8ArrayAsStablePtr#
  , readWord8ArrayAsInt16#
  , readWord8ArrayAsInt32#
  , readWord8ArrayAsInt64#
  , readWord8ArrayAsInt#
  , readWord8ArrayAsWord16#
  , readWord8ArrayAsWord32#
  , readWord8ArrayAsWord64#
  , readWord8ArrayAsWord#
  , writeCharArray#
  , writeWideCharArray#
  , writeIntArray#
  , writeWordArray#
  , writeAddrArray#
  , writeFloatArray#
  , writeDoubleArray#
  , writeStablePtrArray#
  , writeInt8Array#
  , writeInt16Array#
  , writeInt32Array#
  , writeInt64Array#
  , writeWord8Array#
  , writeWord16Array#
  , writeWord32Array#
  , writeWord64Array#
  , writeWord8ArrayAsChar#
  , writeWord8ArrayAsWideChar#
  , writeWord8ArrayAsAddr#
  , writeWord8ArrayAsFloat#
  , writeWord8ArrayAsDouble#
  , writeWord8ArrayAsStablePtr#
  , writeWord8ArrayAsInt16#
  , writeWord8ArrayAsInt32#
  , writeWord8ArrayAsInt64#
  , writeWord8ArrayAsInt#
  , writeWord8ArrayAsWord16#
  , writeWord8ArrayAsWord32#
  , writeWord8ArrayAsWord64#
  , writeWord8ArrayAsWord#
  , copyByteArray#
  , copyMutableByteArray#
  , copyByteArrayToAddr#
  , copyMutableByteArrayToAddr#
  , copyAddrToByteArray#
  , setByteArray#
  , atomicReadIntArray#
  , atomicWriteIntArray#
  , casIntArray#
  , fetchAddIntArray#
  , fetchSubIntArray#
  , fetchAndIntArray#
  , fetchNandIntArray#
  , fetchOrIntArray#
  , fetchXorIntArray#
  , newArrayArray#
  , readByteArrayArray#
  , readMutableByteArrayArray#
  , readArrayArrayArray#
  , readMutableArrayArrayArray#
  , writeByteArrayArray#
  , writeMutableByteArrayArray#
  , writeArrayArrayArray#
  , writeMutableArrayArrayArray#
  , copyArrayArray#
  , copyMutableArrayArray#
  , readCharOffAddr#
  , readWideCharOffAddr#
  , readIntOffAddr#
  , readWordOffAddr#
  , readAddrOffAddr#
  , readFloatOffAddr#
  , readDoubleOffAddr#
  , readStablePtrOffAddr#
  , readInt8OffAddr#
  , readInt16OffAddr#
  , readInt32OffAddr#
  , readInt64OffAddr#
  , readWord8OffAddr#
  , readWord16OffAddr#
  , readWord32OffAddr#
  , readWord64OffAddr#
  , writeCharOffAddr#
  , writeWideCharOffAddr#
  , writeIntOffAddr#
  , writeWordOffAddr#
  , writeAddrOffAddr#
  , writeFloatOffAddr#
  , writeDoubleOffAddr#
  , writeStablePtrOffAddr#
  , writeInt8OffAddr#
  , writeInt16OffAddr#
  , writeInt32OffAddr#
  , writeInt64OffAddr#
  , writeWord8OffAddr#
  , writeWord16OffAddr#
  , writeWord32OffAddr#
  , writeWord64OffAddr#
  , atomicExchangeAddrAddr#
  , atomicExchangeWordAddr#
  , atomicCasAddrAddr#
  , atomicCasWordAddr#
  , newMutVar#
  , readMutVar#
  , writeMutVar#
  , atomicModifyMutVar2#
  , atomicModifyMutVar_#
  , casMutVar#
  , newTVar#
  , readTVar#
  , readTVarIO#
  , writeTVar#
  , newMVar#
  , takeMVar#
  , tryTakeMVar#
  , putMVar#
  , tryPutMVar#
  , readMVar#
  , tryReadMVar#
  , newIOPort#
  , readIOPort#
  , writeIOPort#
  , delay#
  , waitRead#
  , waitWrite#
  , noDuplicate#
  , spark#
  , seq#
  , getSpark#
  , numSparks#
  , newBCO#
  , getCCSOf#
  , getCurrentCCS#
  , clearCCS#
  , traceEvent#
  , traceBinaryEvent#
  , traceMarker#
  , prefetchByteArray3#
  , prefetchMutableByteArray3#
  , prefetchAddr3#
  , prefetchValue3#
  , prefetchByteArray2#
  , prefetchMutableByteArray2#
  , prefetchAddr2#
  , prefetchValue2#
  , prefetchByteArray1#
  , prefetchMutableByteArray1#
  , prefetchAddr1#
  , prefetchValue1#
  , prefetchByteArray0#
  , prefetchMutableByteArray0#
  , prefetchAddr0#
  , prefetchValue0#
  )

-- realworld
import Control.Monad.State.RealWorld
  ( StateS#(..) )
import Control.Monad.State.RealWorld.Instances
  ( )

--------------------------------------------------------------------------------
-- GHC primops extracted from "primops.txt"

-- | Create a new mutable array with the specified number of elements,
-- in the specified state thread,
-- with each element containing the specified initial value.
newArray#
  :: Int#
  -> a
  -> StateS# s ( MutableArray# s a )
newArray# arg1 arg2 =
  StateS# ( GHC.Exts.newArray# arg1 arg2 )

-- | Write to specified index of mutable array.
writeArray#
  :: MutableArray# s a
  -> Int#
  -> a
  -> StateS# s (##)
writeArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

-- | Make a mutable array immutable, without copying.
unsafeFreezeArray#
  :: MutableArray# s a
  -> StateS# s ( Array# a )
unsafeFreezeArray# arg1 =
  StateS# ( GHC.Exts.unsafeFreezeArray# arg1 )

-- | Make an immutable array mutable, without copying.
unsafeThawArray#
  :: Array# a
  -> StateS# s ( MutableArray# s a )
unsafeThawArray# arg1 =
  StateS# ( GHC.Exts.unsafeThawArray# arg1 )

-- | Given a source array, an offset into the source array, a
-- destination array, an offset into the destination array, and a
-- number of elements to copy, copy the elements from the source array
-- to the destination array. Both arrays must fully contain the
-- specified ranges, but this is not checked. The two arrays must not
-- be the same array in different states, but this is not checked
-- either.
copyArray#
  :: Array# a -- ^ src
  -> Int# -- ^ src offset
  -> MutableArray# s a -- ^ dst
  -> Int# -- ^ dst offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copyArray# arg1 arg2 arg3 arg4 arg5 =
  StateS# \ s# ->
    case GHC.Exts.copyArray# arg1 arg2 arg3 arg4 arg5 s# of
      t# -> (# t#, (##) #)

-- | Given a source array, an offset into the source array, a
-- destination array, an offset into the destination array, and a
-- number of elements to copy, copy the elements from the source array
-- to the destination array. Both arrays must fully contain the
-- specified ranges, but this is not checked. In the case where
-- the source and destination are the same array the source and
-- destination regions may overlap.
copyMutableArray#
  :: MutableArray# s a  -- ^ src
  -> Int# -- ^ src offset
  -> MutableArray# s a -- ^ dst
  -> Int# -- ^ dst offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copyMutableArray# arg1 arg2 arg3 arg4 arg5 =
  StateS# \ s# ->
    case GHC.Exts.copyMutableArray# arg1 arg2 arg3 arg4 arg5 s# of
      t# -> (# t#, (##) #)

-- | Given a source array, an offset into the source array, and a number
-- of elements to copy, create a new array with the elements from the
-- source array. The provided array must fully contain the specified
-- range, but this is not checked.
cloneMutableArray#
  :: MutableArray# s a -- ^ src
  -> Int# -- ^ src offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s ( MutableArray# s a )
cloneMutableArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.cloneMutableArray# arg1 arg2 arg3 )

-- | Given a source array, an offset into the source array, and a number
-- of elements to copy, create a new array with the elements from the
-- source array. The provided array must fully contain the specified
-- range, but this is not checked.
freezeArray#
  :: MutableArray# s a -- ^ src
  -> Int# -- ^ src offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s ( Array# a )
freezeArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.freezeArray# arg1 arg2 arg3 )

-- | Given a source array, an offset into the source array, and a number
-- of elements to copy, create a new array with the elements from the
-- source array. The provided array must fully contain the specified
-- range, but this is not checked.
thawArray#
  :: Array# a -- ^ src
  -> Int# -- ^ src offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s ( MutableArray# s a )
thawArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.thawArray# arg1 arg2 arg3 )

-- | Given an array, an offset, the expected old value, and
-- the new value, perform an atomic compare and swap (i.e. write the new
-- value if the current value and the old value are the same pointer).
-- Returns 0 if the swap succeeds and 1 if it fails. Additionally, returns
-- the element at the offset after the operation completes. This means that
-- on a success the new value is returned, and on a failure the actual old
-- value (not the expected one) is returned. Implies a full memory barrier.
-- The use of a pointer equality on a lifted value makes this function harder
-- to use correctly than 'casIntArray'
casArray#
  :: MutableArray# s a
  -> Int# -- ^ index
  -> a -- ^ expected old value
  -> a -- ^ new value
  -> StateS# s (# Int#, a #)
casArray# arg1 arg2 arg3 arg4 =
  StateS# \ s# ->
    case GHC.Exts.casArray# arg1 arg2 arg3 arg4 s# of
      (# t#, res1, res2 #) -> (# t#, (# res1, res2 #) #)

-- | Create a new mutable array with the specified number of elements,
-- in the specified state thread,
-- with each element containing the specified initial value.
newSmallArray#
  :: Int#
  -> a
  -> StateS# s ( SmallMutableArray# s a )
newSmallArray# arg1 arg2 =
  StateS# ( GHC.Exts.newSmallArray# arg1 arg2 )

-- | Read from specified index of mutable array. Result is not yet evaluated.
readSmallArray#
  :: SmallMutableArray# s a
  -> Int#
  -> StateS# s a
readSmallArray# arg1 arg2 =
  StateS# ( GHC.Exts.readSmallArray# arg1 arg2 )

-- | Write to specified index of mutable array.
writeSmallArray#
  :: SmallMutableArray# s a
  -> Int#
  -> a
  -> StateS# s (##)
writeSmallArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeSmallArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

-- | Return the number of elements in the array.
getSizeofSmallMutableArray#
  :: SmallMutableArray# s a
  -> StateS# s Int#
getSizeofSmallMutableArray# arg1 =
  StateS# ( GHC.Exts.getSizeofSmallMutableArray# arg1 )

-- | Make a mutable array immutable, without copying.
unsafeFreezeSmallArray#
  :: SmallMutableArray# s a
  -> StateS# s ( SmallArray# a )
unsafeFreezeSmallArray# arg1 =
  StateS# ( GHC.Exts.unsafeFreezeSmallArray# arg1 )

-- | Make an immutable array mutable, without copying.
unsafeThawSmallArray#
  :: SmallArray# a
  -> StateS# s ( SmallMutableArray# s a )
unsafeThawSmallArray# arg1 =
  StateS# ( GHC.Exts.unsafeThawSmallArray# arg1 )

-- | Given a source array, an offset into the source array, a
-- destination array, an offset into the destination array, and a
-- number of elements to copy, copy the elements from the source array
-- to the destination array. Both arrays must fully contain the
-- specified ranges, but this is not checked. The two arrays must not
-- be the same array in different states, but this is not checked
-- either.
copySmallArray#
  :: SmallArray# a -- ^ src
  -> Int# -- ^ src offset
  -> SmallMutableArray# s a -- ^ dst
  -> Int# -- ^ dst offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copySmallArray# arg1 arg2 arg3 arg4 arg5 =
  StateS# \ s# ->
    case GHC.Exts.copySmallArray# arg1 arg2 arg3 arg4 arg5 s# of
      t# -> (# t#, (##) #)

-- | Given a source array, an offset into the source array, a
-- destination array, an offset into the destination array, and a
-- number of elements to copy, copy the elements from the source array
-- to the destination array. The source and destination arrays can
-- refer to the same array. Both arrays must fully contain the
-- specified ranges, but this is not checked.
-- The regions are allowed to overlap, although this is only possible when the same
-- array is provided as both the source and the destination.
copySmallMutableArray#
  :: SmallMutableArray# s a -- ^ src
  -> Int# -- ^ src offset
  -> SmallMutableArray# s a -- ^ dst
  -> Int# -- ^ dst offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copySmallMutableArray# arg1 arg2 arg3 arg4 arg5 =
  StateS# \ s# ->
    case GHC.Exts.copySmallMutableArray# arg1 arg2 arg3 arg4 arg5 s# of
      t# -> (# t#, (##) #)

-- | Given a source array, an offset into the source array, and a number
-- of elements to copy, create a new array with the elements from the
-- source array. The provided array must fully contain the specified
-- range, but this is not checked.
cloneSmallMutableArray#
  :: SmallMutableArray# s a
  -> Int# -- ^ offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s ( SmallMutableArray# s a )
cloneSmallMutableArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.cloneSmallMutableArray# arg1 arg2 arg3 )

-- | Given a source array, an offset into the source array, and a number
-- of elements to copy, create a new array with the elements from the
-- source array. The provided array must fully contain the specified
-- range, but this is not checked.
freezeSmallArray#
  :: SmallMutableArray# s a
  -> Int# -- ^ offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s ( SmallArray# a )
freezeSmallArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.freezeSmallArray# arg1 arg2 arg3 )

-- | Given a source array, an offset into the source array, and a number
-- of elements to copy, create a new array with the elements from the
-- source array. The provided array must fully contain the specified
-- range, but this is not checked.
thawSmallArray#
  :: SmallArray# a
  -> Int# -- ^ offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s ( SmallMutableArray# s a )
thawSmallArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.thawSmallArray# arg1 arg2 arg3 )

-- | Unsafe, machine-level atomic compare and swap on an element within an array.
-- See the documentation of 'casArray'
casSmallArray#
  :: SmallMutableArray# s a
  -> Int# -- ^ index
  -> a -- ^ expected old value
  -> a -- ^ new value
  -> StateS# s (# Int#, a #)
casSmallArray# arg1 arg2 arg3 arg4 =
  StateS# \ s# ->
    case GHC.Exts.casSmallArray# arg1 arg2 arg3 arg4 s# of
      (# t#, res1, res2 #) -> (# t#, (# res1, res2 #) #)

-- | Create a new mutable byte array of specified size (in bytes), in
-- the specified state thread.
newByteArray#
  :: Int# -- ^ size (in bytes)
  -> StateS# s ( MutableByteArray# s )
newByteArray# arg1 =
  StateS# ( GHC.Exts.newByteArray# arg1 )

-- | Create a mutable byte array that the GC guarantees not to move.
newPinnedByteArray#
  :: Int# -- ^ size (in bytes)
  -> StateS# s ( MutableByteArray# s )
newPinnedByteArray# arg1 =
  StateS# ( GHC.Exts.newPinnedByteArray# arg1 )

-- | Create a mutable byte array, aligned by the specified amount, that the GC guarantees not to move.
newAlignedPinnedByteArray#
  :: Int# -- ^ size (in bytes)
  -> Int# -- ^ alignment
  -> StateS# s ( MutableByteArray# s )
newAlignedPinnedByteArray# arg1 arg2 =
  StateS# ( GHC.Exts.newAlignedPinnedByteArray# arg1 arg2 )

-- | Shrink mutable byte array to new specified size (in bytes), in the specified state thread.
-- The new size argument must be less than or equal to the current size as reported by sizeofMutableByteArray#.
shrinkMutableByteArray#
  :: MutableByteArray# s
  -> Int# -- ^ size (in bytes) to shrink to
  -> StateS# s (##)
shrinkMutableByteArray# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.shrinkMutableByteArray# arg1 arg2 s# of
      t# -> (# t#, (##) #)

-- | Resize (unpinned) mutable byte array to new specified size (in bytes).
-- The returned 'MutableByteArray#'
resizeMutableByteArray#
  :: MutableByteArray# s
  -> Int# -- ^ new size (in bytes)
  -> StateS# s ( MutableByteArray# s )
resizeMutableByteArray# arg1 arg2 =
  StateS# ( GHC.Exts.resizeMutableByteArray# arg1 arg2 )

-- | Make a mutable byte array immutable, without copying.
unsafeFreezeByteArray#
  :: MutableByteArray# s
  -> StateS# s ByteArray#
unsafeFreezeByteArray# arg1 =
  StateS# ( GHC.Exts.unsafeFreezeByteArray# arg1 )

-- | Return the number of elements in the array.
getSizeofMutableByteArray#
  :: MutableByteArray# s
  -> StateS# s Int#
getSizeofMutableByteArray# arg1 =
  StateS# ( GHC.Exts.getSizeofMutableByteArray# arg1 )

-- | Read 8-bit character; offset in bytes.
readCharArray#
  :: MutableByteArray# s
  -> Int# -- ^ byte offset
  -> StateS# s Char#
readCharArray# arg1 arg2 =
  StateS# ( GHC.Exts.readCharArray# arg1 arg2 )

-- | Read 31-bit character; offset in 4-byte words.
readWideCharArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in units of 4 bytes)
  -> StateS# s Char#
readWideCharArray# arg1 arg2 =
  StateS# ( GHC.Exts.readWideCharArray# arg1 arg2 )

-- | Read integer; offset in machine words.
readIntArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Int#
readIntArray# arg1 arg2 =
  StateS# ( GHC.Exts.readIntArray# arg1 arg2 )

-- | Read word; offset in machine words.
readWordArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Word#
readWordArray# arg1 arg2 =
  StateS# ( GHC.Exts.readWordArray# arg1 arg2 )

readAddrArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Addr#
readAddrArray# arg1 arg2 =
  StateS# ( GHC.Exts.readAddrArray# arg1 arg2 )

readFloatArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Float#
readFloatArray# arg1 arg2 =
  StateS# ( GHC.Exts.readFloatArray# arg1 arg2 )

readDoubleArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Double#
readDoubleArray# arg1 arg2 =
  StateS# ( GHC.Exts.readDoubleArray# arg1 arg2 )

readStablePtrArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s ( StablePtr# a )
readStablePtrArray# arg1 arg2 =
  StateS# ( GHC.Exts.readStablePtrArray# arg1 arg2 )

readInt8Array#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Int#
readInt8Array# arg1 arg2 =
  StateS# ( GHC.Exts.readInt8Array# arg1 arg2 )

readInt16Array#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Int#
readInt16Array# arg1 arg2 =
  StateS# ( GHC.Exts.readInt16Array# arg1 arg2 )

readInt32Array#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s INT32
readInt32Array# arg1 arg2 =
  StateS# ( GHC.Exts.readInt32Array# arg1 arg2 )

readInt64Array#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s INT64
readInt64Array# arg1 arg2 =
  StateS# ( GHC.Exts.readInt64Array# arg1 arg2 )

readWord8Array#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Word#
readWord8Array# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8Array# arg1 arg2 )

readWord16Array#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Word#
readWord16Array# arg1 arg2 =
  StateS# ( GHC.Exts.readWord16Array# arg1 arg2 )

readWord32Array#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s WORD32
readWord32Array# arg1 arg2 =
  StateS# ( GHC.Exts.readWord32Array# arg1 arg2 )

readWord64Array#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s WORD64
readWord64Array# arg1 arg2 =
  StateS# ( GHC.Exts.readWord64Array# arg1 arg2 )

readWord8ArrayAsChar#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Char#
readWord8ArrayAsChar# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsChar# arg1 arg2 )

readWord8ArrayAsWideChar#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Char#
readWord8ArrayAsWideChar# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsWideChar# arg1 arg2 )

readWord8ArrayAsAddr#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Addr#
readWord8ArrayAsAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsAddr# arg1 arg2 )

readWord8ArrayAsFloat#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Float#
readWord8ArrayAsFloat# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsFloat# arg1 arg2 )

readWord8ArrayAsDouble#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Double#
readWord8ArrayAsDouble# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsDouble# arg1 arg2 )

readWord8ArrayAsStablePtr#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s ( StablePtr# a )
readWord8ArrayAsStablePtr# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsStablePtr# arg1 arg2 )

readWord8ArrayAsInt16#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Int#
readWord8ArrayAsInt16# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsInt16# arg1 arg2 )

readWord8ArrayAsInt32#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s INT32
readWord8ArrayAsInt32# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsInt32# arg1 arg2 )

readWord8ArrayAsInt64#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s INT64
readWord8ArrayAsInt64# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsInt64# arg1 arg2 )

readWord8ArrayAsInt#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Int#
readWord8ArrayAsInt# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsInt# arg1 arg2 )

readWord8ArrayAsWord16#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Word#
readWord8ArrayAsWord16# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsWord16# arg1 arg2 )

readWord8ArrayAsWord32#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s WORD32
readWord8ArrayAsWord32# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsWord32# arg1 arg2 )

readWord8ArrayAsWord64#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s WORD64
readWord8ArrayAsWord64# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsWord64# arg1 arg2 )

readWord8ArrayAsWord#
  :: MutableByteArray# s
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Word#
readWord8ArrayAsWord# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8ArrayAsWord# arg1 arg2 )

-- | Write 8-bit character; offset in bytes.
writeCharArray#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Char# -- ^ value to write
  -> StateS# s (##)
writeCharArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeCharArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

-- | Write 31-bit character; offset in 4-byte words.
writeWideCharArray#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Char# -- ^ value to write
  -> StateS# s (##)
writeWideCharArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWideCharArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeIntArray#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Int# -- ^ value to write
  -> StateS# s (##)
writeIntArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeIntArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWordArray#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Word# -- ^ value to write
  -> StateS# s (##)
writeWordArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWordArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeAddrArray#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Addr# -- ^ value to write
  -> StateS# s (##)
writeAddrArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeAddrArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeFloatArray#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Float# -- ^ value to write
  -> StateS# s (##)
writeFloatArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeFloatArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeDoubleArray#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Double# -- ^ value to write
  -> StateS# s (##)
writeDoubleArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeDoubleArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeStablePtrArray#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> StablePtr# a -- ^ value to write
  -> StateS# s (##)
writeStablePtrArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeStablePtrArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeInt8Array#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Int# -- ^ value to write
  -> StateS# s (##)
writeInt8Array# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeInt8Array# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeInt16Array#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Int# -- ^ value to write
  -> StateS# s (##)
writeInt16Array# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeInt16Array# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeInt32Array#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> INT32 -- ^ value to write
  -> StateS# s (##)
writeInt32Array# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeInt32Array# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeInt64Array#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> INT64 -- ^ value to write
  -> StateS# s (##)
writeInt64Array# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeInt64Array# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8Array#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Word# -- ^ value to write
  -> StateS# s (##)
writeWord8Array# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8Array# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord16Array#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Word# -- ^ value to write
  -> StateS# s (##)
writeWord16Array# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord16Array# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord32Array#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> WORD32 -- ^ value to write
  -> StateS# s (##)
writeWord32Array# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord32Array# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord64Array#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> WORD64 -- ^ value to write
  -> StateS# s (##)
writeWord64Array# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord64Array# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsChar#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Char# -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsChar# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsChar# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsWideChar#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Char# -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsWideChar# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsWideChar# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsAddr#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Addr# -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsFloat#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Float# -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsFloat# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsFloat# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsDouble#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Double# -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsDouble# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsDouble# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsStablePtr#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> StablePtr# a -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsStablePtr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsStablePtr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsInt16#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Int# -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsInt16# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsInt16# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsInt32#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> INT32 -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsInt32# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsInt32# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsInt64#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> INT64 -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsInt64# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsInt64# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsInt#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Int# -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsInt# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsInt# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsWord16#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Word# -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsWord16# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsWord16# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsWord32#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> WORD32 -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsWord32# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsWord32# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsWord64#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> WORD64 -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsWord64# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsWord64# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8ArrayAsWord#
  :: MutableByteArray# s
  -> Int# -- ^ index
  -> Word# -- ^ value to write
  -> StateS# s (##)
writeWord8ArrayAsWord# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8ArrayAsWord# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

-- | @ copyByteArray# src src_ofs dst dst_ofs n@
copyByteArray#
  :: ByteArray# -- ^ src
  -> Int# -- ^ src offset
  -> MutableByteArray# s -- ^ dst
  -> Int# -- ^ dst offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copyByteArray# arg1 arg2 arg3 arg4 arg5 =
  StateS# \ s# ->
    case GHC.Exts.copyByteArray# arg1 arg2 arg3 arg4 arg5 s# of
      t# -> (# t#, (##) #)

-- | Copy a range of the first MutableByteArray\# to the specified region in the second MutableByteArray\#.
-- Both arrays must fully contain the specified ranges, but this is not checked. The regions are
-- allowed to overlap, although this is only possible when the same array is provided
-- as both the source and the destination.
copyMutableByteArray#
  :: MutableByteArray# s -- ^ src
  -> Int# -- ^ src offset
  -> MutableByteArray# s -- ^ dst
  -> Int# -- ^ dst offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copyMutableByteArray# arg1 arg2 arg3 arg4 arg5 =
  StateS# \ s# ->
    case GHC.Exts.copyMutableByteArray# arg1 arg2 arg3 arg4 arg5 s# of
      t# -> (# t#, (##) #)

-- | Copy a range of the ByteArray\# to the memory range starting at the Addr\#.
-- The ByteArray\# and the memory region at Addr\# must fully contain the
-- specified ranges, but this is not checked. The Addr\# must not point into the
-- ByteArray\# (e.g. if the ByteArray\# were pinned), but this is not checked
-- either.
copyByteArrayToAddr#
  :: ByteArray# -- ^ src
  -> Int# -- ^ src offset
  -> Addr# -- ^ dst
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copyByteArrayToAddr# arg1 arg2 arg3 arg4 =
  StateS# \ s# ->
    case GHC.Exts.copyByteArrayToAddr# arg1 arg2 arg3 arg4 s# of
      t# -> (# t#, (##) #)

-- | Copy a range of the MutableByteArray\# to the memory range starting at the
-- Addr\#. The MutableByteArray\# and the memory region at Addr\# must fully
-- contain the specified ranges, but this is not checked. The Addr\# must not
-- point into the MutableByteArray\# (e.g. if the MutableByteArray\# were
-- pinned), but this is not checked either.
copyMutableByteArrayToAddr#
  :: MutableByteArray# s -- ^ src
  -> Int# -- ^ src offset
  -> Addr# -- ^ dst
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copyMutableByteArrayToAddr# arg1 arg2 arg3 arg4 =
  StateS# \ s# ->
    case GHC.Exts.copyMutableByteArrayToAddr# arg1 arg2 arg3 arg4 s# of
      t# -> (# t#, (##) #)

-- | Copy a memory range starting at the Addr\# to the specified range in the
-- MutableByteArray\#. The memory region at Addr\# and the ByteArray\# must fully
-- contain the specified ranges, but this is not checked. The Addr\# must not
-- point into the MutableByteArray\# (e.g. if the MutableByteArray\# were pinned),
-- but this is not checked either.
copyAddrToByteArray#
  :: Addr# -- ^ src
  -> MutableByteArray# s -- ^ dst
  -> Int# -- ^ dst offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copyAddrToByteArray# arg1 arg2 arg3 arg4 =
  StateS# \ s# ->
    case GHC.Exts.copyAddrToByteArray# arg1 arg2 arg3 arg4 s# of
      t# -> (# t#, (##) #)

-- | @ setByteArray# ba off len c@
setByteArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset
  -> Int# -- ^ number of elements to set
  -> Int# -- ^ value to set
  -> StateS# s (##)
setByteArray# arg1 arg2 arg3 arg4 =
  StateS# \ s# ->
    case GHC.Exts.setByteArray# arg1 arg2 arg3 arg4 s# of
      t# -> (# t#, (##) #)

-- | Given an array and an offset in machine words, read an element. The
-- index is assumed to be in bounds. Implies a full memory barrier.
atomicReadIntArray#
  :: MutableByteArray# s
  -> Int#
  -> StateS# s Int#
atomicReadIntArray# arg1 arg2 =
  StateS# ( GHC.Exts.atomicReadIntArray# arg1 arg2 )

-- | Given an array and an offset in machine words, write an element. The
-- index is assumed to be in bounds. Implies a full memory barrier.
atomicWriteIntArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset
  -> Int# -- ^ value to write
  -> StateS# s (##)
atomicWriteIntArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.atomicWriteIntArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

-- | Given an array, an offset in machine words, the expected old value, and
-- the new value, perform an atomic compare and swap i.e. write the new
-- value if the current value matches the provided old value. Returns
-- the value of the element before the operation. Implies a full memory
-- barrier.
casIntArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset
  -> Int# -- ^ expected old value
  -> Int# -- ^ value to (potentially) write
  -> StateS# s Int#
casIntArray# arg1 arg2 arg3 arg4 =
  StateS# ( GHC.Exts.casIntArray# arg1 arg2 arg3 arg4 )

-- | Given an array, and offset in machine words, and a value to add,
-- atomically add the value to the element. Returns the value of the
-- element before the operation. Implies a full memory barrier.
fetchAddIntArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset
  -> Int# -- ^ value to add
  -> StateS# s Int#
fetchAddIntArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.fetchAddIntArray# arg1 arg2 arg3 )

-- | Given an array, and offset in machine words, and a value to subtract,
-- atomically subtract the value to the element. Returns the value of
-- the element before the operation. Implies a full memory barrier.
fetchSubIntArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset
  -> Int# -- ^ value to subtract
  -> StateS# s Int#
fetchSubIntArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.fetchSubIntArray# arg1 arg2 arg3 )

-- | Given an array, and offset in machine words, and a value to AND,
-- atomically AND the value to the element. Returns the value of the
-- element before the operation. Implies a full memory barrier.
fetchAndIntArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset
  -> Int# -- ^ value to AND
  -> StateS# s Int#
fetchAndIntArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.fetchAndIntArray# arg1 arg2 arg3 )

-- | Given an array, and offset in machine words, and a value to NAND,
-- atomically NAND the value to the element. Returns the value of the
-- element before the operation. Implies a full memory barrier.
fetchNandIntArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset
  -> Int# -- ^ value to NAND
  -> StateS# s Int#
fetchNandIntArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.fetchNandIntArray# arg1 arg2 arg3 )

-- | Given an array, and offset in machine words, and a value to OR,
-- atomically OR the value to the element. Returns the value of the
-- element before the operation. Implies a full memory barrier.
fetchOrIntArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset
  -> Int# -- ^ value to OR
  -> StateS# s Int#
fetchOrIntArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.fetchOrIntArray# arg1 arg2 arg3 )

-- | Given an array, and offset in machine words, and a value to XOR,
-- atomically XOR the value to the element. Returns the value of the
-- element before the operation. Implies a full memory barrier.
fetchXorIntArray#
  :: MutableByteArray# s
  -> Int# -- ^ offset
  -> Int# -- ^ value to XOR
  -> StateS# s Int#
fetchXorIntArray# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.fetchXorIntArray# arg1 arg2 arg3 )

-- | Create a new mutable array of arrays with the specified number of elements,
-- in the specified state thread, with each element recursively referring to the
-- newly created array.
newArrayArray#
  :: Int#
  -> StateS# s ( MutableArrayArray# s )
newArrayArray# arg1 =
  StateS# ( GHC.Exts.newArrayArray# arg1 )

readByteArrayArray#
  :: MutableArrayArray# s
  -> Int#
  -> StateS# s ByteArray#
readByteArrayArray# arg1 arg2 =
  StateS# ( GHC.Exts.readByteArrayArray# arg1 arg2 )

readMutableByteArrayArray#
  :: MutableArrayArray# s
  -> Int#
  -> StateS# s ( MutableByteArray# s )
readMutableByteArrayArray# arg1 arg2 =
  StateS# ( GHC.Exts.readMutableByteArrayArray# arg1 arg2 )

readArrayArrayArray#
  :: MutableArrayArray# s
  -> Int#
  -> StateS# s ArrayArray#
readArrayArrayArray# arg1 arg2 =
  StateS# ( GHC.Exts.readArrayArrayArray# arg1 arg2 )

readMutableArrayArrayArray#
  :: MutableArrayArray# s
  -> Int#
  -> StateS# s ( MutableArrayArray# s )
readMutableArrayArrayArray# arg1 arg2 =
  StateS# ( GHC.Exts.readMutableArrayArrayArray# arg1 arg2 )

writeByteArrayArray#
  :: MutableArrayArray# s
  -> Int#
  -> ByteArray#
  -> StateS# s (##)
writeByteArrayArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeByteArrayArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeMutableByteArrayArray#
  :: MutableArrayArray# s
  -> Int#
  -> MutableByteArray# s
  -> StateS# s (##)
writeMutableByteArrayArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeMutableByteArrayArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeArrayArrayArray#
  :: MutableArrayArray# s
  -> Int#
  -> ArrayArray#
  -> StateS# s (##)
writeArrayArrayArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeArrayArrayArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeMutableArrayArrayArray#
  :: MutableArrayArray# s
  -> Int#
  -> MutableArrayArray# s
  -> StateS# s (##)
writeMutableArrayArrayArray# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeMutableArrayArrayArray# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

-- | Copy a range of the ArrayArray\# to the specified region in the MutableArrayArray\#.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The two arrays must not be the same array in different states, but this is not checked either.
copyArrayArray#
  :: ArrayArray# -- ^ src
  -> Int# -- ^ src offset
  -> MutableArrayArray# s -- ^ dst
  -> Int# -- ^ dst offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copyArrayArray# arg1 arg2 arg3 arg4 arg5 =
  StateS# \ s# ->
    case GHC.Exts.copyArrayArray# arg1 arg2 arg3 arg4 arg5 s# of
      t# -> (# t#, (##) #)

-- | Copy a range of the first MutableArrayArray# to the specified region in the second
-- MutableArrayArray#.
-- Both arrays must fully contain the specified ranges, but this is not checked.
-- The regions are allowed to overlap, although this is only possible when the same
-- array is provided as both the source and the destination.
-- 
copyMutableArrayArray#
  :: MutableArrayArray# s -- ^ src
  -> Int# -- ^ src offset
  -> MutableArrayArray# s -- ^ dst
  -> Int# -- ^ dst offset
  -> Int# -- ^ number of elements to copy
  -> StateS# s (##)
copyMutableArrayArray# arg1 arg2 arg3 arg4 arg5 =
  StateS# \ s# ->
    case GHC.Exts.copyMutableArrayArray# arg1 arg2 arg3 arg4 arg5 s# of
      t# -> (# t#, (##) #)

-- | Reads 8-bit character; offset in bytes.
readCharOffAddr#
  :: Addr#
  -> Int# -- ^ byte offset
  -> StateS# s Char#
readCharOffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readCharOffAddr# arg1 arg2 )

-- | Reads 31-bit character; offset in 4-byte words.
readWideCharOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in units of 4 bytes)
  -> StateS# s Char#
readWideCharOffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readWideCharOffAddr# arg1 arg2 )

readIntOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Int#
readIntOffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readIntOffAddr# arg1 arg2 )

readWordOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Word#
readWordOffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readWordOffAddr# arg1 arg2 )

readAddrOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Addr#
readAddrOffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readAddrOffAddr# arg1 arg2 )

readFloatOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Float#
readFloatOffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readFloatOffAddr# arg1 arg2 )

readDoubleOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Double#
readDoubleOffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readDoubleOffAddr# arg1 arg2 )

readStablePtrOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s ( StablePtr# a )
readStablePtrOffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readStablePtrOffAddr# arg1 arg2 )

readInt8OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Int#
readInt8OffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readInt8OffAddr# arg1 arg2 )

readInt16OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Int#
readInt16OffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readInt16OffAddr# arg1 arg2 )

readInt32OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s INT32
readInt32OffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readInt32OffAddr# arg1 arg2 )

readInt64OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s INT64
readInt64OffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readInt64OffAddr# arg1 arg2 )

readWord8OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Word#
readWord8OffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readWord8OffAddr# arg1 arg2 )

readWord16OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s Word#
readWord16OffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readWord16OffAddr# arg1 arg2 )

readWord32OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s WORD32
readWord32OffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readWord32OffAddr# arg1 arg2 )

readWord64OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StateS# s WORD64
readWord64OffAddr# arg1 arg2 =
  StateS# ( GHC.Exts.readWord64OffAddr# arg1 arg2 )

writeCharOffAddr#
  :: Addr#
  -> Int#  -- ^ offset (in machine words)
  -> Char# -- ^ value to write
  -> StateS# s (##)
writeCharOffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeCharOffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWideCharOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> Char# -- ^ value to write
  -> StateS# s (##)
writeWideCharOffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWideCharOffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeIntOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> Int# -- ^ value to write
  -> StateS# s (##)
writeIntOffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeIntOffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWordOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> Word# -- ^ value to write
  -> StateS# s (##)
writeWordOffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWordOffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeAddrOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> Addr# -- ^ value to write
  -> StateS# s (##)
writeAddrOffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeAddrOffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeFloatOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> Float# -- ^ value to write
  -> StateS# s (##)
writeFloatOffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeFloatOffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeDoubleOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> Double# -- ^ value to write
  -> StateS# s (##)
writeDoubleOffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeDoubleOffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeStablePtrOffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> StablePtr# a -- ^ value to write
  -> StateS# s (##)
writeStablePtrOffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeStablePtrOffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeInt8OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> Int# -- ^ value to write
  -> StateS# s (##)
writeInt8OffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeInt8OffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeInt16OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> Int# -- ^ value to write
  -> StateS# s (##)
writeInt16OffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeInt16OffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeInt32OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> INT32 -- ^ value to write
  -> StateS# s (##)
writeInt32OffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeInt32OffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeInt64OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> INT64 -- ^ value to write
  -> StateS# s (##)
writeInt64OffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeInt64OffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord8OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> Word# -- ^ value to write
  -> StateS# s (##)
writeWord8OffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord8OffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord16OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> Word# -- ^ value to write
  -> StateS# s (##)
writeWord16OffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord16OffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord32OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> WORD32 -- ^ value to write
  -> StateS# s (##)
writeWord32OffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord32OffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

writeWord64OffAddr#
  :: Addr#
  -> Int# -- ^ offset (in machine words)
  -> WORD64 -- ^ value to write
  -> StateS# s (##)
writeWord64OffAddr# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.writeWord64OffAddr# arg1 arg2 arg3 s# of
      t# -> (# t#, (##) #)

-- | The atomic exchange operation. Atomically exchanges the value at the first address
-- with the Addr# given as second argument. Implies a read barrier.
atomicExchangeAddrAddr#
  :: Addr#
  -> Addr#
  -> StateS# s Addr#
atomicExchangeAddrAddr# arg1 arg2 =
  StateS# ( GHC.Exts.atomicExchangeAddrAddr# arg1 arg2 )

-- | The atomic exchange operation. Atomically exchanges the value at the address
-- with the given value. Returns the old value. Implies a read barrier.
atomicExchangeWordAddr#
  :: Addr#
  -> Word#
  -> StateS# s Word#
atomicExchangeWordAddr# arg1 arg2 =
  StateS# ( GHC.Exts.atomicExchangeWordAddr# arg1 arg2 )

-- | Compare and swap on a word-sized memory location.
-- 
-- Use as: \s -> atomicCasAddrAddr# location expected desired s
-- 
-- This version always returns the old value read. This follows the normal
-- protocol for CAS operations (and matches the underlying instruction on
-- most architectures).
-- 
-- Implies a full memory barrier.
atomicCasAddrAddr#
  :: Addr# -- ^ address at which to perform the operation
  -> Addr# -- ^ expected old value
  -> Addr# -- ^ new value
  -> StateS# s Addr#
atomicCasAddrAddr# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.atomicCasAddrAddr# arg1 arg2 arg3 )

-- | Compare and swap on a word-sized and aligned memory location.
-- 
-- Use as: \s -> atomicCasWordAddr# location expected desired s
-- 
-- This version always returns the old value read. This follows the normal
-- protocol for CAS operations (and matches the underlying instruction on
-- most architectures).
-- 
-- Implies a full memory barrier.
atomicCasWordAddr#
  :: Addr# -- ^ address at which to perform the operation
  -> Word# -- ^ expected old value
  -> Word# -- ^ new value
  -> StateS# s Word#
atomicCasWordAddr# arg1 arg2 arg3 =
  StateS# ( GHC.Exts.atomicCasWordAddr# arg1 arg2 arg3 )

-- | Create 'MutVar#' with specified initial value.
newMutVar#
  :: a
  -> StateS# s ( MutVar# s a )
newMutVar# arg1 =
  StateS# ( GHC.Exts.newMutVar# arg1 )

-- | Read contents of 'MutVar#'. Result is not yet evaluated.
readMutVar#
  :: MutVar# s a
  -> StateS# s a
readMutVar# arg1 =
  StateS# ( GHC.Exts.readMutVar# arg1 )

-- | Write contents of 'MutVar#'.
writeMutVar#
  :: MutVar# s a
  -> a
  -> StateS# s (##)
writeMutVar# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.writeMutVar# arg1 arg2 s# of
      t# -> (# t#, (##) #)

-- | Modify the contents of a 'MutVar#', returning the previous
-- contents and the result of applying the given function to the
-- previous contents. Note that this isn't strictly
-- speaking the correct type for this function; it should really be
--   @
--     MutVar# s a -> (a -> (a,b)) -> State# s -> (# State# s, a, (a, b) #)
--   @
-- but we don't know about pairs here.
atomicModifyMutVar2#
  :: MutVar# s a
  -> ( a -> c )
  -> StateS# s (# a, c #)
atomicModifyMutVar2# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.atomicModifyMutVar2# arg1 arg2 s# of
      (# t#, res1, res2 #) -> (# t#, (# res1, res2 #) #)

-- | Modify the contents of a 'MutVar#', returning the previous
-- contents and the result of applying the given function to the
-- previous contents.
atomicModifyMutVar_#
  :: MutVar# s a
  -> ( a -> a )
  -> StateS# s (# a, a #)
atomicModifyMutVar_# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.atomicModifyMutVar_# arg1 arg2 s# of
      (# t#, res1, res2 #) -> (# t#, (# res1, res2 #) #)

casMutVar#
  :: MutVar# s a
  -> a
  -> a
  -> StateS# s (# Int#, a #)
casMutVar# arg1 arg2 arg3 =
  StateS# \ s# ->
    case GHC.Exts.casMutVar# arg1 arg2 arg3 s# of
      (# t#, res1, res2 #) -> (# t#, (# res1, res2 #) #)

-- | Create a new 'TVar#' holding a specified initial value.
newTVar#
  :: a
  -> StateS# s ( TVar# s a )
newTVar# arg1 =
  StateS# ( GHC.Exts.newTVar# arg1 )

-- | Read contents of 'TVar#' outside an STM transaction.
readTVar#
  :: TVar# s a
  -> StateS# s a
readTVar# arg1 =
  StateS# ( GHC.Exts.readTVar# arg1 )

-- | Read contents of 'TVar#'.
readTVarIO#
  :: TVar# s a
  -> StateS# s a
readTVarIO# arg1 =
  StateS# ( GHC.Exts.readTVarIO# arg1 )

-- | Write contents of 'TVar#'.
writeTVar#
  :: TVar# s a
  -> a
  -> StateS# s (##)
writeTVar# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.writeTVar# arg1 arg2 s# of
      t# -> (# t#, (##) #)

-- | Create new 'MVar#'; initially empty.
newMVar#
  :: StateS# s ( MVar# s a )
newMVar# =
  StateS# GHC.Exts.newMVar#

-- |If 'MVar#' is empty, block until it becomes full.
-- Then remove and return its contents, and set it empty.
takeMVar#
  :: MVar# s a
  -> StateS# s a
takeMVar# arg1 =
  StateS# ( GHC.Exts.takeMVar# arg1 )

-- | If 'MVar#' is empty, immediately return with integer 0 and value undefined.
-- Otherwise, return with integer 1 and contents of 'MVar#', and set 'MVar#' empty.
tryTakeMVar#
  :: MVar# s a
  -> StateS# s (# Int#, a #)
tryTakeMVar# arg1 =
  StateS# \ s# ->
    case GHC.Exts.tryTakeMVar# arg1 s# of
      (# t#, res1, res2 #) -> (# t#, (# res1, res2 #) #)

-- | If 'MVar#' is full, block until it becomes empty.
-- Then store value arg as its new contents.
putMVar#
  :: MVar# s a
  -> a
  -> StateS# s (##)
putMVar# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.putMVar# arg1 arg2 s# of
      t# -> (# t#, (##) #)

-- | If 'MVar#' is full, immediately return with integer 0.
-- Otherwise, store value arg as 'MVar#''s new contents, and return with integer 1.
tryPutMVar#
  :: MVar# s a
  -> a
  -> StateS# s Int#
tryPutMVar# arg1 arg2 =
  StateS# ( GHC.Exts.tryPutMVar# arg1 arg2 )

-- | If 'MVar#' is empty, block until it becomes full.
-- Then read its contents without modifying the MVar, without possibility
readMVar#
  :: MVar# s a
  -> StateS# s a
readMVar# arg1 =
  StateS# ( GHC.Exts.readMVar# arg1 )

-- | If 'MVar#' is empty, immediately return with integer 0 and value undefined.
-- Otherwise, return with integer 1 and contents of 'MVar#'.
tryReadMVar#
  :: MVar# s a
  -> StateS# s (# Int#, a #)
tryReadMVar# arg1 =
  StateS# \ s# ->
    case GHC.Exts.tryReadMVar# arg1 s# of
      (# t#, res1, res2 #) -> (# t#, (# res1, res2 #) #)

-- | Create new 'IOPort#'; initially empty.
newIOPort#
  :: StateS# s ( IOPort# s a )
newIOPort# =
  StateS# GHC.Exts.newIOPort#

-- | If 'IOPort#' is empty, block until it becomes full.
-- Then remove and return its contents, and set it empty.
readIOPort#
  :: IOPort# s a
  -> StateS# s a
readIOPort# arg1 =
  StateS# ( GHC.Exts.readIOPort# arg1 )

-- | If 'IOPort#' is full, immediately return with integer 0.
-- Otherwise, store value arg as 'IOPort#''s new contents,
-- and return with integer 1.
writeIOPort#
  :: IOPort# s a
  -> a
  -> StateS# s Int#
writeIOPort# arg1 arg2 =
  StateS# ( GHC.Exts.writeIOPort# arg1 arg2 )

-- | Sleep specified number of microseconds.
delay#
  :: Int# -- ^ µs
  -> StateS# s (##)
delay# arg1 =
  StateS# \ s# ->
    case GHC.Exts.delay# arg1 s# of
      t# -> (# t#, (##) #)

-- | Block until input is available on specified file descriptor.
waitRead#
  :: Int# -- ^ µs
  -> StateS# s (##)
waitRead# arg1 =
  StateS# \ s# ->
    case GHC.Exts.waitRead# arg1 s# of
      t# -> (# t#, (##) #)

-- | Block until output is possible on specified file descriptor.
waitWrite#
  :: Int# -- ^ µs
  -> StateS# s (##)
waitWrite# arg1 =
  StateS# \ s# ->
    case GHC.Exts.waitWrite# arg1 s# of
      t# -> (# t#, (##) #)

noDuplicate#
  :: StateS# s (##)
noDuplicate# =
  StateS# \ s# ->
    case GHC.Exts.noDuplicate# s# of
      t# -> (# t#, (##) #)

spark#
  :: a
  -> StateS# s a
spark# arg1 =
  StateS# ( GHC.Exts.spark# arg1 )

seq#
  :: a
  -> StateS# s a
seq# arg1 =
  StateS# ( GHC.Exts.seq# arg1 )

getSpark#
  :: StateS# s (# Int#, a #)
getSpark# =
  StateS# \ s# ->
    case GHC.Exts.getSpark# s# of
      (# t#, res1, res2 #) -> (# t#, (# res1, res2 #) #)

-- | Returns the number of sparks in the local spark pool.
numSparks#
  :: StateS# s Int#
numSparks# =
  StateS# GHC.Exts.numSparks#

-- | @NewBCO# instrs lits ptrs arity bitmap@ creates a new bytecode object. The
-- resulting object encodes a function of the given arity with the instructions
-- encoded in @instrs@, and a static reference table usage bitmap given by
-- @bitmap@.
newBCO#
  :: ByteArray# -- ^ instructions
  -> ByteArray# -- ^ its
  -> Array# a -- ^ ptrs
  -> Int# -- ^ arity
  -> ByteArray# -- ^ bitmap
  -> StateS# s BCO
newBCO# arg1 arg2 arg3 arg4 arg5 =
  StateS# ( GHC.Exts.newBCO# arg1 arg2 arg3 arg4 arg5 )

getCCSOf#
  :: a
  -> StateS# s Addr#
getCCSOf# arg1 =
  StateS# ( GHC.Exts.getCCSOf# arg1 )

-- | Returns the current @CostCentreStack@ (value is @NULL@ if
-- not profiling).  Takes a dummy argument which can be used to
-- avoid the call to 'getCurrentCCS#' being floated out by the
-- simplifier, which would result in an uninformative stack
-- ("CAF").
getCurrentCCS#
  :: a
  -> StateS# s Addr#
getCurrentCCS# arg1 =
  StateS# ( GHC.Exts.getCurrentCCS# arg1 )

-- | Run the supplied IO action with an empty CCS.  For example, this
-- is used by the interpreter to run an interpreted computation
-- without the call stack showing that it was invoked from GHC.
clearCCS#
  :: ( State# s -> (# State# s, a #) )
  -> StateS# s a
clearCCS# arg1 =
  StateS# ( GHC.Exts.clearCCS# arg1 )

-- | Emits an event via the RTS tracing framework.  The contents
-- of the event is the zero-terminated byte string passed as the first
-- argument.  The event will be emitted either to the .eventlog file,
-- or to stderr, depending on the runtime RTS flags.
traceEvent#
  :: Addr#
  -> StateS# s (##)
traceEvent# arg1 =
  StateS# \ s# ->
    case GHC.Exts.traceEvent# arg1 s# of
      t# -> (# t#, (##) #)

-- | Emits an event via the RTS tracing framework.  The contents
-- of the event is the binary object passed as the first argument with
-- the given length passed as the second argument. The event will be
-- emitted to the .eventlog file.
traceBinaryEvent#
  :: Addr#
  -> Int#
  -> StateS# s (##)
traceBinaryEvent# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.traceBinaryEvent# arg1 arg2 s# of
      t# -> (# t#, (##) #)

-- | Emits a marker event via the RTS tracing framework.  The contents
-- of the event is the zero-terminated byte string passed as the first
-- argument.  The event will be emitted either to the .eventlog file,
-- or to stderr, depending on the runtime RTS flags.
traceMarker#
  :: Addr#
  -> StateS# s (##)
traceMarker# arg1 =
  StateS# \ s# ->
    case GHC.Exts.traceMarker# arg1 s# of
      t# -> (# t#, (##) #)

prefetchByteArray3#
  :: ByteArray#
  -> Int#
  -> StateS# s (##)
prefetchByteArray3# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchByteArray3# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchMutableByteArray3#
  :: MutableByteArray# s
  -> Int#
  -> StateS# s (##)
prefetchMutableByteArray3# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchMutableByteArray3# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchAddr3#
  :: Addr#
  -> Int#
  -> StateS# s (##)
prefetchAddr3# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchAddr3# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchValue3#
  :: a
  -> StateS# s (##)
prefetchValue3# arg1 =
  StateS# \ s# ->
    case GHC.Exts.prefetchValue3# arg1 s# of
      t# -> (# t#, (##) #)

prefetchByteArray2#
  :: ByteArray#
  -> Int#
  -> StateS# s (##)
prefetchByteArray2# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchByteArray2# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchMutableByteArray2#
  :: MutableByteArray# s
  -> Int#
  -> StateS# s (##)
prefetchMutableByteArray2# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchMutableByteArray2# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchAddr2#
  :: Addr#
  -> Int#
  -> StateS# s (##)
prefetchAddr2# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchAddr2# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchValue2#
  :: a
  -> StateS# s (##)
prefetchValue2# arg1 =
  StateS# \ s# ->
    case GHC.Exts.prefetchValue2# arg1 s# of
      t# -> (# t#, (##) #)

prefetchByteArray1#
  :: ByteArray#
  -> Int#
  -> StateS# s (##)
prefetchByteArray1# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchByteArray1# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchMutableByteArray1#
  :: MutableByteArray# s
  -> Int#
  -> StateS# s (##)
prefetchMutableByteArray1# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchMutableByteArray1# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchAddr1#
  :: Addr#
  -> Int#
  -> StateS# s (##)
prefetchAddr1# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchAddr1# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchValue1#
  :: a
  -> StateS# s (##)
prefetchValue1# arg1 =
  StateS# \ s# ->
    case GHC.Exts.prefetchValue1# arg1 s# of
      t# -> (# t#, (##) #)

prefetchByteArray0#
  :: ByteArray#
  -> Int#
  -> StateS# s (##)
prefetchByteArray0# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchByteArray0# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchMutableByteArray0#
  :: MutableByteArray# s
  -> Int#
  -> StateS# s (##)
prefetchMutableByteArray0# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchMutableByteArray0# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchAddr0#
  :: Addr#
  -> Int#
  -> StateS# s (##)
prefetchAddr0# arg1 arg2 =
  StateS# \ s# ->
    case GHC.Exts.prefetchAddr0# arg1 arg2 s# of
      t# -> (# t#, (##) #)

prefetchValue0#
  :: a
  -> StateS# s (##)
prefetchValue0# arg1 =
  StateS# \ s# ->
    case GHC.Exts.prefetchValue0# arg1 s# of
      t# -> (# t#, (##) #)
