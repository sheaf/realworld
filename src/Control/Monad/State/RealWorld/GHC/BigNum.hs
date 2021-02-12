module Control.Monad.State.RealWorld.GHC.BigNum
  ( -- * "GHC.Num.BigNat" functions
    bigNatToAddrLE#
  , bigNatToAddrBE#
  , bigNatToAddr#
  , bigNatFromAddrLE#
  , bigNatFromAddrBE#
  , bigNatFromAddr#
  , bigNatToMutableByteArrayLE#
  , bigNatToMutableByteArrayBE#
  , bigNatToMutableByteArray#
  , bigNatFromByteArrayLE#
  , bigNatFromByteArrayBE#
  , bigNatFromByteArray#
  , module GHC.Num.BigNat

  -- * "GHC.Num.Integer" functions
  , integerToAddr#
  , integerFromAddr#
  , integerToMutableByteArray#
  , integerFromByteArray#
  , module GHC.Num.Integer

  -- * "GHC.Num.Natural" functions
  , naturalToAddr#
  , naturalFromAddr#
  , naturalToMutableByteArray#
  , naturalFromByteArray#
  , module GHC.Num.Natural

  -- * "GHC.Num.Primitives" functions
  , wordToAddrLE#
  , wordToAddrBE#
  , wordToAddr#
  , wordFromAddrLE#
  , wordFromAddrBE#
  , wordFromAddr#
  , wordWriteAddrLE#
  , wordWriteAddrBE#
  , wordToMutableByteArrayLE#
  , wordToMutableByteArrayBE#
  , wordToMutableByteArray#
  , wordWriteMutableByteArrayLE#
  , wordWriteMutableByteArrayBE#
  , module GHC.Num.Primitives

  -- * "GHC.Num.WordArray" functions
  , mwaSize#
  , mwaArrayCopy#
  , mwaShrink#
  , mwaSetSize#
  , mwaInitCopyShrink#
  , mwaTrimZeroes#
  , mwaClz
  , mwaClzAt
  , mwaInitArrayPlusWord
  , mwaWriteOrShrink
  , mwaWriteMostSignificant
  , mwaInitArrayBinOp
  , mwaWrite#
  , mwaFill#
  , mwaAddInplaceWord#
  , mwaSubInplaceWord#
  , mwaTrimCompare
  , mwaSubInplaceArray
  , mwaAddInplaceArray
  , mwaSubInplaceMutableArray
  , mwaSubInplaceArrayTrim
  , mwaReadOrZero
  , mwaRead#
  , module GHC.Num.WordArray
  )
  where

-- base
import GHC.Exts
  ( Addr#, ByteArray#, Int#, MutableByteArray#, Word# )

-- ghc-bignum
import qualified GHC.Num.BigNat as GHC.BigNum
import GHC.Num.BigNat hiding
  ( bigNatToAddrLE#
  , bigNatToAddrBE#
  , bigNatToAddr#
  , bigNatFromAddrLE#
  , bigNatFromAddrBE#
  , bigNatFromAddr#
  , bigNatToMutableByteArrayLE#
  , bigNatToMutableByteArrayBE#
  , bigNatToMutableByteArray#
  , bigNatFromByteArrayLE#
  , bigNatFromByteArrayBE#
  , bigNatFromByteArray#
  )
import qualified GHC.Num.Integer as GHC.BigNum
import GHC.Num.Integer hiding
  ( integerToAddr#
  , integerFromAddr#
  , integerToMutableByteArray#
  , integerFromByteArray#
  )
import qualified GHC.Num.Natural as GHC.BigNum
import GHC.Num.Natural hiding
  ( naturalToAddr#
  , naturalFromAddr#
  , naturalToMutableByteArray#
  , naturalFromByteArray#
  )
import qualified GHC.Num.Primitives as GHC.BigNum
import GHC.Num.Primitives hiding
  ( wordToAddrLE#
  , wordToAddrBE#
  , wordToAddr#
  , wordFromAddrLE#
  , wordFromAddrBE#
  , wordFromAddr#
  , wordWriteAddrLE#
  , wordWriteAddrBE#
  , wordToMutableByteArrayLE#
  , wordToMutableByteArrayBE#
  , wordToMutableByteArray#
  , wordWriteMutableByteArrayLE#
  , wordWriteMutableByteArrayBE#
  )
import qualified GHC.Num.WordArray as GHC.BigNum
import GHC.Num.WordArray hiding
  ( mwaSize#
  , mwaArrayCopy#
  , mwaShrink#
  , mwaSetSize#
  , mwaInitCopyShrink#
  , mwaTrimZeroes#
  , mwaClz
  , mwaClzAt
  , mwaInitArrayPlusWord
  , mwaWriteOrShrink
  , mwaWriteMostSignificant
  , mwaInitArrayBinOp
  , mwaWrite#
  , mwaFill#
  , mwaAddInplaceWord#
  , mwaSubInplaceWord#
  , mwaTrimCompare
  , mwaSubInplaceArray
  , mwaAddInplaceArray
  , mwaSubInplaceMutableArray
  , mwaSubInplaceArrayTrim
  , mwaReadOrZero
  , mwaRead#
  )

-- realworld
import Control.Monad.State.RealWorld
  ( StateS#(..) )
import Control.Monad.State.RealWorld.Instances
  ( )

--------------------------------------------------------------------------------
-- BigNat

-- | Write a BigNat in base-256 little-endian representation and return the
-- number of bytes written.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToAddrLE#
  :: BigNat# 
  -> Addr# 
  -> StateS# s Word#
bigNatToAddrLE# a addr =
  StateS# ( GHC.BigNum.bigNatToAddrLE# a addr )

-- | Write a BigNat in base-256 big-endian representation and return the
-- number of bytes written.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToAddrBE#
  :: BigNat# 
  -> Addr# 
  -> StateS# s Word#
bigNatToAddrBE# a addr =
  StateS# ( GHC.BigNum.bigNatToAddrBE# a addr )

-- | Write a BigNat in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToAddr#
  :: BigNat# 
  -> Addr# 
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Word#
bigNatToAddr# a addr be =
  StateS# ( GHC.BigNum.bigNatToAddr# a addr be )

-- | Read a BigNat in base-256 little-endian representation from an 'Addr#'.
--
-- Higher limbs equal to 0 are automatically trimed.
bigNatFromAddrLE#
  :: Word# -- ^ Number of bytes to read
  -> Addr# 
  -> StateS# s BigNat#
bigNatFromAddrLE# sz addr =
  StateS# ( GHC.BigNum.bigNatFromAddrLE# sz addr )

-- | Read a BigNat in base-256 big-endian representation from an 'Addr#'.
--
-- Null higher limbs are automatically trimed.
bigNatFromAddrBE#
  :: Word#  -- ^ Number of bytes to read
  -> Addr# 
  -> StateS# s BigNat#
bigNatFromAddrBE# sz addr =
  StateS# ( GHC.BigNum.bigNatFromAddrBE# sz addr )

-- | Read a BigNat in base-256 representation from an 'Addr#'.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
bigNatFromAddr#
  :: Word# -- ^ Number of bytes to read 
  -> Addr#
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s BigNat#
bigNatFromAddr# sz addr be =
  StateS# ( GHC.BigNum.bigNatFromAddr# sz addr be )

-- | Write a BigNat in base-256 little-endian representation and return the
-- number of bytes written.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToMutableByteArrayLE#
  :: BigNat# 
  -> MutableByteArray# s 
  -> Word# -- ^ Byte offset into the 'MutableByteArray#'
  -> StateS# s Word#
bigNatToMutableByteArrayLE# a mba off =
  StateS# ( GHC.BigNum.bigNatToMutableByteArrayLE# a mba off )

-- | Write a BigNat in base-256 big-endian representation and return the
-- number of bytes written.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToMutableByteArrayBE#
  :: BigNat# 
  -> MutableByteArray# s 
  -> Word#  -- ^ Offset into the 'MutableByteArray#'
  -> StateS# s Word#
bigNatToMutableByteArrayBE# a mba off =
  StateS# ( GHC.BigNum.bigNatToMutableByteArrayBE# a mba off )

-- | Write a BigNat in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Use \"@'bigNatSizeInBase' 256# /i/@\" to compute the exact number of bytes
-- written in advance. In case of @/i/ == 0@, the function will write and report
-- zero bytes written.
bigNatToMutableByteArray#
  :: BigNat# 
  -> MutableByteArray# s 
  -> Word# -- ^ Byte offset into the 'MutableByteArray#'
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Word#
bigNatToMutableByteArray# a mba off be =
  StateS# ( GHC.BigNum.bigNatToMutableByteArray# a mba off be )

-- | Read a BigNat in base-256 little-endian representation from a ByteArray#.
--
-- Null higher limbs are automatically trimed.
bigNatFromByteArrayLE#
  :: Word# -- ^ Number of bytes to read
  -> ByteArray# 
  -> Word# -- ^ Byte offset into the 'ByteArray#'
  -> StateS# s BigNat#
bigNatFromByteArrayLE# sz ba off =
  StateS# ( GHC.BigNum.bigNatFromByteArrayLE# sz ba off )

-- | Read a BigNat in base-256 big-endian representation from a ByteArray#.
--
-- Null higher limbs are automatically trimed.
bigNatFromByteArrayBE#
  :: Word# -- ^ Number of bytes to read
  -> ByteArray# 
  -> Word# -- ^ Byte offset into the 'ByteArray#'
  -> StateS# s BigNat#
bigNatFromByteArrayBE# sz ba off =
  StateS# ( GHC.BigNum.bigNatFromByteArrayBE# sz ba off )

-- | Read a BigNat in base-256 representation from a ByteArray#.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
bigNatFromByteArray#
  :: Word# -- ^ Number of bytes to read
  -> ByteArray# 
  -> Word# -- ^ Byte offset into the 'ByteArray#'
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s BigNat#
bigNatFromByteArray# sz ba off be =
  StateS# ( GHC.BigNum.bigNatFromByteArray# sz ba off be )

--------------------------------------------------------------------------------
-- Integer

-- | Write an 'Integer' (without sign) to @/addr/@ in base-256 representation
-- and return the number of bytes written.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
integerToAddr#
  :: Integer 
  -> Addr# 
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Word#
integerToAddr# i addr be =
  StateS# ( GHC.BigNum.integerToAddr# i addr be )

-- | Read an 'Integer' (without sign) in base-256 representation from an Addr#.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
integerFromAddr#
  :: Word# -- ^ Number of bytes to read
  -> Addr# 
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Integer
integerFromAddr# sz addr be =
  StateS# ( GHC.BigNum.integerFromAddr# sz addr be )

-- | Write an 'Integer' (without sign) in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
integerToMutableByteArray#
  :: Integer 
  -> MutableByteArray# s 
  -> Word# -- ^ Byte offset into the 'MutableByteArray#'
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Word#
integerToMutableByteArray# i mba off be =
  StateS# ( GHC.BigNum.integerToMutableByteArray# i mba off be )

-- | Read an 'Integer' (without sign) in base-256 representation from a 'ByteArray#'.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.

integerFromByteArray#
  :: Word# -- ^ Number of bytes to read
  -> ByteArray# 
  -> Word# -- ^ Byte offset into the 'MutableByteArray#'
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Integer
integerFromByteArray# sz ba off be =
  StateS# ( GHC.BigNum.integerFromByteArray# sz ba off be )

--------------------------------------------------------------------------------
-- Natural

-- | Write a 'Natural' to @/addr/@ in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
naturalToAddr#
  :: Natural 
  -> Addr# 
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Word#
naturalToAddr# n addr be =
  StateS# ( GHC.BigNum.naturalToAddr# n addr be )

-- | Read a Natural in base-256 representation from an Addr#.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.
naturalFromAddr#
  :: Word# -- ^ Number of bytes to read
  -> Addr# 
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Natural
naturalFromAddr# sz addr be =
  StateS# ( GHC.BigNum.naturalFromAddr# sz addr be )

-- | Write a Natural in base-256 representation and return the
-- number of bytes written.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
naturalToMutableByteArray#
  :: Natural 
  -> MutableByteArray# s 
  -> Word# -- ^ Byte offset into the 'MutableByteArray#'
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Word#
naturalToMutableByteArray# n mba off be =
  StateS# ( GHC.BigNum.naturalToMutableByteArray# n mba off be )

-- | Read a Natural in base-256 representation from a ByteArray#.
--
-- The endianness is selected with the Bool# parameter: most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- Null higher limbs are automatically trimed.

naturalFromByteArray#
  :: Word# -- ^ Number of bytes to read
  -> ByteArray# 
  -> Word# -- ^ Byte offset into the 'ByteArray#'
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Natural
naturalFromByteArray# sz ba off be =
  StateS# ( GHC.BigNum.naturalFromByteArray# sz ba off be )

--------------------------------------------------------------------------------
-- Primitives

-- | Write a Word to @/addr/@ in base-256 little-endian representation and
-- return the number of bytes written.
wordToAddrLE#
  :: Word# 
  -> Addr# 
  -> StateS# s Word#
wordToAddrLE# x addr =
  StateS# ( GHC.BigNum.wordToAddrLE# x addr )

-- | Write a Word to @/addr/@ in base-256 big-endian representation and
-- return the number of bytes written.
wordToAddrBE#
  :: Word# 
  -> Addr# 
  -> StateS# s Word#
wordToAddrBE# x addr =
  StateS# ( GHC.BigNum.wordToAddrBE# x addr )

-- | Write a Word to @/addr/@ in base-256 representation and
-- return the number of bytes written.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
wordToAddr#
  :: Word# 
  -> Addr# 
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Word#
wordToAddr# x addr be =
  StateS# ( GHC.BigNum.wordToAddr# x addr be )

-- | Read a Word from @/addr/@ in base-256 little-endian representation.
wordFromAddrLE#
  :: Word# -- ^ Number of bytes to read
  -> Addr# 
  -> StateS# s Word#
wordFromAddrLE# sz addr =
  StateS# ( GHC.BigNum.wordFromAddrLE# sz addr )

-- | Read a Word from @/addr/@ in base-256 big-endian representation.
wordFromAddrBE#
  :: Word# -- ^ Number of bytes to read
  -> Addr# 
  -> StateS# s Word#
wordFromAddrBE# sz addr =
  StateS# ( GHC.BigNum.wordFromAddrBE# sz addr )

-- | Read a Word from @/addr/@ in base-256 representation.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
wordFromAddr#
  :: Word# -- ^ Number of bytes to read
  -> Addr# 
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Word#
wordFromAddr# sz addr be =
  StateS# ( GHC.BigNum.wordFromAddr# sz addr be )

-- | Write a full word with little-endian encoding
wordWriteAddrLE#
  :: Word# 
  -> Addr# 
  -> StateS# s (##)
wordWriteAddrLE# x addr =
  StateS# \ s# ->
    case GHC.BigNum.wordWriteAddrLE# x addr s# of
      t# -> (# t#, (##) #)

-- | Write a full word with little-endian encoding
wordWriteAddrBE#
  :: Word# 
  -> Addr# 
  -> StateS# s (##)
wordWriteAddrBE# x addr =
  StateS# \ s# ->
    case GHC.BigNum.wordWriteAddrBE# x addr s# of
      t# -> (# t#, (##) #)

-- | Write a Word to @/MutableByteArray/@ in base-256 little-endian
-- representation and return the number of bytes written.
wordToMutableByteArrayLE#
  :: Word# -- ^ Word to write
  -> MutableByteArray# s 
  -> Word# -- ^ Byte offset into the 'MutableByteArray#'
  -> StateS# s Word#
wordToMutableByteArrayLE# x mba off =
  StateS# ( GHC.BigNum.wordToMutableByteArrayLE# x mba off )

-- | Write a Word to @/MutableByteArray/@ in base-256 big-endian representation and
-- return the number of bytes written.
wordToMutableByteArrayBE#
  :: Word# -- ^ Word to write
  -> MutableByteArray# s 
  -> Word# -- ^ Byte offset into the 'MutableByteArray#'
  -> StateS# s Word#
wordToMutableByteArrayBE# x mba off =
  StateS# ( GHC.BigNum.wordToMutableByteArrayBE# x mba off )

-- | Write a Word to @/MutableByteArray/@ in base-256 representation and
-- return the number of bytes written.
--
-- The endianness is selected with the Bool# parameter: write most significant
-- byte first (big-endian) if @1#@ or least significant byte first
-- (little-endian) if @0#@.
--
-- The offset is in bytes.
wordToMutableByteArray#
  :: Word# -- ^ Word to write
  -> MutableByteArray# s 
  -> Word# -- ^ Byte offset into the 'MutableByteArray#'
  -> Bool# -- ^ Use big-endian representation?
  -> StateS# s Word#
wordToMutableByteArray# x mba off be =
  StateS# ( GHC.BigNum.wordToMutableByteArray# x mba off be )

-- | Write a full word with little-endian encoding
wordWriteMutableByteArrayLE#
  :: Word# -- ^ Word to write
  -> MutableByteArray# s 
  -> Word# -- ^ Byte offset into the 'MutableByteArray#'
  -> StateS# s (##)
wordWriteMutableByteArrayLE# x mba off =
  StateS# \ s# ->
    case GHC.BigNum.wordWriteMutableByteArrayLE# x mba off s# of
      t# -> (# t#, (##) #)

-- | Write a full word with little-endian encoding
wordWriteMutableByteArrayBE#
  :: Word# -- ^ Word to write
  -> MutableByteArray# s 
  -> Word# -- ^ Byte offset into the 'MutableByteArray#'
  -> StateS# s (##)
wordWriteMutableByteArrayBE# x mba off =
  StateS# \ s# ->
    case GHC.BigNum.wordWriteMutableByteArrayBE# x mba off s# of
      t# -> (# t#, (##) #)

--------------------------------------------------------------------------------
-- WordArray

-- | Equality test for WordArray#
--
-- Get size in Words
mwaSize#
  :: MutableWordArray# s
  -> StateS# s Int#
mwaSize# mba =
  StateS# ( GHC.BigNum.mwaSize# mba )

-- | Copy Words from a WordArray
--
-- Don't do anything if the number of words to copy is <= 0
mwaArrayCopy#
  :: MutableByteArray# s -- ^ destination
  -> Int# -- ^ destination offset
  -> WordArray#  -- ^ source
  -> Int#  -- ^ source offset
  -> Int#  -- ^ number of words to copy
  -> StateS# s (##)
mwaArrayCopy# dst dstOff src srcOff sz =
  StateS# \ s# ->
    case GHC.BigNum.mwaArrayCopy# dst dstOff src srcOff sz s# of
      t# -> (# t#, (##) #)

-- | Shrink last words of a WordArray
mwaShrink#
  :: MutableByteArray# s 
  -> Int# -- ^ Number of words to shrink from the end
  -> StateS# s (##)
mwaShrink# mba i =
  StateS# \ s# ->
    case GHC.BigNum.mwaShrink# mba i s# of
      t# -> (# t#, (##) #)

-- | Shrink a mutable word array to the given number of words
mwaSetSize#
  :: MutableByteArray# s 
  -> Int# -- ^ Size (in words) to shrink to
  -> StateS# s (##)
mwaSetSize# mba sz =
  StateS# \ s# ->
    case GHC.BigNum.mwaSetSize# mba sz s# of
      t# -> (# t#, (##) #)

-- | Copy the WordArray into the MWA and shrink the size of MWA to the one s# of
-- the WordArray
mwaInitCopyShrink#
  :: MutableByteArray# s -- ^ destination
  -> WordArray#  -- ^ source
  -> StateS# s (##)
mwaInitCopyShrink# mba wa =
  StateS# \ s# ->
    case GHC.BigNum.mwaInitCopyShrink# mba wa s# of
      t# -> (# t#, (##) #)

-- | Trim ending zeroes
mwaTrimZeroes#
  :: MutableByteArray# s 
  -> StateS# s (##)
mwaTrimZeroes# mba =
  StateS# \ s# ->
    case GHC.BigNum.mwaTrimZeroes# mba s# of
      t# -> (# t#, (##) #)


-- | Count leading zero Words
mwaClz
  :: MutableWordArray# s 
  -> StateS# s Int#
mwaClz mba =
  StateS# ( GHC.BigNum.mwaClz mba )

-- | Count leading zero Words starting at given position
mwaClzAt
  :: MutableWordArray# s 
  -> Int# -- ^ index from which to start counting
  -> StateS# s Int#
mwaClzAt mba idx =
  StateS# ( GHC.BigNum.mwaClzAt mba idx )

-- | Compute MutableWordArray <- WordArray + Word
--
-- The MutableWordArray may not be initialized and will be erased anyway.
--
-- Input: Size(MutableWordArray) = Size(WordArray) + 1
-- Output: Size(MutableWordArray) = Size(WordArray) [+ 1]
mwaInitArrayPlusWord
  :: MutableWordArray# s -- ^ where to store the result
  -> WordArray# -- ^ source
  -> Word# -- ^ word to add
  -> StateS# s (##)
mwaInitArrayPlusWord mwa wa a =
  StateS# \ s# ->
    case GHC.BigNum.mwaInitArrayPlusWord mwa wa a s# of
      t# -> (# t#, (##) #)

-- | Write the most-significant Word:
--    * if it is 0: shrink the array by 1 Word
--    * otherwise: write it
mwaWriteOrShrink
  :: MutableWordArray# s 
  -> Word# -- ^ word to write
  -> Int#  -- ^ size in words of the word array
  -> StateS# s (##)
mwaWriteOrShrink mwa w i =
  StateS# \ s# ->
    case GHC.BigNum.mwaWriteOrShrink mwa w i s# of
      t# -> (# t#, (##) #)

-- | Compute the index of the most-significant Word and write it.
mwaWriteMostSignificant
  :: MutableWordArray# s
  -> Word# -- ^ 'Word#' to write
  -> StateS# s (##)
mwaWriteMostSignificant mwa w =
  StateS# \ s# ->
    case GHC.BigNum.mwaWriteMostSignificant mwa w s# of
      t# -> (# t#, (##) #)

-- | MutableWordArray <- zipWith op wa1 wa2
--
-- Required output: Size(MutableWordArray) = min Size(wa1) Size(wa2)
mwaInitArrayBinOp
  :: MutableWordArray# s -- ^ destination (must be at least as large as both 'WordArray#'s)
  -> WordArray#
  -> WordArray# 
  -> ( Word# -> Word# -> Word# ) -- ^ function to zip
  -> StateS# s (##)
mwaInitArrayBinOp mwa wa wb op =
  StateS# \ s# ->
    case GHC.BigNum.mwaInitArrayBinOp mwa wa wb op s# of
      t# -> (# t#, (##) #)

-- | Write an element of the MutableWordArray
mwaWrite#
  :: MutableWordArray# s 
  -> Int# -- ^ Index at which to write
  -> Word#  -- ^ Word to write
  -> StateS# s (##)
mwaWrite# mwa i w =
  StateS# \ s# ->
    case GHC.BigNum.mwaWrite# mwa i w s# of
      t# -> (# t#, (##) #)

-- | Fill some part of a 'MutableWordArray#' with the given 'Word#'
mwaFill#
  :: MutableWordArray# s 
  -> Word# -- ^ Value to write
  -> Word# -- ^ Offset
  -> Word# -- ^ Size of array
  -> StateS# s (##)
mwaFill# mwa v off sz =
  StateS# \ s# ->
    case GHC.BigNum.mwaFill# mwa v off sz s# of
      t# -> (# t#, (##) #)

-- | Add Word# inplace (a the specified offset) in the mwa with carry propagation.
mwaAddInplaceWord#
  :: MutableWordArray# s
  -> Int#   -- ^ Offset
  -> Word#  -- ^ Word to add
  -> StateS# s (##)
mwaAddInplaceWord# mwa off x =
  StateS# \ s# ->
    case GHC.BigNum.mwaAddInplaceWord# mwa off x s# of
      t# -> (# t#, (##) #)

-- | Subtract a Word# inplace (at the specified offset) in the mwa with carry
-- propagation.
--
-- Returns 'False#' on underflow
mwaSubInplaceWord#
  :: MutableWordArray# s
  -> Int# -- ^ Offset
  -> Word#  -- ^ Word to subtract
  -> StateS# s Bool#
mwaSubInplaceWord# mwa off x =
  StateS# ( GHC.BigNum.mwaSubInplaceWord# mwa off x )

-- | Trim `a` of `k` less significant limbs and then compare the result with `b`
--
-- "mwa" doesn't need to be trimmed
mwaTrimCompare
  :: Int# -- ^ `k`, number of least significant limbs to trim
  -> MutableWordArray# s -- ^ `a`
  -> WordArray# -- ^ `b`
  -> StateS# s Ordering
mwaTrimCompare k mwa wb =
  StateS# ( GHC.BigNum.mwaTrimCompare k mwa wb )

-- | Sub array inplace (at the specified offset) in the mwa with carry propagation.
--
-- We don't trim the resulting array!
--
-- Return 'False#' on underflow.
mwaSubInplaceArray
  :: MutableWordArray# s
  -> Int# -- ^ Offset at which to start the subtraction
  -> WordArray# 
  -> StateS# s Bool#
mwaSubInplaceArray mwa off wb =
  StateS# ( GHC.BigNum.mwaSubInplaceArray mwa off wb )

-- | Add array inplace (a the specified offset) in the mwa with carry propagation.
--
-- Upper bound of the result mutable aray is not checked against overflow.
mwaAddInplaceArray
  :: MutableWordArray# s
  -> Int# -- ^ Offset at which to start the addition
  -> WordArray# 
  -> StateS# s (##)
mwaAddInplaceArray mwa off wb =
  StateS# \ s# ->
    case GHC.BigNum.mwaAddInplaceArray mwa off wb s# of
      t# -> (# t#, (##) #)

-- | Sub array inplace (at the specified offset) in the mwa with carry propagation.
--
-- We don't trim the resulting array!
--
-- Return 'False#' on underflow.
mwaSubInplaceMutableArray
  :: MutableWordArray# s
  -> Int# -- ^ Offset at which to start the subtraction
  -> MutableWordArray# s
  -> StateS# s Bool#
mwaSubInplaceMutableArray mwa off mwb =
  StateS# ( GHC.BigNum.mwaSubInplaceMutableArray mwa off mwb )

-- | Sub an array inplace and then trim zeroes
--
-- Don't check overflow. The caller must ensure that @a>=b@
mwaSubInplaceArrayTrim
  :: MutableWordArray# s-- ^ `a`
  -> Int# -- ^ Offset at which to start the subtraction
  -> WordArray#  -- ^ `b`
  -> StateS# s (##)
mwaSubInplaceArrayTrim mwa off wb =
  StateS# \ s# ->
    case GHC.BigNum.mwaSubInplaceArrayTrim mwa off wb s# of
      t# -> (# t#, (##) #)

-- | Read an indexed Word in the MutableWordArray. If the index is out-of-bound,
-- return zero.
mwaReadOrZero
  :: MutableWordArray# s 
  -> Int# -- ^ Offset in words
  -> StateS# s Word#
mwaReadOrZero mwa i =
  StateS# ( GHC.BigNum.mwaReadOrZero mwa i )

-- | Read an indexed Word in the MutableWordArray. No bounds checking is performed.
mwaRead#
  :: MutableWordArray# s 
  -> Int# -- ^ Offset in words
  -> StateS# s Word#
mwaRead# mwa i =
  StateS# ( GHC.BigNum.mwaRead# mwa i )
