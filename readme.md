# realworld <a href="https://hackage.haskell.org/package/realworld" alt="Hackage"><img src="https://img.shields.io/hackage/v/realworld.svg" /></a>

* [Introduction](#intro)
* [Quick start](#start)

<a name="intro"></a>
# Introduction

A simple levity polymorphic `State#`-token-passing monad for writing state-token-passing code
in a more idiomatic – monadic – style.    
Currently only supports the basic monadic operations (`fmap`, `pure`, `>>=`).    

Provides drop-in replacements for [`GHC.Exts`](https://hackage.haskell.org/package/base/docs/GHC-Exts.html) and
[`ghc-bignum`](https://hackage.haskell.org/package/ghc-bignum).    

Crudely bypasses levity polymorphism restrictions using typeclasses.

<a name="start"></a>
# Quick start

Here's a simple before/after example (converting a `Natural` to base 256 representation stored in a `ByteArray`):

```haskell
import qualified GHC.Exts          as GHC
import qualified GHC.Num.Natural   as GHC
import qualified GHC.Num.WordArray as GHC

intoBytes_ghc :: Int -> Natural -> ByteArray#
intoBytes_ghc (I# nbBytes) i = GHC.runRW# \ s1 ->
  case GHC.newByteArray# nbBytes s1 of
    (# s2, mba #) -> case GHC.naturalToMutableByteArray# i mba 0## 0# s2 of
      (# s3, word2Int# -> bytesWritten #) ->
        case GHC.setByteArray# mba bytesWritten ( nbBytes -# bytesWritten ) 0# s3 of
          s4 -> case GHC.unsafeFreezeByteArray# mba s4 of
            (# _, ba #) -> ba
```

```haskell
import qualified RealWorld.Monad       as RW -- typeclasses providing monadic operations
import qualified RealWorld.Monad.State as RW -- StateS# monad
import qualified RealWorld.GHC         as RW -- drop-in replacement for GHC.Exts
import qualified RealWorld.GHC.BigNum  as RW -- drop-in replacement for ghc-bignum

intoBytes_rw :: Int -> Natural -> ByteArray#
intoBytes_rw (I# nbBytes) i = RW.runRWS# RW.do
  mba <- RW.newByteArray# nbBytes
  ( word2Int# -> bytesWritten ) <- RW.naturalToMutableByteArray# i mba 0## 0#
  RW.setByteArray# mba bytesWritten ( nbBytes -# bytesWritten ) 0#
  RW.unsafeFreezeByteArray# mba
```
