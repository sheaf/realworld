{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module GenPrimOps where

-- base
import qualified Data.Char as Char
  ( isLower )
import Data.Maybe
  ( isJust, mapMaybe )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
  ( all, intercalate, lines, pack, strip )

-- gen-primops
import ParsePrimOps
  ( PrimOp(..), PrimType(..) )

--------------------------------------------------------------------------------

data StatePrimOp
  = SimpleStatePrimOp
    { primOpName  :: Text
    , primOpDocs  :: Maybe Text
    , prevArgs    :: [ PrimType ]
    , otherResTys :: [ PrimType ]
    }

--------------------------------------------------------------------------------
-- Generating code for supported primops.

genPrimOps :: [ PrimOp ] -> ( [Text], [Text] )
genPrimOps primOps =
  ( map ( primOpName :: StatePrimOp -> Text ) statePrimOps
  , map genPrimOp statePrimOps
  )
  where
    statePrimOps :: [ StatePrimOp ]
    statePrimOps = mapMaybe recogniseStatePrimOp primOps

genPrimOp :: StatePrimOp -> Text
genPrimOp primOp = primOpHaddocks ( ( primOpDocs :: StatePrimOp -> Maybe Text ) primOp )
               <> primOpTySig primOp <> "\n"
               <> primOpImpl primOp

primOpHaddocks :: Maybe Text -> Text
primOpHaddocks Nothing = ""
primOpHaddocks ( Just docs ) =
  "-- | " <> Text.intercalate "\n-- " ( map Text.strip $ Text.lines docs ) <> "\n"

primOpTySig :: StatePrimOp -> Text
primOpTySig ( SimpleStatePrimOp { primOpName, prevArgs, otherResTys } ) =
  primOpName <> "\n  :: " <> Text.intercalate "\n  -> " ( map ( showTy False ) prevArgs <> [ showResTy otherResTys ] )

showResTy :: [ PrimType ] -> Text
showResTy []   = "StateS# s (##)"
showResTy [ty] = "StateS# s " <> showTy True ty
showResTy tys  = "StateS# s (# " <> Text.intercalate ", " ( map ( showTy False ) tys ) <> " #)"

showTy :: Bool -> PrimType -> Text
showTy _     ( TyApp ty []   ) = ty
showTy False ( TyApp ty args ) =
  ty <> " " <> Text.intercalate " " ( map ( showTy True ) args )
showTy True  ( TyApp ty args ) =
  "( " <> ty <> " " <> Text.intercalate " " ( map ( showTy True ) args ) <> " )"
showTy _ ( UnboxedTuple []  )  = "(##)"
showTy _ ( UnboxedTuple tys )  = "(# " <> Text.intercalate ", " ( map ( showTy False ) tys ) <> " #)"
showTy _ ( Function args res ) = "( " <> Text.intercalate " -> " ( map ( showTy False ) ( args <> [res] ) ) <> " )"

primOpImpl :: StatePrimOp -> Text
primOpImpl ( SimpleStatePrimOp { primOpName, prevArgs, otherResTys } ) =
  primOpName <> arguments <> "=\n" <>
  case otherResTys of
    [] -> "  StateS# \\ s# ->\n" <>
          "    case GHC.Exts." <> primOpName <> arguments <> "s# of\n" <>
          "      t# -> (# t#, (##) #)"
    [_] 
      | null prevArgs
      -> "  StateS# GHC.Exts." <> primOpName
      | otherwise
      -> "  StateS# ( GHC.Exts." <> primOpName <> arguments <> ")"
    _ ->  "  StateS# \\ s# ->\n" <>
          "    case GHC.Exts." <> primOpName <> arguments <> "s# of\n" <>
          "      (# t#," <> results <> "#) -> (# t#, (#" <> results <> "#) #)"

  where
    arguments, results :: Text
    arguments
      | null prevArgs
      = " "
      | otherwise
      = " " <> Text.intercalate " " [ "arg" <> Text.pack ( show i ) | ( i :: Int ) <- [ 1 .. length prevArgs    ] ] <> " "
    results
      = " " <> Text.intercalate ", " [ "res" <> Text.pack ( show i ) | ( i :: Int ) <- [ 1 .. length otherResTys ] ] <> " "

--------------------------------------------------------------------------------
-- Recognising supported primops.

recogniseStatePrimOp :: PrimOp -> Maybe StatePrimOp
recogniseStatePrimOp ( PrimOp { primOpName, primOpDocs, primOpArgs, primOpRes } )
  | Just prevArgs    <- splitArgsState# primOpArgs
  , Just otherResTys <- splitResState#  primOpRes
  , not ( any ( isJust . vectorStandin ) prevArgs )
  , not ( any ( isJust . vectorStandin ) otherResTys )
  = Just ( SimpleStatePrimOp { primOpName, primOpDocs, prevArgs, otherResTys } )
  | otherwise
  = Nothing

splitArgsState# :: [ PrimType ] -> Maybe [ PrimType ]
splitArgsState# []
  = Nothing
splitArgsState# [ TyApp "State#" [ TyApp tyVar [] ] ]
  | Text.all Char.isLower tyVar
  = Just []
splitArgsState# [ _ ]
  = Nothing
splitArgsState# ( a : as )
  = ( a : ) <$> splitArgsState# as

splitResState# :: PrimType -> Maybe [ PrimType ]
splitResState# ( TyApp "State#" [ TyApp tyVar [] ] )
  | Text.all Char.isLower tyVar
  = Just []
splitResState# ( UnboxedTuple ( TyApp "State#" [ TyApp tyVar [] ] : rest ) )
  | Text.all Char.isLower tyVar
  = Just rest
splitResState# _
  = Nothing

data VectorStandin
  = VECTOR
  | SCALAR
  | VECTUPLE

vectorStandin :: PrimType -> Maybe VectorStandin
vectorStandin ( TyApp standin [] ) = case standin of
  "VECTOR"   -> Just VECTOR
  "SCALAR"   -> Just SCALAR
  "VECTUPLE" -> Just VECTUPLE
  _          -> Nothing
vectorStandin _ = Nothing

{-

primop  MkWeakOp "mkWeak#" GenPrimOp
   o -> b -> (State# RealWorld -> (# State# RealWorld, c #))
     -> State# RealWorld -> (# State# RealWorld, Weak# b #)

primop  FinalizeWeakOp "finalizeWeak#" GenPrimOp
   Weak# a -> State# RealWorld -> (# State# RealWorld, Int#,
              (State# RealWorld -> (# State# RealWorld, b #) ) #)

-}
