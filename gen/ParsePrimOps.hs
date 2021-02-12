
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ParsePrimOps where

-- base
import Control.Arrow
  ( (&&&) )
import Control.Monad
  ( void )
import Data.Char
  ( isAlphaNum )
import Data.Functor
  ( ($>) )
import qualified Data.List.NonEmpty as NonEmpty
  ( init, last )
import Data.Void
  ( Void )

-- megaparsec
import Text.Megaparsec
  ( MonadParsec )
import qualified Text.Megaparsec as Mp

import qualified Text.Megaparsec.Char as Mp

import qualified Text.Megaparsec.Char.Lexer as Mp.Lex

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text

-- parser-combinators
import Control.Applicative.Combinators
import qualified Control.Applicative.Combinators.NonEmpty as NonEmpty

--------------------------------------------------------------------------------

data PrimOp
  = PrimOp
  { primOpName :: Text
  , primOpArgs :: [PrimType]
  , primOpRes  :: PrimType
  , primOpDocs :: Maybe Text
  }
  deriving stock Show

data PrimType
  = TyApp Text [PrimType]
  | UnboxedTuple [PrimType]
  | Function [PrimType] PrimType
  deriving stock Show

primOps :: MonadParsec Void Text m => m [PrimOp]
primOps = do
  isPrim <- skipManyTill ( many ( Mp.Lex.skipLineComment "--" ) *> Mp.anySingle ) do
    ( Mp.lookAhead ( Mp.try ( Mp.chunk "primop" ) $> True <|> Mp.chunk "thats_all_folks" $> False ) )
  if isPrim
  then (:) <$> primOp <*> primOps
  else pure []

primOp :: MonadParsec e Text m => m PrimOp
primOp = do

  _ <- lexeme ( Mp.chunk "primop" )
  _ <- lexeme ( Mp.takeWhile1P ( Just "alphaNum or _" ) ( \ c -> isAlphaNum c || c == '_' ) )

  primOpName <- lexeme do
    _ <- Mp.single '"'
    primOpName <- name
    _ <- Mp.single '"'
    pure primOpName

  _ <- lexeme ( Mp.takeWhile1P ( Just "alphaNum" ) isAlphaNum )

  ( primOpArgs, primOpRes ) <-
    ( NonEmpty.init &&& NonEmpty.last ) <$> ( primType `NonEmpty.sepBy1` arrow )

  primOpDocs <- optional docs

  pure ( PrimOp {..} )

space :: MonadParsec e Text m => m ()
space =
  Mp.Lex.space
    Mp.space1
    ( Mp.Lex.skipLineComment "--" )
    ( Mp.Lex.skipBlockCommentNested "<!--" "--!>" )

lexeme :: MonadParsec e Text m => m a -> m a
lexeme = Mp.Lex.lexeme space

name :: MonadParsec e Text m => m Text
name = do
  Mp.try $ Mp.notFollowedBy ( Mp.chunk "with" )
  Mp.try $ Mp.notFollowedBy arrow
  Text.cons
    <$> Mp.satisfy ( \ c -> isAlphaNum c || c `elem` allowedSymbols ) -- does not include '#'
    <*> Mp.takeWhileP ( Just "alphaNum or allowed symbol" ) ( \ c -> isAlphaNum c || c `elem` ( '#' : allowedSymbols ) )

arrow :: MonadParsec e Text m => m ()
arrow = lexeme ( void $ Mp.chunk "->" )

primType :: MonadParsec e Text m => m PrimType
primType = tyApp <|> unboxedTuple <|> function

tyApp :: MonadParsec e Text m => m PrimType
tyApp = do
  nm <- lexeme name
  args <- many arg
  pure ( TyApp nm args )

arg :: MonadParsec e Text m => m PrimType
arg = ( ( \nm -> TyApp nm [] ) <$> lexeme name )
  <|> ( lexeme ( Mp.chunk "(" ) *> primType <* lexeme ( Mp.chunk ")" ) )

unboxedTuple :: MonadParsec e Text m => m PrimType
unboxedTuple = do
  _ <- lexeme ( Mp.chunk "(#" )
  args <- primType `sepBy` lexeme ( Mp.single ',' )
  _ <- lexeme ( Mp.chunk "#)" )
  pure ( UnboxedTuple args )

function :: MonadParsec e Text m => m PrimType
function = do
  _ <- lexeme ( Mp.chunk "(" )
  ( args, res ) <- ( NonEmpty.init &&& NonEmpty.last ) <$> primType `NonEmpty.sepBy1` arrow
  _ <- lexeme ( Mp.chunk ")" )
  pure ( Function args res )

allowedSymbols :: [ Char ]
allowedSymbols = [ '*', '+', '-', '<', '>', '=', '/', '_' ]

docs :: MonadParsec e Text m => m Text
docs = do
  _ <- Mp.chunk "{"
  Text.concat <$> manyTill
    ( italicComment <|> monotypeComment <|> basicComment )
    ( Mp.chunk "}" )

italicComment :: MonadParsec e Text m => m Text
italicComment = do
  _ <- Mp.chunk "{\\it"
  content <- Mp.takeWhile1P ( Just "italic comment text" ) ( \ c -> c /= '}' )
  pure ( "/" <> content <> "/" )
  -- TOOD: this seems messed up

monotypeComment :: MonadParsec e Text m => m Text
monotypeComment = do
  _ <- Mp.chunk "{\\tt"
  content <- Mp.takeWhile1P ( Just "monotype comment text" ) ( \ c -> c /= '}' )
  pure ( "@" <> content <> "@" )

basicComment ::MonadParsec e Text m => m Text
basicComment = Mp.takeWhile1P ( Just "comment text" ) ( \ c -> c /= '}' )


{-
TODO
what's up with the following primop not being auto-generated:

primop  AtomicModifyMutVar2Op "atomicModifyMutVar2#" GenPrimOp
   MutVar# s a -> (a -> c) -> State# s -> (# State# s, a, c #)
   { Modify the contents of a {\tt MutVar\#}, returning the previous
     contents and the result of applying the given function to the
     previous contents. Note that this isn't strictly
     speaking the correct type for this function; it should really be
     {\tt MutVar\# s a -> (a -> (a,b)) -> State\# s -> (\# State\# s, a, (a, b) \#)},
     but we don't know about pairs here. }
   with
   out_of_line = True
   has_side_effects = True
   can_fail         = True

also an issue with shrinkMutableByteArray# ?

-}