
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- megaparsec
import qualified Text.Megaparsec as Mp

-- text
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- gen-primops
import GenPrimOps
  ( genPrimOps )
import qualified ParsePrimOps as Parse
  ( primOps )


--------------------------------------------------------------------------------

main :: IO ()
main = do
  contents <- Text.readFile "gen/primops.txt"
  case Mp.parse Parse.primOps "primops.txt" contents of
    Left err      -> putStr ( Mp.errorBundlePretty err )
    Right primOps -> case genPrimOps primOps of
      ( primOpNames, primOpImpls ) -> do
        Text.appendFile "gen/primops_impl.txt"
          ( Text.unlines primOpNames <> "\n\n\n\n\n" <> Text.intercalate "\n\n" primOpImpls )
