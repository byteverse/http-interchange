{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  ( main
  ) where

import Control.Monad (when)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Http.Headers (Headers)
import Http.Types (Header, Request)
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Text.Show.Pretty (ppShow)

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as LBC8
import Data.Bytes qualified as Bytes
import Data.Bytes.Chunks qualified as Chunks
import Data.List qualified as List
import Data.Primitive qualified as PM
import GHC.Exts qualified as Exts
import Http.Header qualified
import Http.Headers qualified as Headers
import Http.Request qualified as Request
import Http.Response qualified as Response

main :: IO ()
main = defaultMain =<< goldenTests

deriving stock instance Generic Header
deriving stock instance Generic Request.RequestLine
deriving stock instance Generic Request.Request

deriving anyclass instance FromJSON Header
deriving anyclass instance FromJSON Request.RequestLine
deriving anyclass instance FromJSON Request.Request

instance FromJSON Headers where
  parseJSON = fmap Headers.fromArray . Aeson.parseJSON

goldenTests :: IO TestTree
goldenTests = do
  responseFiles <- List.sort <$> findByExtension [".input"] "golden/response"
  requestFiles <- List.sort <$> findByExtension [".json"] "golden/request"
  return $
    testGroup
      "http"
      [ testGroup
          "response"
          [ goldenVsString
            (takeBaseName file) -- test name
            outFile -- golden file path
            ( do
                contents <- Bytes.readFile file
                when (PM.sizeofPrimArray (Bytes.findIndices (Exts.fromList [0x0d, 0x0a]) contents) < 2) $ do
                  fail "Expected at least 2 CRLF sequences. Possible fix: unix2dos."
                case Response.decode 128 contents of
                  Nothing -> fail "Could not decode HTTP response prelude"
                  Just resp -> pure (LBC8.pack (addTrailingNewline (ppShow resp)))
            )
          | file <- responseFiles
          , let outFile = replaceExtension file ".output"
          ]
      , testGroup
          "request"
          [ goldenVsString
            (takeBaseName file) -- test name
            outFile -- golden file path
            ( do
                req :: Request <- either fail pure =<< Aeson.eitherDecodeFileStrict' file
                pure $ LBC8.fromStrict $ Bytes.toByteString $ Chunks.concat $ Request.toChunks req
            )
          | file <- requestFiles
          , let outFile = replaceExtension file ".output"
          ]
      ]

-- If the trailing newline is missing, add it. The pretty-show library
-- has a defect of omitting it, but if prett-show ever changes, this
-- function will prevent that change from breaking all the tests here.
addTrailingNewline :: String -> String
addTrailingNewline [] = "\n"
addTrailingNewline [x] = case x of
  '\n' -> [x]
  _ -> [x, '\n']
addTrailingNewline (x : xs) = x : addTrailingNewline xs
