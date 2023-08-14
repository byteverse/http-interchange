{-# language DeriveAnyClass #-}
{-# language DerivingStrategies #-}

module Main
  ( main
  ) where

import Control.Monad (when)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Http.Types (Request,Header)
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString, findByExtension)
import Text.Show.Pretty (ppShow)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBC8
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified GHC.Exts as Exts
import qualified Http.Header
import qualified Http.Request as Request
import qualified Http.Response as Response

main :: IO ()
main = defaultMain =<< goldenTests

deriving stock instance Generic Header
deriving stock instance Generic Request.RequestLine
deriving stock instance Generic Request.Request

deriving anyclass instance FromJSON Header
deriving anyclass instance FromJSON Request.RequestLine
deriving anyclass instance FromJSON Request.Request

goldenTests :: IO TestTree
goldenTests = do
  responseFiles <- List.sort <$> findByExtension [".input"] "golden/response"
  requestFiles <- List.sort <$> findByExtension [".json"] "golden/request"
  return $ testGroup "http"
    [ testGroup "response"
      [ goldenVsString
          (takeBaseName file) -- test name
          outFile -- golden file path
          (do contents <- Bytes.readFile file
              when (PM.sizeofPrimArray (Bytes.findIndices (Exts.fromList [0x0d,0x0a]) contents) < 2) $ do
                fail "Expected at least 2 CRLF sequences. Possible fix: unix2dos."
              case Response.decode 128 contents of
                Nothing -> fail "Could not decode HTTP response prelude"
                Just resp -> pure (LBC8.pack (addTrailingNewline (ppShow resp)))
          )
      | file <- responseFiles
      , let outFile = replaceExtension file ".output"
      ]
    , testGroup "request"
      [ goldenVsString
          (takeBaseName file) -- test name
          outFile -- golden file path
          (do req :: Request <- either fail pure =<< Aeson.eitherDecodeFileStrict' file
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
  _ -> [x,'\n']
addTrailingNewline (x : xs) = x : addTrailingNewline xs
