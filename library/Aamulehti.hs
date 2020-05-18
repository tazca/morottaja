-- | An example module.
module Aamulehti (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Either
import System.FilePath

import Print
import Types

jsonTestMags :: FilePath
jsonTestMags = "mags.json"

jsonTestMagazine :: FilePath
jsonTestMagazine = "magazine.json"

-- | An example function.
main :: IO ()
main = do
  testMags <- LB.readFile jsonTestMags
  print (eitherDecode testMags :: Either String Mags)
  testMagazine <- LB.readFile jsonTestMagazine
  print (eitherDecode testMagazine :: Either String Magazine)
  creds <- getAamulehtiCreds
  print (encode creds)
  status <- either return (\mag -> getMagazine mag >>= printMagazine) (eitherDecode testMagazine)
  print status


getAamulehtiCreds :: IO LoginRequest
getAamulehtiCreds = do
  creds <- T.lines <$> T.pack <$> readFile "aamulehtiCreds"
  return $ LoginRequest (head creds) (head $ tail creds)
