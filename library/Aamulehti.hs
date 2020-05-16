-- | An example module.
module Aamulehti (main) where

import Data.Aeson
import qualified Data.ByteString.Lazy as LB
import Data.Either

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
