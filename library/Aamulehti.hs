{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, RecordWildCards #-}

module Aamulehti (main) where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Either
import Data.Time
import Data.Time.Format.ISO8601
import System.FilePath

import Login
import Print
import Types

{--
TODO: painospäivävalinta
TODO: järkevöitä verkko-operaatioiden välillä odottelu (monad stackin bindiin integrointi?)
-}


-- | An example function.
main :: IO ()
main =
  runExceptT (produceAamulehti >>= printAamulehti)
  >>= print

produceAamulehti :: NetworkOperation CompleteAamulehti
produceAamulehti = do
  catalog <- humaneWait =<< aamulehtiCatalog Nothing
  readAamulehtiCreds
    >>= aamulehtiLogin
    >>= humaneWait
    >>= aamulehtiRedirect (head $ mags catalog)
    >>= aamulehtiMH5
    >>= humaneWait
    >>= aamulehtiMagazine
    >>= aamulehtiDownload

printAamulehti :: CompleteAamulehti -> FileOperation FilePath
printAamulehti = aamulehtiPrint

readAamulehtiCreds :: FileOperation LoginRequest
readAamulehtiCreds = do
  creds <- liftIO $ T.lines <$> T.readFile "aamulehtiCreds"
  if (length creds < 2)
  then throwError "Kirjautumistunnus tai -salasana puuttuu aamulehtiCreds-tiedostosta."
  else return $ LoginRequest (head creds) (head $ tail creds)
