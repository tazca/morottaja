{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Aamulehti (main) where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Format.ISO8601
import Login
import Print
import System.FilePath
import Types

{--
TODO: painospäivävalinta
TODO: näytä näköislehtimanifestin sivumäärä alussa
TODO: järkevöitä verkko-operaatioiden välillä odottelu (monad stackin bindiin integrointi?)
-}

-- | An example function.
main :: IO ()
main =
  runExceptT (produceAamulehti >>= printAamulehti)
    >>= print

produceAamulehti :: NetworkOperation CompleteAamulehti
produceAamulehti = do
  catalog <- humaneWait "Katalogi haettu." =<< aamulehtiCatalog Nothing
  readAamulehtiCreds
    >>= aamulehtiLogin
    >>= humaneWait "Kirjauduttu sisään."
    >>= aamulehtiRedirect (head $ mags catalog)
    >>= aamulehtiMH5
    >>= humaneWait "Saavuttu näköislehtilukijaan."
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
