{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, RecordWildCards #-}

module Aamulehti (main) where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Either
import Data.Time
import Data.Time.Format.ISO8601
import System.FilePath

import Login
import Print
import Types

-- | An example function.
main :: IO ()
main = do
  --testMags <- LB.readFile jsonTestMags
  --print (eitherDecode testMags :: Either String Mags)
  --testMagazine <- LB.readFile jsonTestMagazine
  --print (eitherDecode testMagazine :: Either String Magazine)

  isoDate <- getContents

  creds <- getAamulehtiCreds
  print (encode creds)

  loginData <- getAamulehtiCreds >>= getLoginData
  print loginData

  magsEndDate <- iso8601ParseM isoDate --"2020-05-18T20:59:59.999Z"
  let placeholderMagsCatalogReq = MagsRequest Nothing magsEndDate
  magsCatalog <- getMags placeholderMagsCatalogReq
  print magsCatalog

  case loginData of
    (Just lData) -> do
      case magsCatalog of
        (Just mc) -> do
          mh5 <- getMH5 (head $ mags mc) lData
          print mh5
          case mh5 of
            (Just mh5Cookies) -> do
              magazineJson <- getMagazineJson (head $ mags mc) mh5Cookies
              print magazineJson
              case magazineJson of
                (Just zine) ->
                  getMagazine zine >>= printMagazine >>= putStrLn
                Nothing ->
                  putStrLn "Ei saatu magazine.jsonia"
            Nothing ->
              putStrLn "MH5-keksit jäi saamatta."
        Nothing ->
          putStrLn "mags.jsonin hakeminen epäonnistui."
    Nothing ->
      putStrLn "Kirjautuminen epäonnistui."



getAamulehtiCreds :: IO LoginRequest
getAamulehtiCreds = do
  creds <- T.lines <$> T.pack <$> readFile "aamulehtiCreds"
  return $ LoginRequest (head creds) (head $ tail creds)
