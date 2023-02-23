{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Login where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types
import Types

aamulehtiCatalog :: Maybe MagsRequest -> NetworkOperation Mags
aamulehtiCatalog cr = do
  catReq <- case cr of
    Just c ->
      let catalogStartDate = addUTCTime (negate $ 10 * nominalDay) $ endDate c
       in if (isJust $ startDate c)
            then return c
            else return $ MagsRequest (Just catalogStartDate) (endDate c)
    Nothing -> do
      -- a hacky way to strip time of day from current UTC time
      isoDate <-
        liftIO $
          iso8601ParseM
            =<< (\x -> x ++ "T00:00:00.000Z")
              <$> iso8601Show
              <$> utctDay
              <$> addUTCTime (secondsToNominalDiffTime 7200) -- UTC+2
              <$> getCurrentTime
      let time205959_999 = secondsToNominalDiffTime $ 20 * 3600 + 59 * 60 + 59 + 0.999
          catalogEndDate = addUTCTime time205959_999 isoDate
          catalogStartDate = addUTCTime (negate $ 10 * nominalDay) catalogEndDate
          todaysCatalog = MagsRequest (Just catalogStartDate) catalogEndDate
      return todaysCatalog

  liftIO $ putStrLn $ "Haetaan Aamulehden painosta " ++ iso8601Show (utctDay $ endDate catReq)
  req <-
    parseRequest
      ( "https://www.aamulehti.fi/aas/mags?include_linked=1&publication=aamulehti&start_date="
          ++ iso8601Show (fromJust $ startDate catReq)
          ++ "&end_date="
          ++ iso8601Show (endDate catReq)
      )
  getResponseBody <$> httpJSON req

aamulehtiLogin :: LoginRequest -> NetworkOperation LoginData
aamulehtiLogin lr = do
  req <-
    setRequestMethod "POST"
      <$> setRequestBodyJSON lr
      <$> parseRequest "https://www.aamulehti.fi/aas/login?publication=aamulehti"
  getResponseBody <$> httpJSON req

aamulehtiRedirect :: AamulehtiMag -> LoginData -> NetworkOperation RichiefiRedirect
aamulehtiRedirect mag lData = do
  let magUuid = T.unpack $ uuid mag
      user = T.unpack $ username (lData :: LoginData)
  req <-
    addAuthHeader (B.pack $ T.unpack $ token lData)
      <$> parseRequest
        ( "https://www.aamulehti.fi/aas/mags/" ++ magUuid
            ++ "?publication=aamulehti&username="
            ++ user
        )
  getResponseBody <$> httpJSON req
  where
    addAuthHeader :: B.ByteString -> Request -> Request
    addAuthHeader token = addRequestHeader hAuthorization (B.concat ["Bearer ", token])

aamulehtiMH5 :: RichiefiRedirect -> NetworkOperation MH5Cookies
aamulehtiMH5 redi = do
  mh5Req <- parseRequest (T.unpack $ redirect redi)
  responseCookieJar <$> httpBS mh5Req

aamulehtiMagazine :: MH5Cookies -> NetworkOperation Magazine
aamulehtiMagazine mh5 = do
  let magUrl =
        "https://aamulehti.ap.richiefi.net"
          ++ foldl (\acc x -> if cookie_name x == "mh5_tok" then B.unpack (cookie_path x) else acc) "" (destroyCookieJar mh5)
          ++ "/magazine.json"
  req <- parseRequest magUrl
  getResponseBody <$> httpJSON req {cookieJar = Just mh5}
