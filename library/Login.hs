{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, RecordWildCards #-}

module Login where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types

import Types

getLoginData :: LoginRequest -> IO (Maybe LoginData)
getLoginData loginReq =
  let req = setRequestMethod "POST"
        <$> setRequestBodyJSON loginReq
        <$> parseRequest "https://www.aamulehti.fi/aas/login?publication=aamulehti" :: Maybe Request
  in case req of
       (Just r) -> do
         print r
         getResponseBody <$> httpJSON r
       Nothing -> return Nothing

getMags :: MagsRequest -> IO (Maybe Mags)
getMags magsReq = do
  let magsJsonReq = parseRequest
                    ("https://www.aamulehti.fi/aas/mags?include_linked=1&publication=aamulehti&start_date="
                     ++ iso8601Show (fillInStartDate (endDate magsReq))
                     ++ "&end_date=" ++ iso8601Show (endDate magsReq))
  case magsJsonReq of
    (Just r) -> do
      getResponseBody <$> httpJSON r
      --mags <- httpJSON r :: IO (Response Mags)
      --return (Just $ getResponseBody mags)
    Nothing -> do
      putStrLn "Virheellisesti muodostettu mags.json request."
      return Nothing
  where
    fillInStartDate endDate = addUTCTime (negate $ 10 * nominalDay) endDate

getMH5 :: AamulehtiMag -> LoginData -> IO (Maybe MH5Cookies)
getMH5 mag lData =
  let magUuid = T.unpack $ uuid mag
      user = T.unpack $ username (lData :: LoginData)
      req :: Maybe Request
      req = addAuthHeader (B.pack $ T.unpack $ token lData)
        <$> parseRequest ("https://www.aamulehti.fi/aas/mags/" ++ magUuid
                         ++ "?publication=aamulehti&username=" ++ user)
  in case req of
       (Just r) -> do
         redirectUrl <- getResponseBody <$> httpJSON r
         let mh5Req = parseRequest (T.unpack $ redirect redirectUrl)
         case mh5Req of  -- TODO: tarkastele löytyykö sekä mh5_tok että mh5_ret
           (Just mh5R) -> do
             gotCookies <- responseCookieJar <$> httpBS mh5R
             return $ Just gotCookies
           Nothing -> do
             putStrLn "Richiefin redirect URLin kanssa ongelma."
             return Nothing
       Nothing -> do
         putStrLn "Virheellisesti muodostettu MH5 request."
         return Nothing
  where
    addAuthHeader :: B.ByteString -> Request -> Request
    addAuthHeader token = addRequestHeader hAuthorization (B.concat ["Bearer ", token])

getMagazineJson :: AamulehtiMag -> MH5Cookies -> IO (Maybe Magazine)
getMagazineJson mag mh5 = do
  let magUrl = "https://aamulehti.ap.richiefi.net" ++
               foldl (\acc x -> if cookie_name x == "mh5_tok" then B.unpack (cookie_path x) else acc) "" (destroyCookieJar mh5) ++
               "/magazine.json"
  protoReq <- parseRequest magUrl
  let req = protoReq { cookieJar = Just mh5 }
  getResponseBody <$> httpJSON req
