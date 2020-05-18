{-# LANGUAGE DuplicateRecordFields, OverloadedStrings, RecordWildCards #-}

module Types where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Time
import Network.HTTP.Conduit (CookieJar)

{--
Huomioi tilanteet joissa JSONin result /= 200 virhetilanteiden hienosäätöä varten; nyt riittää vain että parser palauttaa virheen.

--}
data LoginRequest =
  LoginRequest { username :: T.Text
               , password :: T.Text
               } deriving (Show)

instance ToJSON LoginRequest where
  toJSON LoginRequest{..} =
    object [
    "username" .= username,
    "password" .= password
    ]

data LoginData =
  LoginData { username :: T.Text
            , token :: T.Text
            , hash :: T.Text
            , authorized :: Bool
            } deriving (Show)

instance FromJSON LoginData where
  parseJSON = withObject "LoginData" $ \v -> do
    nestedLogin <- v .: "value"
    username <- nestedLogin .: "username"
    token <- nestedLogin .: "token"
    hash <- nestedLogin .: "hash"
    authorized <- nestedLogin .: "authorized"
    return LoginData{..}

data MagsRequest =
  MagsRequest { startDate :: Maybe UTCTime -- "2020-05-04T20:59:59.999Z"
                             -- if Nothing then endDate - 10 days mimicking the website
              , endDate :: UTCTime -- "2020-05-14T20:59:59.999Z"
              }

data AamulehtiMag = -- value > mags > Array Aamulehti
  AamulehtiMag { uuid :: T.Text
               , name :: T.Text -- == date ("14.5.2020")
               , publishedAt :: UTCTime -- "2020-05-14T00:00:00.000Z"
               } deriving (Show)

instance FromJSON AamulehtiMag where
  parseJSON = withObject "AamulehtiMag" $ \v -> do
    uuid <- v .: "uuid"
    name <- v .: "name"
    publishedAt <- v .: "publishedAt"
    return AamulehtiMag{..}

data Mags =
  Mags { mags :: [AamulehtiMag]
       } deriving (Show)

instance FromJSON Mags where
  parseJSON = withObject "Mags" $ \v -> do
    nestedFirst <- v .: "value"
    nestedSecond <- nestedFirst .: "mags"
    mags <- nestedSecond .: "Aamulehti"
    return Mags{..}

data RichiefiRedirect =
  RichiefiRedirect { redirect :: T.Text
                   }

instance FromJSON RichiefiRedirect where
  parseJSON = withObject "RichiefiRedirect" $ \v -> do
    redirect <- v .: "value"
    return RichiefiRedirect{..}

type MH5Cookies = CookieJar

data Spread =
  Spread { leftPage :: Maybe (T.Text, T.Text)
         , rightPage :: Maybe (T.Text, T.Text)
         } deriving (Show)

instance FromJSON Spread where
  parseJSON = withObject "Spread" $ \v -> do
    let maybeMap obj str = mapM (\x -> x .: str) obj -- >>= olisi elegantimpi
    nestedLeft <- v .:? "left"

    nestedLPortrait <- maybeMap nestedLeft "portrait"
    nestedLImages <- maybeMap nestedLPortrait "images"
    leftPage <- maybeMap nestedLImages "background"
    leftOverlay <- maybeMap nestedLImages "overlay"

    nestedRight <- v .:? "right"
    nestedRPortrait <- maybeMap nestedRight "portrait"
    nestedRImages <- maybeMap nestedRPortrait "images"
    rightPage <- maybeMap nestedRImages "background"
    rightOverlay <- maybeMap nestedRImages "overlay"
    return $ Spread (fuseMaybes leftPage leftOverlay) (fuseMaybes rightPage rightOverlay)
      where
        fuseMaybes (Just pg) (Just ol) = Just (pg, ol)
        fuseMaybes (Just _) Nothing = Nothing
        fuseMaybes Nothing (Just _) = Nothing
        fuseMaybes Nothing Nothing = Nothing

data Magazine = -- magazine.json > variants > html5_splitjpg_960
  Magazine { baseurl :: T.Text
           , spreads :: [Spread]
           } deriving (Show)

instance FromJSON Magazine where
  parseJSON = withObject "Magazine" $ \v -> do
    nestedFirst <- v .: "variants"
    nestedSecond <- nestedFirst .: "html5_splitjpg_960"
    baseurl <- nestedSecond .: "baseurl"
    spreads <- nestedSecond .: "spreads"
    return Magazine{..}

type PageNumber = Int

data AamulehtiPage =
  AamulehtiPage { page :: B.ByteString -- jpeg binary
                , overlay :: B.ByteString -- png binary
                , pageNumber :: PageNumber
                }

data CompleteAamulehti =
  CompleteAamulehti { pages :: [AamulehtiPage]
                    } -- TODO: date
