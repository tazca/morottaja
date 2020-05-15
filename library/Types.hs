{-# LANGUAGE DeriveGeneric, DuplicateRecordFields #-}

module Types where

import Data.Aeson
import qualified Data.ByteString as BS
import Data.Text
import Data.Time
import GHC.Generics

data LoginRequest =
  LoginRequest { username :: Text
               , password :: Text
               } deriving (Generic, Show)

instance ToJSON LoginRequest where
-- Generic derivoi

data LoginData =
  LoginData { username :: Text
            , token :: Text
            , hash :: Text
            , authorized :: Bool
            } deriving (Generic, Show)

instance FromJSON LoginData where
-- Generic derivoi

data MagsRequest =
  MagsRequest { startDate :: UTCTime -- "2020-05-04T20:59:59.999Z"
              , endDate :: UTCTime -- "2020-05-14T20:59:59.999Z"
              }

data AamulehtiMag =
  AamulehtiMag { uuid :: Text
               , name :: Text -- == date ("14.5.2020")
               , publishedAt :: UTCTime -- "2020-05-14T00:00:00.000Z"
               }

--instance FromJSON AamulehtiMag where


newtype Mags = Mags [AamulehtiMag]

data MH5Cookies =
  MH5Cookies { mh5_tok :: Text
             , mh5_ret :: Text
             }

data Magazine = -- magazine.json > variants > html5_splitjpg_960
  Magazine { baseurl :: Text
           , spreads :: [Spread]
           }

--instance FromJSON Magazine where

data Spread = -- magazine.json > variants > html5_splitjpg_960 > spreads
  Spread { leftPage :: Maybe Text
         -- left > portrait > images > background/overlay
         , leftOverlay :: Maybe Text
         , rightPage :: Maybe Text
         -- right > portrait > images > background/overlay
         , rightOverlay :: Maybe Text
         }

--instance FromJSON Spread where

data AamulehtiPage =
  AamulehtiPage { page :: BS.ByteString -- jpeg binary
                , overlay :: BS.ByteString -- png binary
                }

data CompleteAamulehti =
  CompleteAamulehti { date :: Day
                    , pages :: [AamulehtiPage]
                    }
