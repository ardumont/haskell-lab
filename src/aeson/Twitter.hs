{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Twitter where

import Web.Authenticate.OAuth
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Aeson
import GHC.Generics

myoauth :: OAuth
myoauth =
  newOAuth { oauthServerName     = "api.twitter.com"
           , oauthConsumerKey    = "your consumer key here"
           , oauthConsumerSecret = "your consumer secret here"
             }

mycred :: Credential
mycred = newCredential "your access token here"
                       "your access token secret here"
data Tweet =
  Tweet { text :: !Text
        , created_at :: !UTCTime
          } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet
