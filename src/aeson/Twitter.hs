{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Twitter where

import Web.Authenticate.OAuth (OAuth, signOAuth, newOAuth, oauthServerName, oauthConsumerKey, oauthConsumerSecret, newCredential, Credential)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.Generics
import Network.HTTP.Conduit (parseUrl, withManager, httpLbs, responseBody)

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

timeline :: String -- ^ Screen name of the user
         -> IO (Either String [Tweet]) -- ^ If there is any error parsing the JSON data, it
                                       --   will return 'Left String', where the 'String'
                                       --   contains the error information.
timeline name = do
  -- Firstly, we create a HTTP request with method GET (it is the default so we don't have to change that).
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
  -- Using a HTTP manager, we authenticate the request and send it to get a response.
  res <- withManager $ \m -> do
           -- OAuth Authentication.
           signedreq <- signOAuth myoauth mycred req
           -- Send request.
           httpLbs signedreq m
  -- Decode the response body.
  return $ eitherDecode $ responseBody res

-- λ> timeline "Hackage"
-- Left "could not parse ISO-8601 date"

main :: IO ()
main = do
  -- Read the timeline from Hackage user.
  ets <- timeline "Hackage"
  case ets of
   -- When the parsing of the JSON data fails, we report it.
   Left err -> putStrLn err
   -- When successful, print in the screen the first 5 tweets.
   Right ts  -> mapM_ print $ take 5 ts

-- λ> main
-- could not parse ISO-8601 date
