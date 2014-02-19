{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Converter where

import Data.Aeson
import GHC.Generics
import Data.Text
import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy as B

data Conversion =
  Conversion { to :: !Text      -- target currency
             , rate :: Double   -- rate of conversion
             , from :: !Text    -- origin currency
             , v :: Double      -- converted value in target currency
               } deriving (Show, Generic)

instance FromJSON Conversion
instance ToJSON Conversion

sampleJSON :: String
sampleJSON = "{ \"to\": \"EUR\", \"rate\": 0.74962518700000003, \"from\": \"USD\", \"v\": 0.74962518700000003}"

getCurrencyRateUrl :: String -> String -> Double -> String
getCurrencyRateUrl fromCur toCur query = "http://rate-exchange.appspot.com/currency?from=" ++ fromCur ++ "&to=" ++ toCur ++ "&q=" ++ show query

getConversion :: String -> String -> Double -> IO (Maybe Conversion)
getConversion fromCur toCur query =
  fmap decode $ simpleHttp $ getCurrencyRateUrl fromCur toCur query

-- | Convert a monetary value from one currency to another.
convert :: Double -- ^ Initial quantity.
        -> String -- ^ Initial currency.
        -> String -- ^ Target currency.
        -> IO (Maybe Double) -- ^ Result.
convert q fromCur toCur = fmap (fmap v) $ getConversion fromCur toCur q
