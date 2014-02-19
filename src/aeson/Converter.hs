{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Converter where

import Data.Aeson
import GHC.Generics
import Data.Text
import Network.HTTP.Conduit (simpleHttp)

data Conversion =
  Conversion { to :: !Text      -- target currency
             , rate :: Double   -- rate of conversion
             , from :: !Text    -- origin currency
             , v :: Double      -- converted value in target currency
               } deriving (Show, Generic)

instance FromJSON Conversion
instance ToJSON Conversion

-- | Sample extract from conversion API
-- | full sample url: http://rate-exchange.appspot.com/currency?from=USD&to=EUR&q=1
sampleJSON :: String
sampleJSON = "{ \"to\": \"EUR\", \"rate\": 0.74962518700000003, \"from\": \"USD\", \"v\": 0.74962518700000003}"

-- | Compute the conversion API url
getCurrencyRateUrl :: String -> String -> Double -> String
getCurrencyRateUrl fromCur toCur query = "http://rate-exchange.appspot.com/currency?from=" ++ fromCur ++ "&to=" ++ toCur ++ "&q=" ++ show query

-- | Retrieve the current conversion rate
getConversion :: String -> String -> Double -> IO (Maybe Conversion)
getConversion fromCur toCur query =
  fmap decode $ simpleHttp $ getCurrencyRateUrl fromCur toCur query

-- | Convert a monetary value from one currency to another.
convert :: Double -- ^ Initial quantity.
        -> String -- ^ Initial currency.
        -> String -- ^ Target currency.
        -> IO (Maybe Double) -- ^ Result.
convert query fromCur toCur = fmap (fmap v) $ getConversion fromCur toCur query

-- *Converter> convert 1 "EUR" "USD"
-- Just 1.37601

-- | As an example, we show the conversion from euros to dollars.
--   However, feel free to change the initial quantity q and the
--   origin/target currencies.
main :: IO ()
main = do
  let q = 1
  mr <- convert q "EUR" "USD"
  case mr of
   Nothing -> putStrLn "There was an error reading the JSON data."
   Just r  -> putStrLn $ show q ++ " euro(s) is equivalent to " ++ show r ++ " dollar(s)."
