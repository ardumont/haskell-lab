{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XDeriveGeneric #-}
module Pizza where

import Data.Text
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Control.Applicative

data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

jsonFile :: FilePath
jsonFile = "../../resources/pizza.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

jsonURL :: String
jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"

getJSONFromUrl :: IO B.ByteString
getJSONFromUrl = simpleHttp jsonURL

main :: IO ()
main = do
  -- Get JSON data and decode it
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
  -- If d is Left, the JSON was malformed.
  -- In that case, we report the error.
  -- Otherwise, we perform the operation of
  -- our choice. In this case, just print it.
  case d of
    Left err -> putStrLn err
    Right ps -> print ps
