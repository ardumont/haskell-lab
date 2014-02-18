{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XDeriveGeneric #-}
module Aeson where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

data Person =
  Person { firstName  :: !T.Text
         , lastName   :: !T.Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person

jsonFile :: FilePath
jsonFile = "../resources/pizza.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

jsonURL :: String
jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"

getJSONFromUrl :: IO B.ByteString
getJSONFromUrl = simpleHttp jsonURL
