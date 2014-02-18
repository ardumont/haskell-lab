{-# LANGUAGE OverloadedStrings #-}
module Aeson where

import qualified Data.Text as T
import Data.Aeson
import Control.Applicative
import Control.Monad (mzero)

data Person =
  Person { firstName  :: !T.Text
         , lastName   :: !T.Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving Show

instance FromJSON Person where
 parseJSON (Object v) =
    Person <$> v .: "firstName"
           <*> v .: "lastName"
           <*> v .: "age"
           <*> v .: "likesPizza"
 parseJSON _ = mzero

instance ToJSON Person where
 toJSON (Person firstName lastName age likesPizza) =
    object [ "firstName"  .= firstName
           , "lastName"   .= lastName
           , "age"        .= age
           , "likesPizza" .= likesPizza
             ]
