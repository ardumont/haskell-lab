{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -XDeriveGeneric #-}
module Aeson where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

data Person =
  Person { firstName  :: !T.Text
         , lastName   :: !T.Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show, Generic)

instance FromJSON Person
instance ToJSON Person
