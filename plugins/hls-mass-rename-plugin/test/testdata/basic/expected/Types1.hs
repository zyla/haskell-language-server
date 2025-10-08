{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types1 where

import Data.Text (Text)
import PrefixedFields (unprefixFields)

data Restaurant = Restaurant
  { _id :: Int
  , _name :: Text
  , _slug :: Text
  }

data FulfillmentMethod
  = Delivery
    { _address :: Text
    , _price :: Int
    }
  | DineIn
    { _table :: Int
    }

useInSameModule :: Restaurant -> Text
useInSameModule Restaurant{name} = "R:" <> name

unprefixFields ''Restaurant
