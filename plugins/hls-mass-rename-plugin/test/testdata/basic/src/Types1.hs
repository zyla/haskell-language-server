{-# LANGUAGE DuplicateRecordFields #-}

module Types1 where

import Data.Text (Text)

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
useInSameModule Restaurant{_name} = "R:" <> _name
