{-# LANGUAGE DuplicateRecordFields #-}

module Types1 where

import Data.Text (Text)

data Restaurant = Restaurant
  { id :: Int
  , name :: Text
  , slug :: Text
  }

data FulfillmentMethod
  = Delivery
    { address :: Text
    , price :: Int
    }
  | DineIn
    { table :: Int
    }

useInSameModule :: Restaurant -> Text
useInSameModule Restaurant{name} = "R:" <> name
