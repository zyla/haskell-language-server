{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types1 where

import Data.Text (Text)
import PrefixedFields (unprefixFields)

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

useInSameModuleDot :: Restaurant -> Text
useInSameModuleDot r = "R:" <> r.name
