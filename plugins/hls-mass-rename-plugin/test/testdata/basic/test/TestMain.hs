{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T

-- Import types from library (cross-component)
import Types1
import Types2

-- Test helpers using library types with prefixed fields

-- Use NamedFieldPuns with Restaurant
testRestaurant :: Restaurant -> Text
testRestaurant Restaurant{_name, _id} =
  "Restaurant #" <> T.pack (show _id) <> ": " <> _name

-- Use RecordWildCards with Restaurant
describeRestaurant :: Restaurant -> Text
describeRestaurant Restaurant{..} =
  "ID=" <> T.pack (show _id) <> ", Name=" <> _name <> ", Slug=" <> _slug

-- Use pattern matching to extract field
getRestaurantId :: Restaurant -> Int
getRestaurantId Restaurant{_id} = _id

-- Use OverloadedRecordDot
getRestaurantNameDot :: Restaurant -> Text
getRestaurantNameDot r = r._name

-- Update restaurant using prefixed fields
renameRestaurant :: Text -> Restaurant -> Restaurant
renameRestaurant newName r = r { _name = newName, _slug = "new_" <> newName }

-- Pattern match on FulfillmentMethod
describeFulfillment :: FulfillmentMethod -> Text
describeFulfillment = \case
  Delivery{_address, _price} ->
    "Delivery to " <> _address <> " ($" <> T.pack (show _price) <> ")"
  DineIn{_table} ->
    "Dine-in at table " <> T.pack (show _table)

-- Use Account from Types2
testAccount :: Account -> Text
testAccount Account{_id, _name} =
  "Account #" <> T.pack (show _id) <> ": " <> _name

-- Multiple record updates
updateRestaurantDetails :: Int -> Text -> Text -> Restaurant -> Restaurant
updateRestaurantDetails newId newName newSlug r =
  r { _id = newId, _name = newName, _slug = newSlug }

-- Main function (required for test-suite)
main :: IO ()
main = putStrLn "Test helpers compiled successfully"
