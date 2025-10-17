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
testRestaurant Restaurant{name, id} =
  "Restaurant #" <> T.pack (show id) <> ": " <> name

-- Use RecordWildCards with Restaurant
describeRestaurant :: Restaurant -> Text
describeRestaurant Restaurant{..} =
  "ID=" <> T.pack (show id) <> ", Name=" <> name <> ", Slug=" <> slug

-- Use pattern matching to extract field
getRestaurantId :: Restaurant -> Int
getRestaurantId Restaurant{id} = id

-- Use OverloadedRecordDot
getRestaurantNameDot :: Restaurant -> Text
getRestaurantNameDot r = r.name

-- Update restaurant using prefixed fields
renameRestaurant :: Text -> Restaurant -> Restaurant
renameRestaurant newName r = r { name = newName, slug = "new_" <> newName }

-- Pattern match on FulfillmentMethod
describeFulfillment :: FulfillmentMethod -> Text
describeFulfillment = \case
  Delivery{address, price} ->
    "Delivery to " <> address <> " ($" <> T.pack (show price) <> ")"
  DineIn{table} ->
    "Dine-in at table " <> T.pack (show table)

-- Use Account from Types2
testAccount :: Account -> Text
testAccount Account{id, name} =
  "Account #" <> T.pack (show id) <> ": " <> name

-- Multiple record updates
updateRestaurantDetails :: Int -> Text -> Text -> Restaurant -> Restaurant
updateRestaurantDetails newId newName newSlug r =
  r { id = newId, name = newName, slug = newSlug }

-- Main function (required for test-suite)
main :: IO ()
main = putStrLn "Test helpers compiled successfully"
