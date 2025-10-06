{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module UseWithConstructor where

import Data.Text (Text)
import qualified Data.Text as T
-- Constructor already imported - should not change
import Types1 (Restaurant(..))
import Types2 (Account(..))

-- These functions use OverloadedRecordDot with constructor already imported
useRestaurantDot :: Restaurant -> Text
useRestaurantDot r = "R:" <> T.pack (show r._id) <> r._name

useAccountDot :: Account -> Text
useAccountDot a = "A:" <> T.pack (show a._id) <> a._name
