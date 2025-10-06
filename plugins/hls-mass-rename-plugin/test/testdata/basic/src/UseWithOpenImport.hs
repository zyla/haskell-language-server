{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module UseWithOpenImport where

import Data.Text (Text)
import qualified Data.Text as T
-- Open import - everything is already accessible, should not change
import Types1
import Types2

-- These functions use OverloadedRecordDot with open imports
useRestaurantDot :: Restaurant -> Text
useRestaurantDot r = "R:" <> T.pack (show r._id) <> r._name

useAccountDot :: Account -> Text
useAccountDot a = "A:" <> T.pack (show a._id) <> a._name
