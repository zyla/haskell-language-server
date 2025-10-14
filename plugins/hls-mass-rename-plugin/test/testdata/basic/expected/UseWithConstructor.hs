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
useRestaurantDot r = "R:" <> T.pack (show r.id) <> r.name

useAccountDot :: Account -> Text
useAccountDot a = "A:" <> T.pack (show a.id) <> a.name
