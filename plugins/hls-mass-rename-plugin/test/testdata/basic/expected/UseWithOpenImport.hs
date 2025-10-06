{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module UseWithOpenImport where

import Data.Text (Text)
import qualified Data.Text as T
-- Open import - unchanged
import Types1
import Types2

-- These functions use OverloadedRecordDot with open imports
useRestaurantDot :: Restaurant -> Text
useRestaurantDot r = "R:" <> T.pack (show r.id) <> r.name

useAccountDot :: Account -> Text
useAccountDot a = "A:" <> T.pack (show a.id) <> a.name
