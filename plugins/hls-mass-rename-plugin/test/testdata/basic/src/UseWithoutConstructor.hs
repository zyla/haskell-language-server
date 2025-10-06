{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module UseWithoutConstructor where

import Data.Text (Text)
import qualified Data.Text as T
-- Import type but NOT constructor - this should be fixed
import Types1 (Restaurant)
import Types2 (Account)

-- These functions use OverloadedRecordDot but constructor isn't imported
-- After rename, the import should be updated to Restaurant(..)
useRestaurantDot :: Restaurant -> Text
useRestaurantDot r = "R:" <> T.pack (show r._id) <> r._name

useAccountDot :: Account -> Text
useAccountDot a = "A:" <> T.pack (show a._id) <> a._name

-- Multiple field accesses from same type
multipleAccesses :: Restaurant -> Text
multipleAccesses r = r._name <> r._slug

-- Nested field access
nestedAccess :: [Restaurant] -> Text
nestedAccess rs = T.intercalate "," (map (._name) rs)
