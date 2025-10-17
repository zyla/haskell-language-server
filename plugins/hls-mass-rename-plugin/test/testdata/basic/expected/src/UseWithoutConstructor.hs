{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module UseWithoutConstructor where

import Data.Text (Text)
import qualified Data.Text as T
import Types1 (Restaurant(..))
import Types2 (Account(..))

useRestaurantDot :: Restaurant -> Text
useRestaurantDot r = "R:" <> T.pack (show r.id) <> r.name

useAccountDot :: Account -> Text
useAccountDot a = "A:" <> T.pack (show a.id) <> a.name

-- Multiple field accesses from same type
multipleAccesses :: Restaurant -> Text
multipleAccesses r = r.name <> r.slug

-- Nested field access
nestedAccess :: [Restaurant] -> Text
nestedAccess rs = T.intercalate "," (map (.name) rs)
