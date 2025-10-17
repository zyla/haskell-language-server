{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module UseSelector where

import Data.Text (Text)
import qualified Data.Text as T

import Types1

-- NOTE: `id` doesn't work, because it creates conflict with a Prelude import
-- use_selector_id :: Restaurant -> Text
-- use_selector_id r = "R:" <> T.pack (show (_id r))

use_selector :: Restaurant -> Text
use_selector r = "R:" <> T.pack (show (_slug r)) <> _name r

-- NOTE: doesn't work, creates shadowing conflict
-- use_selector_shadow :: Restaurant -> Text
-- use_selector_shadow r = let name = "foo" in _name r <> name

-- Standalone field selector usage (should transform to (.field) syntax)
getNames :: [Restaurant] -> [Text]
getNames = map _name

getSlugs :: [Restaurant] -> [Text]
getSlugs restaurants = map _slug restaurants
