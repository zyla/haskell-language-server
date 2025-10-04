{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module UseSelector where

import Data.Text (Text)
import qualified Data.Text as T

import Types1

use_selector :: Restaurant -> Text
use_selector r = "R:" <> T.pack (show (_id r)) <> _name r

use_selector_shadow :: Restaurant -> Text
use_selector_shadow r = let name = "foo" in _name r <> name
