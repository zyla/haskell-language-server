{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Use where

import Data.Text (Text)
import qualified Data.Text as T

import Types1
import Types2

use :: Restaurant -> Text
use Restaurant{_name = nm, _slug = s} = "R:" <> nm <> s

use_NamedFieldPuns :: Restaurant -> Text
use_NamedFieldPuns Restaurant{_name} = "R:" <> _name

use_RecordWildCards :: Restaurant -> Text
use_RecordWildCards Restaurant{..} = "R:" <> T.pack (show _id) <> _name

use_OverloadedRecordDot :: Restaurant -> Text
use_OverloadedRecordDot r = "R:" <> T.pack (show r._id) <> r._name

use_OverloadedRecordDot_simple :: Restaurant -> Text
use_OverloadedRecordDot_simple restaurant = restaurant._name

use_construct :: Text -> Restaurant
use_construct s = Restaurant
  { _id = 666
  , _name = s
  , _slug = s
  }

use_update :: Text -> Restaurant -> Restaurant
use_update s r = r { _name = s }

use_update_qualified :: Text -> Restaurant -> Restaurant
use_update_qualified s r = r { Types1._name = s }

use_update_multiple :: Text -> Restaurant -> Restaurant
use_update_multiple s r = r { _name = s, _slug = "slug_" <> s }

use_construct_scopeConflict_benign :: Int -> Text -> Text -> Restaurant
use_construct_scopeConflict_benign id name slug = Restaurant
  { _id = id
  , _name = name
  , _slug = slug
  }

use_update_scopeConflict_benign :: Text -> Restaurant -> Restaurant
use_update_scopeConflict_benign name r = r { _name = name }

use_variant :: FulfillmentMethod -> Text
use_variant = \case
  Delivery
    { _address
    , _price = p
    } -> "D" <> _address <> T.pack (show p)
  DineIn { .. } -> "DI" <> T.pack (show _table)

useAccount :: Account -> Text
useAccount Account{_id = id} = "A" <> T.pack (show id)

use_NamedFieldPuns_scopeConflict :: Restaurant -> Text -> Text
use_NamedFieldPuns_scopeConflict Restaurant{_name} name = _name <> name

use_NamedFieldPuns_shadow :: Restaurant -> Text
use_NamedFieldPuns_shadow Restaurant{_name} = let name = "foo" in _name <> name

use_NamedFieldPuns_shadow2 :: Restaurant -> Text
use_NamedFieldPuns_shadow2 =
    let name = "foo"
    in \Restaurant{_name} -> _name <> name
