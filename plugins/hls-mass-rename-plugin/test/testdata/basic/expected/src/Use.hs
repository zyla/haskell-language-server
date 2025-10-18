{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Use where

import Data.Text (Text)
import qualified Data.Text as T

import Types1
import Types2
import Types9

use :: Restaurant -> Text
use Restaurant{name = nm, slug = s} = "R:" <> nm <> s

use_NamedFieldPuns :: Restaurant -> Text
use_NamedFieldPuns Restaurant{name} = "R:" <> name

use_RecordWildCards :: Restaurant -> Text
use_RecordWildCards Restaurant{..} = "R:" <> T.pack (show id) <> name

use_OverloadedRecordDot :: Restaurant -> Text
use_OverloadedRecordDot r = "R:" <> T.pack (show r.id) <> r.name

use_OverloadedRecordDot_simple :: Restaurant -> Text
use_OverloadedRecordDot_simple restaurant = restaurant.name

use_construct :: Text -> Restaurant
use_construct s = Restaurant
  { id = 666
  , name = s
  , slug = s
  }

use_update :: Text -> Restaurant -> Restaurant
use_update s r = r { name = s }

use_update_qualified :: Text -> Restaurant -> Restaurant
use_update_qualified s r = r { Types1.name = s }

use_update_multiple :: Text -> Restaurant -> Restaurant
use_update_multiple s r = r { name = s, slug = "slug_" <> s }

use_construct_scopeConflict_benign :: Int -> Text -> Text -> Restaurant
use_construct_scopeConflict_benign id name slug = Restaurant
  { id = id
  , name = name
  , slug = slug
  }

use_update_scopeConflict_benign :: Text -> Restaurant -> Restaurant
use_update_scopeConflict_benign name r = r { name = name }

use_variant :: FulfillmentMethod -> Text
use_variant = \case
  Delivery
    { address
    , price = p
    } -> "D" <> address <> T.pack (show p)
  DineIn { .. } -> "DI" <> T.pack (show table)

useAccount :: Account -> Text
useAccount Account{id = id} = "A" <> T.pack (show id)

-- NOTE: doesn't work, creates conflict
-- use_NamedFieldPuns_scopeConflict :: Restaurant -> Text -> Text
-- use_NamedFieldPuns_scopeConflict Restaurant{_name} name = _name <> name

use_NamedFieldPuns_shadow :: Restaurant -> Text
use_NamedFieldPuns_shadow Restaurant{name} = let name = "foo" in name <> name

use_NamedFieldPuns_shadow2 :: Restaurant -> Text
use_NamedFieldPuns_shadow2 =
    let name = "foo"
    in \Restaurant{name} -> name <> name

-- Newtype usage tests
use_newtype_pattern :: UserId -> Int
use_newtype_pattern UserId{unUserId} = unUserId

use_newtype_dot :: UserId -> Int
use_newtype_dot uid = uid.unUserId

use_newtype_construct :: Int -> UserId
use_newtype_construct n = UserId { unUserId = n }

use_newtype_update :: Int -> UserId -> UserId
use_newtype_update n uid = uid { unUserId = n }

use_accountname_dot :: AccountName -> Text
use_accountname_dot acc = acc.unAccountName
