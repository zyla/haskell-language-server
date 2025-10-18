{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types9 where

import Data.Text (Text)
import PrefixedFields (unprefixFields)

-- Newtype with a single prefixed field
newtype UserId = UserId
  { _unUserId :: Int
  }

-- Another newtype with a different field
newtype AccountName = AccountName
  { _unAccountName :: Text
  }

-- Test usage in same module
getUserId :: UserId -> Int
getUserId UserId{_unUserId} = _unUserId

getAccountName :: AccountName -> Text
getAccountName acc = acc._unAccountName

unprefixFields ''UserId
unprefixFields ''AccountName

-- Usage with dot syntax after unprefixFields
getUserIdDot :: UserId -> Int
getUserIdDot uid = uid.unUserId
