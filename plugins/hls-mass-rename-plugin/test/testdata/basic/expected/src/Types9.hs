{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types9 where

import Data.Text (Text)
import PrefixedFields (unprefixFields)

-- Newtype with a single prefixed field
newtype UserId = UserId
  { unUserId :: Int
  }

-- Another newtype with a different field
newtype AccountName = AccountName
  { unAccountName :: Text
  }

-- Test usage in same module
getUserId :: UserId -> Int
getUserId UserId{unUserId} = unUserId

getAccountName :: AccountName -> Text
getAccountName acc = acc.unAccountName

-- Usage with dot syntax after unprefixFields
getUserIdDot :: UserId -> Int
getUserIdDot uid = uid.unUserId
