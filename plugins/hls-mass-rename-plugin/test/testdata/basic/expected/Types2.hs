{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types2 where

import Data.Text (Text)
import PrefixedFields (unprefixFields)

data Account = Account
  { _id :: Int
  , _name :: Text
  }

unprefixFields ''Account
