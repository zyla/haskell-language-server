{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types2 where

import Data.Text (Text)
import PrefixedFields (unprefixFields)

data Account = Account
  { id :: Int
  , name :: Text
  }

unprefixFields ''Account
