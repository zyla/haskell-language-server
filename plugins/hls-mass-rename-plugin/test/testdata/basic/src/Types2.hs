{-# LANGUAGE DuplicateRecordFields #-}

module Types2 where

import Data.Text (Text)

data Account = Account
  { _id :: Int
  , _name :: Text
  }
