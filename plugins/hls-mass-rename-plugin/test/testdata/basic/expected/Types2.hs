{-# LANGUAGE DuplicateRecordFields #-}

module Types2 where

import Data.Text (Text)

data Account = Account
  { id :: Int
  , name :: Text
  }
