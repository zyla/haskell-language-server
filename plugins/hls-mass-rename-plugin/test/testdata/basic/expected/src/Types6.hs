{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types6 where

import PrefixedFields (unprefixFields)

data Widget = Widget
  { size :: Int
  , color :: String
  }
  deriving (Show, Eq)
