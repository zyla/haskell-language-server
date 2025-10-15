{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types6 where

import PrefixedFields (unprefixFields)

data Widget = Widget
  { _size :: Int
  , _color :: String
  }
  deriving (Show, Eq)

unprefixFields ''Widget
