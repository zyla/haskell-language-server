{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types8 where

import PrefixedFields (unprefixFields)

-- Tool has prefixed fields and unprefixFields call
-- This type is in --scan but NOT in --rewrite
-- So unprefixFields call should NOT be removed
data Tool = Tool
  { _brand :: String
  , _model :: String
  }
  deriving (Show, Eq)

unprefixFields ''Tool
