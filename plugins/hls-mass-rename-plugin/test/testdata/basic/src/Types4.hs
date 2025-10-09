{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types4 where

import PrefixedFields (unprefixFields)

-- A helper function that doesn't need transformation
getDefaultString :: String
getDefaultString = "default"

data TypeA = TypeA
  { _fieldA :: Int
  }

data TypeB = TypeB
  { _fieldB :: String
  }

data TypeC = TypeC
  { _fieldC :: Bool
  }

data TypeD = TypeD
  { _fieldD :: Double
  }

unprefixFields ''TypeA
unprefixFields ''TypeB
unprefixFields ''TypeC
unprefixFields ''TypeD
