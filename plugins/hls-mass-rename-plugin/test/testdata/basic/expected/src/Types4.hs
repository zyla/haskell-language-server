{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types4 where

import PrefixedFields (unprefixFields)

-- A helper function that doesn't need transformation
getDefaultString :: String
getDefaultString = "default"

data TypeA = TypeA
  { fieldA :: Int
  }

data TypeB = TypeB
  { fieldB :: String
  }

data TypeC = TypeC
  { fieldC :: Bool
  }

data TypeD = TypeD
  { fieldD :: Double
  }
