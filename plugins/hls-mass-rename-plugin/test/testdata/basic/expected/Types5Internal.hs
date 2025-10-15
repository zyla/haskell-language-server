{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Types5Internal where

import PrefixedFields (unprefixFields)

data Account = Account
    { id :: Int
    , accountType :: String
    }
    deriving (Show, Eq)
