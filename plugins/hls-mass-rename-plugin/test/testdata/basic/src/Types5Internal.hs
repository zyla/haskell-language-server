{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module Types5Internal where

import PrefixedFields (unprefixFields)

data Account = Account
    { _id :: Int
    , _accountType :: String
    }
    deriving (Show, Eq)

unprefixFields ''Account
