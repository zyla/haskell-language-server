{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Types5Internal where

data Account = Account
    { _id :: Int
    , _accountType :: String
    }
    deriving (Show, Eq)
