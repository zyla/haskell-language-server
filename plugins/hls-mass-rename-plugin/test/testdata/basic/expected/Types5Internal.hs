{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Types5Internal where

data Account = Account
    { id :: Int
    , accountType :: String
    }
    deriving (Show, Eq)
