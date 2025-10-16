{-# LANGUAGE DuplicateRecordFields #-}

module Types7 where

-- Gadget has unprefixed fields - NOT being renamed
data Gadget = Gadget
  { weight :: Int
  , material :: String
  }
  deriving (Show, Eq)
