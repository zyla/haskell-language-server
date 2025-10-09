{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell#-}

module Types3 where

import PrefixedFields (unprefixFields)

data SectionContent = SectionContent
  { _items :: [String]
  }
  deriving (Show, Eq)

data MenuSection = MenuSection
  { _title :: String
  , _description :: String
  }
  deriving (Show, Eq)

data MenuItem = MenuItem
  { _label :: String
  , _price :: Int
  }
  deriving (Show, Eq)

-- Function that returns MenuSection without requiring it to be imported
getDefaultMenuSection :: MenuSection
getDefaultMenuSection = MenuSection "Default" "Default menu section"
