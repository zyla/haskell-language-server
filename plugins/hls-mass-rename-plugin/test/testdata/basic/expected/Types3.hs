{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell#-}

module Types3 where

import PrefixedFields (unprefixFields)

data SectionContent = SectionContent
  { items :: [String]
  }
  deriving (Show, Eq)

data MenuSection = MenuSection
  { title :: String
  , description :: String
  }
  deriving (Show, Eq)

data MenuItem = MenuItem
  { label :: String
  , price :: Int
  }
  deriving (Show, Eq)

-- Function that returns MenuSection without requiring it to be imported
getDefaultMenuSection :: MenuSection
getDefaultMenuSection = MenuSection "Default" "Default menu section"
