{-# OPTIONS_GHC -Wno-unused-imports #-}
module FieldProjection where

import Types6Functions (getAllWidgets)

-- Use field projection operator section
getSizes :: [Int]
getSizes = map (._size) getAllWidgets
