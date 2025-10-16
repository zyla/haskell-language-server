{-# OPTIONS_GHC -Wno-unused-imports #-}
module FieldProjection where

import Types6Functions (getAllWidgets)
import Types6 ( Widget(..) )

-- Use field projection operator section
getSizes :: [Int]
getSizes = map (.size) getAllWidgets
