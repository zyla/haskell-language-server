{-# OPTIONS_GHC -Wno-unused-imports #-}
module UseNewImport where

import Types6Functions (getDefaultWidget)

-- unrelated module for added confusion
import Types1

useWidget :: Int
useWidget =
    let w = getDefaultWidget
    in w.size * 2
