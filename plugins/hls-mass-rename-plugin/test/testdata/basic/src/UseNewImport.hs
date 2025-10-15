module UseNewImport where

import Types6Functions (getDefaultWidget)

useWidget :: Int
useWidget =
    let w = getDefaultWidget
    in w.size * 2
