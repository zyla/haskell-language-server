module UseNewImport where

import Types6Functions (getDefaultWidget)
import Types6 ( Widget(..) )

useWidget :: Int
useWidget =
    let w = getDefaultWidget
    in w.size * 2
