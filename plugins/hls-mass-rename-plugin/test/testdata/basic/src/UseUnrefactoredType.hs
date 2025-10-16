module UseUnrefactoredType where

import Types6Functions (getDefaultWidget)
import Types7Functions (getDefaultGadget)
import Types7 (weight)

-- Use both Widget (being renamed) and Gadget (NOT being renamed)
useBoth :: Int
useBoth =
    let w = getDefaultWidget
        g = getDefaultGadget
    in w.size + weight g
