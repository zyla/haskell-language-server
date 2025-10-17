module Types7Functions (getDefaultGadget) where

import Types7 (Gadget(..))

-- Export the function but NOT the Gadget type
getDefaultGadget :: Gadget
getDefaultGadget = Gadget 100 "steel"
