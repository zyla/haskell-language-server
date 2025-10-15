module Types6Functions (getDefaultWidget) where

import Types6 (Widget(..))

-- Export the function but NOT the Widget type
getDefaultWidget :: Widget
getDefaultWidget = Widget 42 "blue"
