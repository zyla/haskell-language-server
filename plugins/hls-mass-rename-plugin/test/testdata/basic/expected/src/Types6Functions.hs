module Types6Functions (getDefaultWidget, getAllWidgets) where

import Types6 (Widget(..))

-- Export the function but NOT the Widget type
getDefaultWidget :: Widget
getDefaultWidget = Widget 42 "blue"

getAllWidgets :: [Widget]
getAllWidgets = [Widget 10 "red", Widget 20 "blue", Widget 30 "green"]
