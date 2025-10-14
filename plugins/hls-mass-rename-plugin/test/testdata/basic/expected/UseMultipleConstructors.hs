module UseMultipleConstructors where

import Types4 (getDefaultString)
import qualified Types4

-- Test case for multiple constructor imports being ADDED at once
-- getDefaultString is already imported (a value, not transformed)
-- TypeB, TypeC, TypeD are used via qualified access (not imported yet)
-- Field accesses will trigger ADDING TypeB(..), TypeC(..), TypeD(..) imports
-- This tests that commas are properly added between multiple new (..) imports
useAll :: Types4.TypeB -> Types4.TypeC -> Types4.TypeD -> String
useAll b c d =
    getDefaultString <> ": " <> b.fieldB <> show c.fieldC <> show d.fieldD
