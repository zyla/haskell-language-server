module UsePartialImport where

-- MenuSection is imported as type but NOT constructor (..)
-- This allows using record accessors _title, _description
import Types3 (SectionContent (..), getDefaultMenuSection, MenuSection)

-- This demonstrates the original bug: when transformation converts to OverloadedRecordDot,
-- it needs to add MenuSection (..) to the import list. The comma must be added correctly!
useMenuSection :: String
useMenuSection =
    let m = getDefaultMenuSection
    in "Title: " <> _title m <> " - " <> _description m
