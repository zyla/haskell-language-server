module UsePartialImport where

import Types3 (getDefaultMenuSection)

useMenuSection :: String
useMenuSection =
    let m = getDefaultMenuSection
    in "Title: " <> m.title <> " - " <> m.description
