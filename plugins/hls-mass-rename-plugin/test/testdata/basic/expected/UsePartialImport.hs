module UsePartialImport where

-- After transformation: MenuSection (..) is added with proper comma
import Types3 (SectionContent (..), getDefaultMenuSection, MenuSection (..))

useMenuSection :: String
useMenuSection =
    let m = getDefaultMenuSection
    in "Title: " <> m.title <> " - " <> m.description
