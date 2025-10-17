{-# LANGUAGE OverloadedRecordDot #-}

module UseReexport where

import qualified Data.Text as T
import Data.Text (Text)
import Types5 (Account(..))

-- Uses field access on re-exported Account type
-- Account is defined in Types5Internal but imported via Types5 re-export
useAccountDot :: Account -> Text
useAccountDot account = "Account #" <> T.pack (show account.id) <> ": " <> T.pack account.accountType
