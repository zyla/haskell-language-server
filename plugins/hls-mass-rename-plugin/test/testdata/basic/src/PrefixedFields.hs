{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module PrefixedFields where

import Prelude

import Control.Monad
import GHC.Records
import "template-haskell" Language.Haskell.TH

{- | Derive HasField instances for a record type with lens-prefixed field names (with `_` prefix).
 The field names for instances will be without prefix.

 Based on <https://github.com/trskop/overloaded-records/blob/master/src/Data/OverloadedRecords/TH/Internal.hs>
-}
unprefixFields :: Name -> DecsQ
unprefixFields name = do
  dt <- reify name
  case dt of
    TyConI dec -> case dec of
      -- Not supporting DatatypeContexts, hence the [] required as the first
      -- argument to NewtypeD and DataD.
      NewtypeD [] typeName typeVars _kindSignature constructor _deriving ->
        deriveForConstructor name typeVars constructor
      DataD [] typeName typeVars _kindSignature [] _deriving ->
        pure []
      DataD [] typeName typeVars _kindSignature [constructor] _deriving ->
        deriveForConstructor name typeVars constructor
      DataD [] typeName typeVars _kindSignature _constructors _deriving ->
        fail $ show name <> " has multiple constructors"
      x -> canNotDeriveError name x
    x -> canNotDeriveError name x
  where
    canNotDeriveError :: (Show a) => Name -> a -> Q b
    canNotDeriveError = (fail .) . errMessage

    errMessage :: (Show a) => Name -> a -> String
    errMessage n x =
      "`" <> show n <> "' is neither newtype nor data type: " <> show x

deriveForConstructor ::
  Name ->
  [TyVarBndr ()] ->
  Con ->
  DecsQ
deriveForConstructor typeName typeVars = \case
  RecC constructorName args ->
    fmap concat $ forM args $ \(fieldName, _, fieldType) ->
      let label = stripLensPrefix $ nameBase fieldName
       in [d|
            instance HasField $(litT $ strTyLit label) $(pure $ foldl AppT (ConT typeName) (map tvToType typeVars)) $(pure fieldType) where
              getField = $(pure (ProjectionE (pure (nameBase fieldName))))
            |]
  _ -> pure []
  where
    tvToType = \case
      PlainTV name _ -> VarT name
      KindedTV name _ _ -> VarT name

stripLensPrefix :: String -> String
stripLensPrefix ('_' : xs) = xs
stripLensPrefix xs = xs
