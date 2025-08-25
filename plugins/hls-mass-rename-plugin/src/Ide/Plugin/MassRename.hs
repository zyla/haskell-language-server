{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ide.Plugin.MassRename (descriptor, E.Log) where

import           Control.Lens                          ((^.))
import           Control.Monad
import           Control.Monad.Except                  (ExceptT, throwError)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Trans.Class             (lift)
import           Data.Either                           (rights)
import           Data.Foldable                         (fold)
import           Data.Generics
import           Data.Hashable
import           Data.HashSet                          (HashSet)
import qualified Data.HashSet                          as HS
import           Data.List.NonEmpty                    (NonEmpty ((:|)),
                                                        groupWith)
import qualified Data.Map                              as M
import           Data.Maybe
import           Data.Mod.Word
import qualified Data.Text                             as T
import           Development.IDE                       (Recorder, WithPriority,
                                                        usePropertyAction)
import           Development.IDE.Core.FileStore        (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.ExactPrint
import qualified Development.IDE.GHC.ExactPrint        as E
import           Development.IDE.Plugin.CodeAction
import           Development.IDE.Spans.AtPoint
import           Development.IDE.Types.Location
import           GHC.Iface.Ext.Types                   (HieAST (..),
                                                        HieASTs (..),
                                                        NodeOrigin (..),
                                                        SourcedNodeInfo (..))
import           GHC.Iface.Ext.Utils                   (generateReferencesMap)
import           HieDb                                 ((:.) (..))
import           HieDb.Query
import           HieDb.Types                           (RefRow (refIsGenerated))
import           Ide.Plugin.Error
import           Ide.Plugin.Properties
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens            as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Options.Applicative        (ParserInfo, info)

instance Hashable (Mod a) where hash n = hash (unMod n)

descriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = mkExactprintPluginDescriptor recorder $
    (defaultPluginDescriptor pluginId "Rename all fields of a record")
        { pluginCli = Just exampleCli
        }


exampleCli :: ParserInfo (IdeCommand IdeState)
exampleCli = info p mempty
  where p = pure $ IdeCommand $ \_ideState -> putStrLn "hello HLS"
