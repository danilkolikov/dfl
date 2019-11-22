{- |
Module      :  Frontend.Module.Export.Explicit
Description :  Processor for explicit exports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for processing explicit exports of a module
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Frontend.Module.Export.Explicit
    ( selectExplicitExports
    , ExplicitProcessorError(..)
    ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Module.Base
import Frontend.Module.Explicit
import Frontend.Syntax.Position

-- | Selects explicit exports of a module
selectExplicitExports ::
       NameMapping
    -> F.ImpExpList (WithLocation F.Export)
    -> Explicit
    -> Either ExplicitProcessorError Explicit
selectExplicitExports mapping exports explicit =
    runExplicitProcessor' (selectExplicit exports) explicit mapping

instance SelectsExplicit F.Export where
    selectExplicit export =
        case export of
            F.ExportFunction name -> processFunction name
            F.ExportDataOrClass name components ->
                processDataOrClass name components
            F.ExportModule name -> processModule name

processModule :: WithLocation Ident -> ExplicitProcessor Explicit
processModule name = do
    ExplicitProcessorContext { getExplicitProcessorContextNameMapping = mapping
                             } <- getContext
    let name' = getValue name
        NameMapping { getNameMappingTypes = types
                    , getNameMappingExpressions = expressions
                    , getNameMappingModules = modules
                    } = mapping
    moduleNames <- failIfUnknown name $ HM.lookup name' modules
    let filterNames =
            map (withDummyLocation . head . HS.toList) .
            HM.elems .
            HM.filterWithKey (\k v -> HS.size v == 1 && HS.member k moduleNames)
        exportedTypes = filterNames types
        exportedExpressions = filterNames expressions
    explicitTypes <- mapM (`processDataOrClass` F.ImpExpAll) exportedTypes
    explicitExpressions <- mapM processFunction exportedExpressions
    return . mconcat $ explicitTypes ++ explicitExpressions
