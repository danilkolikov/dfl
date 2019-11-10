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

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Module.Base
import Frontend.Module.Explicit
import Frontend.Syntax.Position

-- | Selects explicit exports of a module
selectExplicitExports ::
       F.ImpExpList (WithLocation F.Export)
    -> Explicit
    -> Either ExplicitProcessorError Explicit
selectExplicitExports exports = runExplicitProcessor (selectExplicit exports)

instance SelectsExplicit F.Export where
    selectExplicit export =
        case export of
            F.ExportFunction name -> processFunction name
            F.ExportDataOrClass name components ->
                processDataOrClass name components
            F.ExportModule name -> processExplicit name

processExplicit :: WithLocation Ident -> ExplicitProcessor Explicit
processExplicit _ = return mempty -- TODO: support export of modules
