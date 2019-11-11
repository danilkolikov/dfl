{- |
Module      :  Frontend.Module.Import.Processor
Description :  Processor for imports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for processing module imports
-}
module Frontend.Module.Import.Processor
    ( module Frontend.Module.Base
    , processImports
    , ImportProcessorError(..)
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Module.Base
import Frontend.Module.Implicit
import Frontend.Module.Import.NameMapping
import Frontend.Module.Import.Selecting
import Frontend.Syntax.Position

-- | Errors which can be encountered during import prococessing
data ImportProcessorError
    = ImportProcessorErrorUnknownModule (WithLocation UserDefinedIdent)
    | ImportProcessorErrorExplicit ExplicitProcessorError
    deriving (Eq, Show)

-- | A type of the import processor
type ImportProcessor = ReaderT DefinedModules (Except ImportProcessorError)

-- | Processes imports of a module
processImports ::
       DefinedModules -> I.Header -> Either ImportProcessorError ModuleImports
processImports defined header =
    runExcept $ runReaderT (processImports' header) defined

processImports' :: I.Header -> ImportProcessor ModuleImports
processImports' (I.Header _ _ imports) =
    mconcat <$> mapM (processSingleImport . getValue) imports

processSingleImport :: I.ImpDecl -> ImportProcessor ModuleImports
processSingleImport (I.ImpDecl isQualified moduleName altName isHiding imports) = do
    context <- ask
    let maybeExport = HM.lookup (getValue moduleName) context
    ModuleExports { getModuleExportsExplicit = explicitExport
                  , getModuleExportsImplicit = implicitExport
                  , getModuleExportsInstances = instances
                  } <-
        case maybeExport of
            Nothing ->
                lift . throwE $ ImportProcessorErrorUnknownModule moduleName
            Just e -> return e
    explicitImport <-
        lift . except . first ImportProcessorErrorExplicit $
        selectExplicitImports explicitExport isHiding imports
    let implicitImport =
            selectImplicitImports implicitExport instances explicitImport
        nameMapping =
            createNameMapping isQualified moduleName altName explicitImport
    return
        ModuleImports
            { getModuleImportsExplicit = explicitImport
            , getModuleImportsImplicit = implicitImport
            , getModuleImportsInstances = instances
            , getModuleImportsNameMapping = nameMapping
            }

selectImplicitImports :: Implicit -> Instances -> Explicit -> Implicit
selectImplicitImports implicit instances explicit
    | Implicit {getImplicitTypeConstructors = typeConstructors} <- implicit =
        selectImplicit explicit instances typeConstructors
