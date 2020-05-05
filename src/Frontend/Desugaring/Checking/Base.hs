{- |
Module      :  Frontend.Desugaring.Checking.Base
Description :  Base functions for disambiguation
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Type for objects which check expressions for ambiguity.
-}
module Frontend.Desugaring.Checking.Base where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe, maybeToList)

import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Base
    ( DefinedNames
    , GroupingProcessorState(..)
    , makeQualifiedName
    )
import Frontend.Syntax.Position (WithLocation(..))
import Util.Debug

-- | Errors which may happen during desugaring
data CheckingError
    = CheckingErrorUndefinedName (WithLocation Ident) -- ^ Undefined name
    | CheckingErrorAmbiguousName (WithLocation Ident)
                                 [AmbiguitySource] -- ^ A name has multiple possible meanings
    deriving (Show, Eq)

-- | A source of an ambgiously defined name
data AmbiguitySource
    = AmbiguitySourceThisFile (WithLocation Ident) -- ^ Definition in this file
    | AmbiguitySourceImport (WithLocation Ident) -- ^ Imported definition
    deriving (Eq, Show)

-- | A map of imported names
type ImportedNames = HM.HashMap Ident [WithLocation Ident]

-- | Groups of imported definitions
data ImportedGroups = ImportedGroups
    { getImportedGroupsTypes :: ImportedNames -- ^ Imported type names (data types, new types, type synonyms)
    , getImportedGroupsExpressions :: ImportedNames -- ^ Imported functions
    , getImportedGroupsModules :: HS.HashSet Ident -- ^ Imported modules
    } deriving (Eq, Show)

instance Semigroup ImportedGroups where
    ImportedGroups t1 e1 m1 <> ImportedGroups t2 e2 m2 =
        ImportedGroups (t1 <> t2) (e1 <> e2) (m1 <> m2)

instance Monoid ImportedGroups where
    mempty = ImportedGroups mempty mempty mempty

-- | An environment of checking processor
data CheckingProcessorEnvironment = CheckingProcessorEnvironment
    { getCheckingProcessorEnvironmentModuleName :: Ident
    , getCheckingProcessorEnvironmentImports :: ImportedGroups
    , getCheckingProcessorEnvironmentTopLevel :: GroupingProcessorState
    , getCheckingProcessorEnvironmentLocalDefinitions :: HS.HashSet Ident
    }

-- | Sets local names
setLocalNames ::
       HS.HashSet Ident
    -> CheckingProcessorEnvironment
    -> CheckingProcessorEnvironment
setLocalNames names env
    | CheckingProcessorEnvironment --
      {getCheckingProcessorEnvironmentLocalDefinitions = locals} <- env =
        env {getCheckingProcessorEnvironmentLocalDefinitions = locals <> names}

-- | A debug output of checking
data CheckingDebugOutput = CheckingDebugOutput
    { getCheckingDebugOutputExports :: Maybe Exports
    , getCheckingDebugOutputTypeSynonyms :: Maybe TypeSynonyms
    , getCheckingDebugOutputDataTypes :: Maybe DataTypes
    , getCheckingDebugOutputClassses :: Maybe (Classes Exp)
    , getCheckingDebugOutputInstances :: Maybe (Instances Exp)
    , getCheckingDebugOutputFunctions :: Maybe (Expressions Exp)
    } deriving (Eq, Show)

instance Semigroup CheckingDebugOutput where
    a <> b
        | CheckingDebugOutput { getCheckingDebugOutputExports = e1
                              , getCheckingDebugOutputTypeSynonyms = t1
                              , getCheckingDebugOutputDataTypes = d1
                              , getCheckingDebugOutputClassses = c1
                              , getCheckingDebugOutputInstances = i1
                              , getCheckingDebugOutputFunctions = f1
                              } <- a
        , CheckingDebugOutput { getCheckingDebugOutputExports = e2
                              , getCheckingDebugOutputTypeSynonyms = t2
                              , getCheckingDebugOutputDataTypes = d2
                              , getCheckingDebugOutputClassses = c2
                              , getCheckingDebugOutputInstances = i2
                              , getCheckingDebugOutputFunctions = f2
                              } <- b =
            CheckingDebugOutput
                { getCheckingDebugOutputExports = e1 <|> e2
                , getCheckingDebugOutputTypeSynonyms = t1 <> t2
                , getCheckingDebugOutputDataTypes = d1 <> d2
                , getCheckingDebugOutputClassses = c1 <> c2
                , getCheckingDebugOutputInstances = i1 <> i2
                , getCheckingDebugOutputFunctions = f1 <> f2
                }

instance Monoid CheckingDebugOutput where
    mempty =
        CheckingDebugOutput
            { getCheckingDebugOutputExports = Nothing
            , getCheckingDebugOutputTypeSynonyms = mempty
            , getCheckingDebugOutputDataTypes = mempty
            , getCheckingDebugOutputClassses = mempty
            , getCheckingDebugOutputInstances = mempty
            , getCheckingDebugOutputFunctions = mempty
            }

-- | Type for objects which do desugaring
type CheckingProcessor
     = ReaderT CheckingProcessorEnvironment (WithDebugOutput CheckingError CheckingDebugOutput)

-- | Defines a set of local names
defineLocalNames ::
       HS.HashSet Ident -> CheckingProcessor a -> CheckingProcessor a
defineLocalNames names = local (setLocalNames names)

-- | Writes a debug output
writeCheckingDebugOutput :: CheckingDebugOutput -> CheckingProcessor ()
writeCheckingDebugOutput = lift . writeDebugOutput

-- | Runs checking
runCheckingProcessor ::
       CheckingProcessorEnvironment
    -> CheckingProcessor a
    -> (Either CheckingError a, CheckingDebugOutput)
runCheckingProcessor env gp = runWithDebugOutput $ runReaderT gp env

-- | Checks that a name is unambiguously defined
checkName ::
       (ImportedGroups -> ImportedNames)
    -> (GroupingProcessorState -> DefinedNames)
    -> WithLocation Ident
    -> CheckingProcessor (WithLocation Ident)
checkName getImported getDefined name = do
    CheckingProcessorEnvironment { getCheckingProcessorEnvironmentModuleName = moduleName
                                 , getCheckingProcessorEnvironmentImports = imports
                                 , getCheckingProcessorEnvironmentTopLevel = topLevel
                                 , getCheckingProcessorEnvironmentLocalDefinitions = locals
                                 } <- ask
    let name' = getValue name
        imported = fromMaybe [] . HM.lookup name' $ getImported imports
        defined = maybeToList . HM.lookup name' $ getDefined topLevel
        sources =
            map AmbiguitySourceImport imported ++
            map AmbiguitySourceThisFile defined
        isLocal = name' `HS.member` locals
    -- If ident is not local, check that it's defined only once
    unless isLocal . lift $ do
        when (null sources) . raiseError $ CheckingErrorUndefinedName name
        when (length sources > 1) . raiseError $
            CheckingErrorAmbiguousName name sources
    let qualifiedName =
            name $>
            if isLocal || isQualified name'
                then name'
                else if length imported == 1 -- If name is imported
                         then getValue $ head imported -- Return correct naming
                         else makeQualifiedName moduleName name'
    return qualifiedName

-- | Checks that a type name is unambigously defined
checkTypeName :: WithLocation Ident -> CheckingProcessor (WithLocation Ident)
checkTypeName = checkName getImportedGroupsTypes getGroupingProcessorStateTypes

-- | Checks that an expression name is unambigously defined
checkExpressionName ::
       WithLocation Ident -> CheckingProcessor (WithLocation Ident)
checkExpressionName =
    checkName getImportedGroupsExpressions getGroupingProcessorStateExpressions

-- | Checks that a module name is defined
checkModuleName :: WithLocation Ident -> CheckingProcessor (WithLocation Ident)
checkModuleName name = do
    CheckingProcessorEnvironment {getCheckingProcessorEnvironmentImports = imports} <-
        ask
    let moduleNames = getImportedGroupsModules imports
    if getValue name `HS.member` moduleNames
        then return name
        else lift . raiseError $ CheckingErrorUndefinedName name

-- | Is ident qualified?
isQualified :: Ident -> Bool
isQualified ident
    | IdentGenerated {} <- ident = False
    | IdentUserDefined ud <- ident =
        case ud of
            IdentQualified {} -> True
            IdentSimple {} -> False
