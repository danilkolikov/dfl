{- |
Module      :  Frontend.Desugaring.Processor
Description :  Functions for desugaring of AST
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with functions for desugaring of AST
-}
module Frontend.Desugaring.Processor
    ( desugarParsedModule
    , DesugaringOutput(..)
    , InfixOperators
    , ImportedGroups(..)
    , DesugaringError(..)
    , DesugaringDebugOutput(..)
    , GroupingDebugOutput(..)
    , CheckingDebugOutput(..)
    ) where

import Control.Applicative ((<|>))

import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Checking.Processor
import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Processor
import qualified Frontend.Desugaring.Fixity.Ast as F
import Frontend.Desugaring.Fixity.Base
import Frontend.Desugaring.Fixity.Processor
import Frontend.Desugaring.Grouping.Base
import Frontend.Desugaring.Grouping.Processor
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Desugaring.Initial.ToModule
import qualified Frontend.Desugaring.Record.Ast as R
import Frontend.Desugaring.Record.Base
import Frontend.Desugaring.Record.Processor
import qualified Frontend.Syntax.Ast as A
import Util.Debug

-- | Result of desugaring
data DesugaringOutput = DesugaringOutput
    { getDesugaringOutputModule :: Module Exp
    , getDesugaringOutputGroups :: ImportedGroups
    , getDesugaringOutputFixity :: InfixOperators
    } deriving (Eq, Show)

-- | A type of errors which can be encountered during desugaring
data DesugaringError
    = DesugaringErrorGrouping GroupingProcessorError -- ^ Error of expression grouping
    | DesugaringErrorChecking CheckingError -- ^ Error of expression checking
    | DesugaringErrorFixity FixityResolutionError -- ^ Error of fixity resolution
    | DesugaringErrorRecord RecordDesugaringError -- ^ Error of record desugaring
    deriving (Eq, Show)

-- | A debug output of desugaring
data DesugaringDebugOutput = DesugaringDebugOutput
    { getDesugaringDebugOutputInitial :: Maybe I.Module
    , getDesugaringDebugOutputGrouping :: Maybe GroupingDebugOutput
    , getDesugaringDebugOutputChecking :: Maybe CheckingDebugOutput
    , getDesugaringDebugOutputFixity :: Maybe (Module F.Exp)
    , getDesugaringDebugOutputRecord :: Maybe (Module R.Exp)
    , getDesugaringDebugOutputFinal :: Maybe (Module Exp)
    } deriving (Eq, Show)

instance Semigroup DesugaringDebugOutput where
    DesugaringDebugOutput i1 g1 c1 f1 r1 fin1 <> DesugaringDebugOutput i2 g2 c2 f2 r2 fin2 =
        DesugaringDebugOutput
            (i1 <|> i2)
            (g1 <> g2)
            (c1 <> c2)
            (f1 <|> f2)
            (r1 <|> r2)
            (fin1 <|> fin2)

instance Monoid DesugaringDebugOutput where
    mempty = DesugaringDebugOutput Nothing mempty mempty Nothing Nothing Nothing

-- | A type of desugaring processor
type DesugaringProcessor = WithDebugOutput DesugaringError DesugaringDebugOutput

-- | Does desugaring of a module
desugarParsedModule ::
       ImportedGroups
    -> InfixOperators
    -> A.Module A.Body
    -> (Either DesugaringError DesugaringOutput, DesugaringDebugOutput)
desugarParsedModule importedGroups infixOperators parsedModule =
    runWithDebugOutput $
    desugarParsedModule' importedGroups infixOperators parsedModule

-- | Desugar parsed module
desugarParsedModule' ::
       ImportedGroups
    -> InfixOperators
    -> A.Module A.Body
    -> DesugaringProcessor DesugaringOutput
desugarParsedModule' importedGroups infixOperators parsedModule = do
    let initialModule = desugarToModule parsedModule
    writeDebugOutput
        mempty {getDesugaringDebugOutputInitial = Just initialModule}
    (groupedModule, groupingState) <-
        wrapErrorAndDebugOutput
            DesugaringErrorGrouping
            (\debug -> mempty {getDesugaringDebugOutputGrouping = Just debug}) $
        processModule initialModule
    checkedModule <-
        wrapErrorAndDebugOutput
            DesugaringErrorChecking
            (\debug -> mempty {getDesugaringDebugOutputChecking = Just debug}) $
        checkModule importedGroups groupingState groupedModule
    resolvedFixity <-
        wrapEither DesugaringErrorFixity $
        resolveModuleFixity infixOperators checkedModule
    writeDebugOutput
        mempty {getDesugaringDebugOutputFixity = Just resolvedFixity}
    desugaredRecords <-
        wrapEither DesugaringErrorRecord $ desugarModuleRecords resolvedFixity
    writeDebugOutput
        mempty {getDesugaringDebugOutputRecord = Just desugaredRecords}
    let final = desugarModule desugaredRecords
    writeDebugOutput mempty {getDesugaringDebugOutputFinal = Just final}
    return
        DesugaringOutput
            { getDesugaringOutputModule = final
            , getDesugaringOutputGroups = importedGroups
            , getDesugaringOutputFixity = infixOperators
            }
