{- |
Module      :  Compiler.Module.Dependencies
Description :  Builder of a dependency tree
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Builder of module's dependency tree
-}
module Compiler.Module.Dependencies
    ( resolveDependencies
    ) where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, execStateT, get, modify)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List (intercalate)

import Compiler.Base
import Compiler.Environment
import Compiler.Module.Base
import Frontend.Processor
import Frontend.Syntax.Ast
import Frontend.Syntax.Position
import Frontend.Syntax.Token
import Util.Debug
import Util.DependencyResolver

-- | A type of the builder of dependencies
type DependencyBuilder m
     = StateT (DependencyGraph FileName) (WithDebugOutputT DependencyBuilderError DependencyBuilderDebugOutput m)

-- | Resolves dependencies between modules
resolveDependencies :: (Compiler m) => m (DependencyGraph FileName)
resolveDependencies = do
    mainFile <- getEnvironmentComponent getMainSourceFile
    result <-
        runWithDebugOutputT $ do
            graph <-
                execStateT (getModuleDependencies $ FileName mainFile) HM.empty
            writeDebugOutput
                mempty {getDependencyBuilderDebugOutputGraph = Just graph}
            return graph
    graph <- traceStepWithDebugOutput (mainFile ++ ".modules") result
    hasCycles <- handleResult $ findCycle graph
    case hasCycles of
        Just aCycle ->
            raiseCompilationError .
            DependencyBuilderErrorCycle . map getFileName $
            HS.toList aCycle
        Nothing -> return graph

getModuleDependencies :: (Compiler m) => FileName -> DependencyBuilder m ()
getModuleDependencies fn@(FileName fileName) = do
    dependencies <- get
    -- Don't visit files twice
    unless (fn `HM.member` dependencies) $ do
        HeaderProcessorOutput {getHeaderProcessorOutputHeader = header} <-
            lift $ do
                fileExists <- liftInner $ doesFileExist fileName -- Check file existence
                unless fileExists . raiseError $
                    DependencyBuilderErrorMissingFile fileName
                -- Read file and parse header
                liftInner $ do
                    fileContent <- readFileContent fileName
                    handleResult . fst $
                        processModuleHeader fileName fileContent
        -- Get dependencies
        let headerDependencies = getHeaderDependencies header
            requiredSourceFiles = map getSourceFile headerDependencies
            fileDependencies = HS.fromList requiredSourceFiles
        -- Save and check all imported files
        modify $ HM.insert fn fileDependencies
        mapM_ getModuleDependencies requiredSourceFiles

getHeaderDependencies :: Module Header -> [[String]]
getHeaderDependencies module' =
    case module' of
        ModuleExplicit _ _ header -> getHeaderDependencies' (getValue header)
        ModuleImplicit header -> getHeaderDependencies' (getValue header)
  where
    getHeaderDependencies' (Header imports) =
        map (getImportDependencies . getValue) imports
    getImportDependencies (ImpDecl _ moduleName _ _) =
        getModuleName $ getValue moduleName
    getModuleName (Qualified path name) = map getConIdString $ path ++ [name]
    getConIdString (ConId str) = str

getSourceFile :: [String] -> FileName
getSourceFile files = FileName $ intercalate "/" files ++ ".dfl"
