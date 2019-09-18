{- |
Module      :  Frontend.Inference.Class.Processor
Description :  Functions for processing classes.
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for processing classes.
-}
module Frontend.Inference.Class.Processor
    ( ClassProcessorState(..)
    , ClassProcessingError(..)
    , ClassDebugOutput(..)
    , processClasses
    ) where

import Control.Monad (unless)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)

import Frontend.Desugaring.Final.Ast (Ident)
import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Class.Ast
import Frontend.Inference.Class.Base
import Frontend.Inference.Class.Single
import Frontend.Inference.DependencyResolver
import qualified Frontend.Inference.Kind.Ast as K
import Frontend.Inference.Signature
import Frontend.Inference.Util.Debug
import Frontend.Syntax.Position

-- | Processes provided type classes - generates data types and collects methods
processClasses ::
       HM.HashMap Ident Class
    -> Signatures TypeConstructorSignature
    -> HM.HashMap Ident K.Class
    -> (Either ClassProcessingError ClassProcessorState, ClassDebugOutput)
processClasses initialClasses initialSignatures classes =
    let globalState =
            mempty
                { getClassProcessorStateClasses = initialClasses
                , getClassProcessorStateSignatures = initialSignatures
                }
     in runWithDebugOutput (processClasses' globalState classes)

processClasses' ::
       ClassProcessorState
    -> HM.HashMap Ident K.Class
    -> ClassProcessor ClassProcessorState
processClasses' globalState classes = do
    let dependencies = getDependencies classes
        loops = getLoops dependencies
    writeDebugOutput
        mempty {getClassDebugOutputDependencyGraph = Just dependencies}
    unless (null loops) . raiseError . ClassProcessingErrorRecursive $
        head loops
    (dependencyGroups, processed) <-
        wrapEither ClassProcessingErrorDependencyResolution $
        traverseGraph
            (processDependencyGroup globalState classes)
            mempty
            dependencies
    writeDebugOutput
        mempty {getClassDebugOutputDependencyGroups = Just dependencyGroups}
    processed

processDependencyGroup ::
       ClassProcessorState
    -> HM.HashMap Ident K.Class
    -> ClassProcessorState
    -> HS.HashSet Ident
    -> ClassProcessor ClassProcessorState
processDependencyGroup globalState allClasses initial group = do
    let groupList = HS.toList group
    unless (length groupList == 1) .
        raiseError . ClassProcessingErrorMutuallyRecursive $
        groupList
    let className = head groupList
        class' = fromJust $ HM.lookup className allClasses
        classSignature =
            fromJust . HM.lookup className $
            getClassProcessorStateSignatures globalState
        newState = globalState <> initial
    result <- processClass newState class' classSignature
    writeDebugOutput mempty {getClassDebugOutputOutputs = Just [result]}
    return $ initial <> result

-- | Gets dependencies of classes
getDependencies :: HM.HashMap Ident K.Class -> DependencyGraph
getDependencies = HM.map getClassDependencies
  where
    getClassDependencies :: K.Class -> Dependencies
    getClassDependencies K.Class {K.getClassContext = constraints} =
        HS.fromList $
        map (getValue . F.getSimpleConstraintClass . getValue) constraints
