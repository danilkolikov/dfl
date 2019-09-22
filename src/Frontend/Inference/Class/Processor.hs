{- |
Module      :  Frontend.Inference.Class.Processor
Description :  Functions for processing classes.
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for processing classes.
-}
module Frontend.Inference.Class.Processor
    ( ClassProcessorOutput(..)
    , ClassProcessorError(..)
    , ClassProcessorDebugOutput(..)
    , processClasses
    ) where

import Control.Monad (unless)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)

import Frontend.Desugaring.Final.Ast (Ident)
import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Class
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
    -> ( Either ClassProcessorError ClassProcessorOutput
       , ClassProcessorDebugOutput)
processClasses initialClasses initialSignatures classes =
    let globalState =
            mempty
                { getClassProcessorOutputClasses = initialClasses
                , getClassProcessorOutputSignatures = initialSignatures
                }
     in runWithDebugOutput (processClasses' globalState classes)

processClasses' ::
       ClassProcessorOutput
    -> HM.HashMap Ident K.Class
    -> ClassProcessor ClassProcessorOutput
processClasses' globalState classes = do
    let dependencies = getDependencies classes
        loops = getLoops dependencies
    writeDebugOutput
        mempty {getClassProcessorDebugOutputDependencyGraph = Just dependencies}
    unless (null loops) . raiseError . ClassProcessorErrorRecursive $ head loops
    (dependencyGroups, processed) <-
        wrapEither ClassProcessorErrorDependencyResolution $
        traverseGraph
            (processDependencyGroup globalState classes)
            mempty
            dependencies
    writeDebugOutput
        mempty
            { getClassProcessorDebugOutputDependencyGroups =
                  Just dependencyGroups
            }
    processed

processDependencyGroup ::
       ClassProcessorOutput
    -> HM.HashMap Ident K.Class
    -> ClassProcessorOutput
    -> HS.HashSet Ident
    -> ClassProcessor ClassProcessorOutput
processDependencyGroup globalState allClasses initial group = do
    let groupList = HS.toList group
    unless (length groupList == 1) .
        raiseError . ClassProcessorErrorMutuallyRecursive $
        groupList
    let className = head groupList
        class' = fromJust $ HM.lookup className allClasses
        classSignature =
            fromJust . HM.lookup className $
            getClassProcessorOutputSignatures globalState
        newState = globalState <> initial
    result <- processClass newState class' classSignature
    writeDebugOutput
        mempty {getClassProcessorDebugOutputOutputs = Just [result]}
    return $ initial <> result

-- | Gets dependencies of classes
getDependencies :: HM.HashMap Ident K.Class -> DependencyGraph
getDependencies = HM.map getClassDependencies
  where
    getClassDependencies :: K.Class -> Dependencies
    getClassDependencies K.Class {K.getClassContext = constraints} =
        HS.fromList $
        map (getValue . F.getSimpleConstraintClass . getValue) constraints
