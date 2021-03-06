{- |
Module      :  Frontend.Inference.Kind.Checker
Description :  Functions for kind-checking of AST
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for kind-checking of AST
-}
module Frontend.Inference.Kind.Checker
    ( checkModule
    , KindsCheckSingleDebugOutput
    ) where

import Control.Monad (liftM2)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Bifunctor (second)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Constraint
import Frontend.Inference.InferenceProcessor
import qualified Frontend.Inference.Kind.Ast as A
import Frontend.Inference.Kind.Base
import Frontend.Inference.Signature
import Frontend.Inference.Type
import Frontend.Syntax.Position
import Util.Debug
import Util.HashMap

-- | A type of debug output of a single check
type KindsCheckSingleDebugOutput
     = SingleGroupInferenceDebugOutput F.TypeSignature TypeConstructorSignature

type CheckProcessor
     = ReaderT (Signatures TypeConstructorSignature) (WithDebugOutput InferenceError [KindsCheckSingleDebugOutput])

-- | Checks kinds of type expressions in the module and returns modified AST
checkModule ::
       Signatures TypeConstructorSignature
    -> F.Module F.Exp
    -> (Either InferenceError A.AstWithKinds, [KindsCheckSingleDebugOutput])
checkModule signatures module' =
    runWithDebugOutput $ runReaderT (checkModule' module') signatures

inferKindOfTypeSignature :: F.TypeSignature -> CheckProcessor TypeSignature
inferKindOfTypeSignature signature = do
    knownSignatures <- ask
    result <-
        lift . wrapDebugOutput return $
        checkKindOfObject knownSignatures signature
    let TypeConstructorSignature { getTypeConstructorSignatureSort = sort
                                 , getTypeConstructorSignatureKindParams = kindParams
                                 , getTypeConstructorSignatureKind = kind
                                 , getTypeConstructorSignatureTypeParams = typeParams
                                 } = result
        F.TypeSignature { F.getTypeSignatureType = type'
                        , F.getTypeSignatureContext = context
                        } = signature
        newType = removePositionsOfType type'
        newContext = map removePositionsOfConstraint context
    return
        TypeSignature
            { getTypeSignatureSort = sort
            , getTypeSignatureKindParams = kindParams
            , getTypeSignatureKind = kind
            , getTypeSignatureTypeParams = typeParams
            , getTypeSignatureType = newType
            , getTypeSignatureContext = newContext
            }

checkExp :: WithLocation F.Exp -> CheckProcessor (WithLocation A.Exp)
checkExp exp' =
    (exp' $>) <$>
    case getValue exp' of
        F.ExpVar name -> return $ A.ExpVar name
        F.ExpConstr name -> return $ A.ExpConstr name
        F.ExpConst c -> return $ A.ExpConst c
        F.ExpAbstraction var inner -> A.ExpAbstraction var <$> checkExp inner
        F.ExpCase var type' params ifTrue ifFalse ->
            (\res -> A.ExpCase var type' params res ifFalse) <$> checkExp ifTrue
        F.ExpApplication func args ->
            liftM2 A.ExpApplication (checkExp func) (mapM checkExp args)
        F.ExpLet exps inner ->
            liftM2 A.ExpLet (mapHashMapM checkExpression exps) (checkExp inner)

checkExpression :: F.Expression F.Exp -> CheckProcessor A.Expression
checkExpression F.Expression { F.getExpressionName = name
                             , F.getExpressionBody = body
                             , F.getExpressionType = type'
                             } = do
    checkedBody <- checkExp body
    checkedType <- traverse inferKindOfTypeSignature type'
    return
        A.Expression
            { A.getExpressionName = name
            , A.getExpressionBody = checkedBody
            , A.getExpressionType = checkedType
            }

checkMethod :: F.Method F.Exp -> CheckProcessor A.Method
checkMethod F.Method { F.getMethodName = name
                     , F.getMethodType = type'
                     , F.getMethodBody = body'
                     } = do
    checkedType <- inferKindOfTypeSignature type'
    checkedBody <- traverse checkExp body'
    return
        A.Method
            { A.getMethodName = name
            , A.getMethodType = checkedType
            , A.getMethodBody = checkedBody
            }

checkClass :: F.Class F.Exp -> CheckProcessor A.Class
checkClass F.Class { F.getClassContext = context
                   , F.getClassName = name
                   , F.getClassParam = param
                   , F.getClassMethods = methods
                   } = do
    checkedMethods <- mapHashMapM checkMethod methods
    return
        A.Class
            { A.getClassContext = context
            , A.getClassName = name
            , A.getClassParam = param
            , A.getClassMethods = checkedMethods
            }

checkInstance :: F.Instance F.Exp -> CheckProcessor A.Instance
checkInstance F.Instance { F.getInstanceContext = context
                         , F.getInstanceClass = className
                         , F.getInstanceType = typeName
                         , F.getInstanceTypeArgs = args
                         , F.getInstanceMethods = methods
                         } = do
    checkedMethods <- mapHashMapM checkExp methods
    return
        A.Instance
            { A.getInstanceContext = context
            , A.getInstanceClass = className
            , A.getInstanceType = typeName
            , A.getInstanceTypeArgs = args
            , A.getInstanceMethods = checkedMethods
            }

checkConstructor :: F.Constructor -> A.Constructor
checkConstructor F.Constructor { F.getConstructorName = name
                               , F.getConstructorArgs = args
                               , F.getConstructorFields = fields
                               } =
    A.Constructor
        { A.getConstructorName = name
        , A.getConstructorArgs = map removePositionsOfType args
        , A.getConstructorFields = fields
        }

checkDataType :: F.DataType -> A.DataType
checkDataType F.DataType { F.getDataTypeContext = context
                         , F.getDataTypeName = name
                         , F.getDataTypeParams = params
                         , F.getDataTypeDeriving = deriving'
                         , F.getDataTypeConstructors = constructors
                         , F.isNewType = isNewType
                         } =
    A.DataType
        { A.getDataTypeContext = map removePositionsOfConstraint context
        , A.getDataTypeName = name
        , A.getDataTypeParams = params
        , A.getDataTypeDeriving = deriving'
        , A.getDataTypeConstructors = map (second checkConstructor) constructors
        , A.isNewType = isNewType
        }

checkModule' :: F.Module F.Exp -> CheckProcessor A.AstWithKinds
checkModule' F.Module { F.getModuleClasses = classes
                      , F.getModuleInstances = instances
                      , F.getModuleExpressions = expressions
                      , F.getModuleDataTypes = dataTypes
                      } = do
    checkedClasses <- mapHashMapM checkClass classes
    checkedInstances <- mapM checkInstance instances
    checkedExpressions <- mapHashMapM checkExpression expressions
    return
        A.AstWithKinds
            { A.getAstWithKindsClasses = checkedClasses
            , A.getAstWithKindsInstances = checkedInstances
            , A.getAstWithKindsExpressions = checkedExpressions
            , A.getAstWithKindsDataTypes = HM.map checkDataType dataTypes
            }
