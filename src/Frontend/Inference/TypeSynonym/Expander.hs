{- |
Module      :  Frontend.Inference.TypeSynonym.Expander
Description :  Functions for expanding type synonyms in AST
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for expanding type synonyms in AST
-}
module Frontend.Inference.TypeSynonym.Expander
    ( expandModule
    , AstWithKinds(..)
    , TypeSynonymExpandingError(..)
    ) where

import Control.Monad (liftM2, when)
import Control.Monad.Trans.Reader (ask)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM

import Frontend.Inference.Kind.Ast
import Frontend.Inference.Signature
import Frontend.Inference.TypeSynonym.Expand
import Frontend.Inference.Variables
import Frontend.Syntax.Position
import Util.HashMap

-- | Expands type synonyms in AST
expandModule ::
       HM.HashMap Ident TypeSignature
    -> AstWithKinds
    -> Either TypeSynonymExpandingError AstWithKinds
expandModule signature ast =
    runTypeSynonymExpander signature $ expandModule' ast

expandExp :: WithLocation Exp -> TypeSynonymExpander (WithLocation Exp)
expandExp exp' =
    (exp' $>) <$>
    case getValue exp' of
        ExpVar name -> return $ ExpVar name
        ExpConstr name -> return $ ExpConstr name
        ExpConst c -> return $ ExpConst c
        ExpAbstraction name inner -> ExpAbstraction name <$> expandExp inner
        ExpCase var type' args ifTrue ifFalse ->
            (\res -> ExpCase var type' args res ifFalse) <$> expandExp ifTrue
        ExpApplication func args ->
            liftM2 ExpApplication (expandExp func) (mapM expandExp args)
        ExpLet exps inner ->
            liftM2 ExpLet (mapHashMapM expandExpression exps) (expandExp inner)

expandExpression :: Expression -> TypeSynonymExpander Expression
expandExpression expr@Expression { getExpressionBody = body
                                 , getExpressionType = type'
                                 } = do
    expandedBody <- expandExp body
    expandedType <- traverse expandTypeSignature type'
    return
        expr
            {getExpressionBody = expandedBody, getExpressionType = expandedType}

expandTypeSignature :: TypeSignature -> TypeSynonymExpander TypeSignature
expandTypeSignature signature = do
    expandedType <- expandType $ getTypeSignatureType signature
    expandedContext <- mapM expandConstraint $ getTypeSignatureContext signature
    return
        signature
            { getTypeSignatureType = expandedType
            , getTypeSignatureContext = expandedContext
            }

expandMethod :: Method -> TypeSynonymExpander Method
expandMethod method@Method {getMethodType = type', getMethodBody = body'} = do
    expandedType <- expandTypeSignature type'
    expandedBody <- traverse expandExp body'
    return method {getMethodType = expandedType, getMethodBody = expandedBody}

expandClass :: Class -> TypeSynonymExpander Class
expandClass class'@Class {getClassMethods = methods} = do
    expandedMethods <- mapHashMapM expandMethod methods
    return class' {getClassMethods = expandedMethods}

expandInstance :: Instance -> TypeSynonymExpander Instance
expandInstance inst@Instance { getInstanceClass = className
                             , getInstanceType = typeName
                             , getInstanceMethods = methods
                             } = do
    typeSynonyms <- ask
    when (getValue typeName `HM.member` typeSynonyms) . raiseError $
        TypeSynonymExpandingErrorSynonymInInstance className typeName
    expandedMethods <- mapHashMapM expandExp methods
    return inst {getInstanceMethods = expandedMethods}

expandConstructor :: Constructor -> TypeSynonymExpander Constructor
expandConstructor constr@Constructor {getConstructorArgs = args} = do
    expandedArgs <- mapM expandType args
    return constr {getConstructorArgs = expandedArgs}

expandDataType :: DataType -> TypeSynonymExpander DataType
expandDataType dataType@DataType { getDataTypeContext = context
                                 , getDataTypeConstructors = constructors
                                 } = do
    expandedContext <- mapM expandConstraint context
    let expandConstructor' (constrName, constr) =
            (\x -> (constrName, x)) <$> expandConstructor constr
    expandedConstructors <- mapM expandConstructor' constructors
    return
        dataType
            { getDataTypeContext = expandedContext
            , getDataTypeConstructors = expandedConstructors
            }

expandModule' :: AstWithKinds -> TypeSynonymExpander AstWithKinds
expandModule' AstWithKinds { getAstWithKindsClasses = classes
                           , getAstWithKindsInstances = instances
                           , getAstWithKindsExpressions = expressions
                           , getAstWithKindsDataTypes = dataTypes
                           } = do
    expandedClasses <- mapHashMapM expandClass classes
    expandedInstances <- mapM expandInstance instances
    expandedExpressions <- mapHashMapM expandExpression expressions
    expandedDataTypes <- mapHashMapM expandDataType dataTypes
    return
        AstWithKinds
            { getAstWithKindsClasses = expandedClasses
            , getAstWithKindsInstances = expandedInstances
            , getAstWithKindsExpressions = expandedExpressions
            , getAstWithKindsDataTypes = expandedDataTypes
            }
