{- |
Module      :  Frontend.Inference.Type.Classes
Description :  Functions for processing classes.
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for processing classes.
-}
module Frontend.Inference.Type.Classes where

import Data.Bifunctor (first, second)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)

import Frontend.Desugaring.Final.Ast
import Frontend.Inference.Base.Descriptor (BaseInferOutput, SimpleInfer)
import qualified Frontend.Inference.Class as C
import qualified Frontend.Inference.Constraint as Cn
import qualified Frontend.Inference.Expression as E
import qualified Frontend.Inference.Instance as I
import qualified Frontend.Inference.Signature as S
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Creates type signatures for a class' methods
createClassSignatures :: Class -> HM.HashMap Ident TypeSignature
createClassSignatures Class { getClassName = name
                            , getClassParam = param
                            , getClassMethods = methods
                            } =
    HM.fromList $ map (createMethodSignature name param) $ HM.elems methods

-- | Creates a type signature for a method of a class
createMethodSignature ::
       WithLocation Ident
    -> WithLocation Ident
    -> Method
    -> (Ident, TypeSignature)
createMethodSignature className classParam Method { getMethodName = name
                                                  , getMethodType = type'
                                                  } =
    (getValue name, createTypeSignature className classParam type')

-- | Creates a type signature for a method
createTypeSignature ::
       WithLocation Ident
    -> WithLocation Ident
    -> TypeSignature
    -> TypeSignature
createTypeSignature className classParam TypeSignature { getTypeSignatureContext = context
                                                       , getTypeSignatureType = type'
                                                       } =
    let constraint = withDummyLocation $ ConstraintParam className classParam
        methodContext = constraint : context
     in TypeSignature
            { getTypeSignatureContext = methodContext
            , getTypeSignatureType = type'
            }

-- | Processes a class, checks types of default implementations of methods
-- | and creates a default instance
inferClass ::
       SimpleInfer Expressions S.TypeSignature E.Exp
    -> Class
    -> BaseInferOutput C.Class
inferClass inferExp Class { getClassContext = context
                          , getClassName = className
                          , getClassParam = classParam
                          , getClassMethods = methods
                          } =
    let processConstraint sc
            | SimpleConstraint name param <- getValue sc =
                Cn.SimpleConstraint (getValue name) (getValue param)
        newContext = map processConstraint context
        newName = getValue className
        newParam = getValue classParam
        expsToInstance exps =
            (HM.map snd exps, Just $ I.DefaultInstance newName exps)
        processResult =
            uncurry (C.Class newContext newName newParam) . expsToInstance
     in first (second processResult) $ processMethods inferExp methods

-- | Processes methods of a class
processMethods ::
       SimpleInfer Expressions S.TypeSignature E.Exp
    -> Methods
    -> BaseInferOutput (S.Signatures E.ExpWithSignature)
processMethods infer methods =
    let preparedMethods = HM.map processMethod methods
     in first (second $ \(x, _, _) -> x) $ infer preparedMethods

-- | Processes a single method
processMethod :: Method -> Expression
processMethod Method {getMethodName = name, getMethodDefault = exp'} =
    let defaultExp =
            withDummyLocation . ExpVar . withDummyLocation . IdentNamed $
            uNDEFINED_NAME
        methodExp = fromMaybe defaultExp exp'
     in Expression name methodExp Nothing
