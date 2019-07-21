{- |
Module      :  Frontend.Inference.Type.Classes
Description :  Functions for processing classes.
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for processing classes.
-}
module Frontend.Inference.Type.Classes where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Creates type signatures for a class' methods
createClassSignatures :: Class -> HM.HashMap Ident TypeSignature
createClassSignatures Class { getClassName = name
                            , getClassParam = param
                            , getClassMethods = methods
                            } =
    let constraint = withDummyLocation $ ConstraintParam name param
     in HM.fromList $ map (createMethodSignature constraint) $ HM.elems methods

-- | Creates a type signature for a method of a class
createMethodSignature ::
       WithLocation Constraint -> Method -> (Ident, TypeSignature)
createMethodSignature constraint Method { getMethodName = name
                                        , getMethodType = TypeSignature { getTypeSignatureContext = context
                                                                        , getTypeSignatureType = type'
                                                                        }
                                        } =
    let methodName = getValue name
        methodContext = constraint : context
     in ( methodName
        , TypeSignature
              { getTypeSignatureContext = methodContext
              , getTypeSignatureType = type'
              })
