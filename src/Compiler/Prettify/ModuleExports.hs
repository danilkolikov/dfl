{- |
Module      :  Compiler.Prettify.InferenceProcessorOutput
Description :  Prettifying of InferenceProcessorOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of InferenceProcessorOutput
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.ModuleExports where

import Compiler.Prettify.Expression ()
import Compiler.Prettify.Utils
import Frontend.Export.Ast
import Frontend.Export.Processor

instance Prettifiable ModuleExports where
    prettify ModuleExports { getModuleExportsExplicit = explicit
                           , getModuleExportsImplicit = implicit
                           } =
        unlines'
            [ prettifyWithHeader "Explicit exports:" $ Indented explicit
            , prettifyWithHeader "Implicit exports:" $ Indented implicit
            ]

instance Prettifiable ImplicitExport where
    prettify ImplicitExport {getImplicitExportTypeConstructors = constructors} =
        prettifyWithHeader "Type Constructor Signatures:" $
        Indented constructors

instance Prettifiable Module where
    prettify Module { getModuleDataTypes = dataTypes
                    , getModuleTypeSynonyms = typeSynonyms
                    , getModuleClasses = classes
                    , getModuleInstances = instances
                    , getModuleExpressions = expressions
                    } =
        unlines'
            [ prettifyWithHeader "Data types:" $ Indented dataTypes
            , prettifyWithHeader "Type Synonyms:" $ Indented typeSynonyms
            , prettifyWithHeader "Classes:" $ Indented classes
            , prettifyWithHeader "Instances:" $ Indented instances
            , prettifyWithHeader "Expressions:" $ Indented expressions
            ]

instance Prettifiable DataType where
    prettify DataType { getDataTypeSignature = signature
                      , getDataTypeConstructors = constructors
                      , isNewType = newType
                      } =
        unlines'
            [ if newType
                  then "newtype"
                  else "data"
            , unwords ["Signature:", prettify signature]
            , prettifyWithHeader "Constructors:" $ Indented constructors
            ]

instance Prettifiable Constructor where
    prettify Constructor { getConstructorExpression = expression
                         , getConstructorFields = fields
                         } =
        unlines' [prettify expression, unwords ["Fields:", show fields]]

instance Prettifiable Class where
    prettify Class { getClassContext = context
                   , getClassSignature = signature
                   , getClassDataTypeName = dataTypeName
                   , getClassComponents = components
                   , getClassGetters = getters
                   , getClassMethods = methods
                   , getClassDefaultInstanceName = defaultInstance
                   } =
        unlines'
            [ unwords $ "Context:" : map prettify context
            , unwords ["Signature:", prettify signature]
            , unwords ["Data type name:", prettify dataTypeName]
            , unwords $ "Components:" : map prettify components
            , unwords ["Default instance name:", prettify defaultInstance]
            , prettifyWithHeader "Getters:" $ Indented getters
            , prettifyWithHeader "Methods:" $ Indented methods
            ]

instance Prettifiable Expression where
    prettify Expression { getExpressionType = type'
                        , getExpressionFixity = fixity
                        } =
        unwords $
        ["Signature:", prettify type'] ++
        case fixity of
            Just f -> ["Fixity:", prettify f]
            Nothing -> []

instance Prettifiable FixitySignature where
    prettify FixitySignature { getFixitySignatureFixity = fixity
                             , getFixitySignaturePrecedence = prec
                             } = unwords [show fixity, show prec]
