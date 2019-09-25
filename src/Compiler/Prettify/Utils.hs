{- |
Module      :  Compiler.Prettify.Utils
Description :  Utility functions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for pretty printing
-}
module Compiler.Prettify.Utils where

import qualified Data.HashMap.Lazy as HM
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Tuple (swap)

import Frontend.Desugaring.Final.Ast (Ident(..), IdentEnvironment(..))
import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Class (Class(..))
import Frontend.Inference.Constraint
import Frontend.Inference.Instance (Instance(..))
import Frontend.Inference.Kind.Processor (KindInferenceEnvironmentItem(..))
import Frontend.Inference.Signature
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position
import Frontend.Syntax.Token

inverseMap :: HM.HashMap a b -> [(b, a)]
inverseMap = map swap . HM.toList

inversedSpecial :: [(Special, Char)]
inversedSpecial = inverseMap specialSymbols

inversedKeywords :: [(Keyword, String)]
inversedKeywords = inverseMap keywords

inversedOperators :: [(Operator, String)]
inversedOperators = inverseMap operators

indentLines :: String -> String
indentLines = unlines' . map ("  " ++) . lines

unlines' :: [String] -> String
unlines' = intercalate "\n"

prettifyHeader :: String -> String
prettifyHeader header = unlines' [header, replicate (length header) '-']

class Prettifiable a where
    prettify :: a -> String

prettifyWithHeader :: (Prettifiable a) => String -> a -> String
prettifyWithHeader header x = unlines' [prettifyHeader header, prettify x]

newtype Indented a =
    Indented a

instance (Prettifiable a) => Prettifiable (Indented a) where
    prettify (Indented x) = indentLines $ prettify x

instance (Prettifiable a) => Prettifiable [a] where
    prettify = unlines' . map prettify

instance (Prettifiable a, Prettifiable b) => Prettifiable (Either a b) where
    prettify = either prettify prettify

instance Prettifiable () where
    prettify = show

instance Prettifiable SourceLocation where
    prettify (SourceLocation start end) =
        concat [prettify start, "...", prettify end]

instance Prettifiable SourcePosition where
    prettify (SourcePosition line column) =
        concat ["(", show line, ", ", show column, ")"]

prettifyEntityName :: EntityName -> String
prettifyEntityName = intercalate "."

instance Prettifiable Ident where
    prettify ident =
        case ident of
            IdentNamed name -> prettifyEntityName name
            IdentParametrised name param ->
                "(" ++ prettifyEntityName name ++ ", " ++ show param ++ ")"
            IdentGenerated env param ->
                "(" ++ prettify env ++ ": " ++ show param ++ ")"
            IdentScoped idents -> intercalate "->" (map prettify idents)
            IdentInstance className instanceName ->
                "(" ++
                prettify className ++ ": " ++ prettify instanceName ++ ")"

instance Prettifiable IdentEnvironment where
    prettify env =
        case env of
            IdentEnvironmentRecordDesugaring -> "record"
            IdentEnvironmentExpressionDesugaring -> "exp"
            IdentEnvironmentDependencyResolution -> "dep"
            IdentEnvironmentTypeVariable -> "type"
            IdentEnvironmentKindVariable -> "kind"
            IdentEnvironmentSortVariable -> "sort"
            IdentEnvironmentInstances -> "instance"
            IdentEnvironmentLet -> "let"
            IdentEnvironmentTranslation -> "translation"

prettifyForAll :: [(Ident, a)] -> String
prettifyForAll [] = ""
prettifyForAll vars = "forall " ++ unwords (map (prettify . fst) vars) ++ ". "

instance Prettifiable Sort where
    prettify sort =
        case sort of
            SortSquare -> "[]"
            SortVar name -> prettify name
            SortFunction from to ->
                "(" ++ prettify from ++ "->" ++ prettify to ++ ")"

instance Prettifiable Kind where
    prettify kind =
        case kind of
            KindStar -> "*"
            KindVar name -> prettify name
            KindFunction from to ->
                "(" ++ prettify from ++ "->" ++ prettify to ++ ")"

instance Prettifiable Type where
    prettify type' =
        case type' of
            TypeVar name -> prettify name
            TypeConstr name -> prettify name
            TypeFunction from to ->
                "(" ++ prettify from ++ "->" ++ prettify to ++ ")"
            TypeApplication func args ->
                "(" ++
                prettify func ++
                " " ++ unwords (map prettify $ NE.toList args) ++ ")"

instance Prettifiable Constraint where
    prettify constraint =
        case constraint of
            ConstraintVariable class' type' ->
                unwords [prettify class', prettify type']
            ConstraintAppliedVariable class' type' args ->
                unwords
                    [ prettify class'
                    , "(" ++
                      unwords (prettify type' : map prettify (NE.toList args)) ++
                      ")"
                    ]

instance Prettifiable SimpleConstraint where
    prettify (SimpleConstraint cls type') = unwords $ map prettify [cls, type']

instance Prettifiable TypeConstructorSignature where
    prettify sig@TypeConstructorSignature { getTypeConstructorSignatureKindParams = kindParams
                                          , getTypeConstructorSignatureTypeParams = typeParams
                                          } =
        unwords
            [ "::"
            , prettifyForAll typeParams
            , "::"
            , prettifyForAll kindParams ++ prettify (getFullKind sig)
            , "::"
            , prettify (getFullSort sig)
            ]

instance Prettifiable TypeSignature where
    prettify sig@TypeSignature { getTypeSignatureKindParams = kindParams
                               , getTypeSignatureTypeParams = typeParams
                               , getTypeSignatureType = type'
                               , getTypeSignatureContext = context
                               } =
        unwords
            [ "::" ++ prettifyContext context
            , prettifyForAll typeParams ++ prettify type'
            , "::"
            , prettifyForAll kindParams ++ prettify (getFullKind sig)
            , "::"
            , prettify (getFullSort sig)
            ]

prettifyContext :: [Constraint] -> String
prettifyContext [] = ""
prettifyContext constraints =
    " (" ++ intercalate ", " (map prettify constraints) ++ ") =>"

instance (Prettifiable a, Prettifiable b) => Prettifiable (HM.HashMap a b) where
    prettify = unlines' . map prettifyPair . HM.toList
      where
        prettifyPair (key, value) =
            intercalate " : " [prettify key, prettify value]

-- An instance for ExpWithSignature
instance (Prettifiable a, Prettifiable b) => Prettifiable (a, b) where
    prettify (k, v) = unlines' [prettify v, prettify k]

instance (Prettifiable a) => Prettifiable (WithLocation a) where
    prettify = prettify . getValue

instance Prettifiable KindInferenceEnvironmentItem where
    prettify item =
        case item of
            KindInferenceEnvironmentItemTypeSynonym F.TypeSynonym {F.getTypeSynonymName = name} ->
                unwords ["Type synonym", prettify name]
            KindInferenceEnvironmentItemDataType F.DataType {F.getDataTypeName = name} ->
                unwords ["DataType", prettify name]
            KindInferenceEnvironmentItemClass F.Class {F.getClassName = name} ->
                unwords ["Class", prettify name]

instance Prettifiable Class where
    prettify Class { getClassContext = context
                   , getClassName = name
                   , getClassParam = param
                   , getClassDataTypeName = dataType
                   , getClassGetters = getters
                   , getClassMethods = methods
                   , getClassDefaultInstanceName = defaultInstance
                   } =
        unlines'
            [ unwords ["Class", prettify name, prettify param]
            , indentLines $
              unlines'
                  [ unwords
                        ["Context:", intercalate ", " $ map prettify context]
                  , unwords
                        [ "Data types:"
                        , intercalate ", " $
                          map prettify [dataType, defaultInstance]
                        ]
                  , unwords ["Methods:", unwords $ map prettify methods]
                  , "Getters:"
                  , prettify $ Indented getters
                  ]
            ]

instance Prettifiable Instance where
    prettify Instance { getInstanceContext = context
                      , getInstanceClass = cls
                      , getInstanceType = type'
                      , getInstanceTypeArgs = args
                      , getInstanceExpression = name
                      } =
        unlines'
            [ unwords $
              ["Instance", prettify cls, prettify type'] ++ map prettify args
            , indentLines $
              unlines'
                  [ unwords
                        ["Context:", intercalate ", " $ map prettify context]
                  , unwords ["Expression:", prettify name]
                  ]
            ]
