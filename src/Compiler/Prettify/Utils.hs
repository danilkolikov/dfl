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
import Frontend.Inference.Constraint
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

prettifyHeader :: String -> String
prettifyHeader header = unlines [header, replicate (length header) '-']

prettifyLocation :: SourceLocation -> String
prettifyLocation (SourceLocation start end) =
    concat [prettifyPosition start, "...", prettifyPosition end]

prettifyPosition :: SourcePosition -> String
prettifyPosition (SourcePosition line column) =
    concat ["(", show line, ", ", show column, ")"]

prettifyEntityName :: EntityName -> String
prettifyEntityName = intercalate "."

prettifyIdent :: Ident -> String
prettifyIdent ident =
    case ident of
        IdentNamed name -> prettifyEntityName name
        IdentParametrised name param ->
            "(" ++ prettifyEntityName name ++ ", " ++ show param ++ ")"
        IdentGenerated env param ->
            "(" ++ prettifyIdentEnvironment env ++ ": " ++ show param ++ ")"

prettifyIdentEnvironment :: IdentEnvironment -> String
prettifyIdentEnvironment env =
    case env of
        IdentEnvironmentRecordDesugaring -> "record"
        IdentEnvironmentExpressionDesugaring -> "exp"
        IdentEnvironmentDependencyResolution -> "dep"
        IdentEnvironmentTypeVariable -> "type"
        IdentEnvironmentKindVariable -> "kind"
        IdentEnvironmentSortVariable -> "sort"

prettifyForAll :: [(Ident, a)] -> String
prettifyForAll [] = ""
prettifyForAll vars =
    "forall " ++ unwords (map (prettifyIdent . fst) vars) ++ ". "

prettifySort :: Sort -> String
prettifySort sort =
    case sort of
        SortSquare -> "[]"
        SortVar name -> prettifyIdent name
        SortFunction from to ->
            "(" ++ prettifySort from ++ "->" ++ prettifySort to ++ ")"

prettifyKind :: Kind -> String
prettifyKind kind =
    case kind of
        KindStar -> "*"
        KindVar name -> prettifyIdent name
        KindFunction from to ->
            "(" ++ prettifyKind from ++ "->" ++ prettifyKind to ++ ")"

prettifyType :: Type -> String
prettifyType type' =
    case type' of
        TypeVar name -> prettifyIdent name
        TypeConstr name -> prettifyIdent name
        TypeFunction from to ->
            "(" ++ prettifyType from ++ "->" ++ prettifyType to ++ ")"
        TypeApplication func args ->
            "(" ++
            prettifyType func ++
            " " ++ unwords (map prettifyType $ NE.toList args) ++ ")"

prettifyConstraint :: Constraint -> String
prettifyConstraint constraint =
    case constraint of
        ConstraintVariable class' type' ->
            unwords [prettifyIdent class', prettifyType type']
        ConstraintType class' type' args ->
            unwords
                [ prettifyIdent class'
                , "(" ++
                  unwords
                      (prettifyIdent type' : map prettifyType (NE.toList args)) ++
                  ")"
                ]

prettifyTypeSignature :: (Ident, TypeSignature) -> String
prettifyTypeSignature (name, sig@TypeSignature { getTypeSignatureKindParams = kindParams
                                               , getTypeSignatureTypeParams = typeParams
                                               , getTypeSignatureType = type'
                                               , getTypeSignatureContext = context
                                               }) =
    unwords
        [ prettifyIdent name
        , "::" ++ prettifyContext context
        , prettifyForAll typeParams ++ prettifyType type'
        , "::"
        , prettifyForAll kindParams ++ prettifyKind (getFullKind sig)
        , "::"
        , prettifySort (getFullSort sig)
        ]

prettifyContext :: [Constraint] -> String
prettifyContext [] = ""
prettifyContext constraints =
    " (" ++ intercalate ", " (map prettifyConstraint constraints) ++ ") =>"
