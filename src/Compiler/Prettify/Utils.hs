{- |
Module      :  Compiler.Prettify.Utils
Description :  Utility functions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for pretty printing
-}
module Compiler.Prettify.Utils where

import Data.List (intercalate)
import Data.Tuple (swap)

import qualified Data.HashMap.Lazy as HM
import Frontend.Desugaring.Final.Ast
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
