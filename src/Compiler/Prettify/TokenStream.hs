{- |
Module      :  Compiler.Prettify.TokenStream
Description :  Prettifying of TokenStream
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of TokenStream
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Compiler.Prettify.TokenStream where

import Data.Char (toUpper)
import Data.List (intercalate, unlines, unwords)
import Data.Maybe (fromJust)

import Compiler.Prettify.Utils
import Frontend.Syntax.Position
import Frontend.Syntax.Stream (TokenStream(..))
import Frontend.Syntax.Token

instance Prettifiable TokenStream where
    prettify = prettifyTokenStream

prettifyTokenStream :: TokenStream -> String
prettifyTokenStream (TokenStream tokens) =
    let help = "Token: (from line, from column)...(to line, to column)"
        outputLines = map prettifyTokenWithLocation tokens
     in unlines $ prettifyHeader help : outputLines

prettifyTokenWithLocation :: WithLocation Token -> String
prettifyTokenWithLocation (WithLocation token location) =
    concat [prettifyToken token, ": ", prettify location]

prettifyToken :: Token -> String
prettifyToken token =
    case token of
        TokenEOF _ -> "End of file"
        TokenChar (CharT char) -> unwords ["Character", show char]
        TokenInteger (IntT int) -> unwords ["Integer", show int]
        TokenFloat (FloatT float) -> unwords ["Float", show float]
        TokenString (StringT string) -> unwords ["String", show string]
        TokenSpecial special ->
            unwords
                ["Special", show . fromJust $ lookup special inversedSpecial]
        TokenKeyword keyword ->
            unwords
                ["Keyword", show . fromJust $ lookup keyword inversedKeywords]
        TokenOperator operator ->
            unwords
                [ "Operator"
                , show . fromJust $ lookup operator inversedOperators
                ]
        TokenName [] name -> capitalise $ prettifyName name
            where capitalise [] = []
                  capitalise (c:rest) = toUpper c : rest
        TokenName qualifier name ->
            concat [prettifyQualifier qualifier, " ", prettifyName name]

prettifyQualifier :: [ModId] -> String
prettifyQualifier path =
    "Qualified (" ++ intercalate "." (map (\(ConId name) -> name) path) ++ ")"

prettifyName :: Name -> String
prettifyName name =
    case name of
        NameVarId (VarId i) -> "variable " ++ show i
        NameConId (ConId i) -> "constructor " ++ show i
        NameVarSym (VarSym s) -> "operator " ++ show s
        NameConSym (ConSym s) -> "constructor operator" ++ show s
