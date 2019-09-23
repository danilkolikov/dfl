{- |
Module      :  Compiler.Prettify.Expression
Description :  Pretty-printing of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty-printing of expressions
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.Expression where

import qualified Data.HashMap.Lazy as HM
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE

import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.PrettyPrinter
import Compiler.Prettify.Utils
import Frontend.Inference.Expression
import Frontend.Inference.Signature
import Frontend.Syntax.Token

instance PrettyPrintable Type where
    prettyPrint = singleLine . prettify

instance PrettyPrintable External where
    prettyPrint = singleLine . prettify

instance Prettifiable External where
    prettify External { getExternalName = name
                      , getExternalTypeArgs = typeArgs
                      , getExternalKindArgs = kindArgs
                      } =
        "(" ++
        prettify name ++
        " {" ++ prettifyMap typeArgs ++ "} {" ++ prettifyMap kindArgs ++ "})"
      where
        prettifyPair (k, v) = intercalate " :: " [prettify k, prettify v]
        prettifyMap ::
               (Prettifiable a, Prettifiable b) => HM.HashMap a b -> String
        prettifyMap = intercalate ", " . map prettifyPair . HM.toList

instance Prettifiable Exp where
    prettify = prettifyAst

instance PrettyPrintable Exp where
    prettyPrint exp' =
        case exp' of
            ExpVar name -> prettyPrint name
            ExpExternal ext -> prettyPrint ext
            ExpConst c -> prettyPrint c
            ExpConstr name -> prettyPrint name
            ExpAbstraction ident inner ->
                multiplePrinters
                    [ joinPrinters
                          [ prettyPrint OperatorBackslash
                          , prettyPrint ident
                          , prettyPrint OperatorRArrow
                          ]
                    , withIndent $ prettyPrint inner
                    ]
            ExpApplication func args ->
                multiplePrinters
                    [ prettyPrint func
                    , withIndent $
                      multiplePrinters
                          (map (inParens . prettyPrint) $ NE.toList args)
                    ]
            ExpCase var name args ifSuccess ifFail ->
                multiplePrinters
                    [ joinPrinters
                          [ prettyPrint KeywordCase
                          , prettyPrint var
                          , prettyPrint KeywordOf
                          ]
                    , withIndent $
                      multiplePrinters
                          [ joinPrinters
                                [ prettyPrint name
                                , notSep args
                                , prettyPrint OperatorRArrow
                                , prettyPrint ifSuccess
                                ]
                          , joinPrinters
                                [ prettyPrint KeywordUnderscore
                                , prettyPrint OperatorRArrow
                                , prettyPrint ifFail
                                ]
                          ]
                    ]
