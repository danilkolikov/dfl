{- |
Module      :  Compiler.Prettify.Desugaring.Final
Description :  Prettifying of final desugared ast
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of final desugared ast
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.Desugaring.Final where

import qualified Data.List.NonEmpty as NE (toList)

import Compiler.Prettify.Ast ()
import Compiler.Prettify.Desugaring.Ast
import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.PrettyPrinter
import Frontend.Desugaring.Final.Ast
import Frontend.Syntax.Token

instance PrettyPrintable Exp where
    prettyPrint exp' =
        case exp' of
            ExpVar name -> prettyPrint name
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
            ExpLet decls inner ->
                joinPrinters
                    [ prettyPrint KeywordLet
                    , newLine
                    , printIndentedMap decls
                    , newLine
                    , prettyPrint KeywordIn
                    , prettyPrint inner
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
                                [ notSep $ name : args
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
