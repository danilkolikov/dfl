{- |
Module      :  Compiler.Prettify.LetExpression
Description :  Pretty-printing of let-removed expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty-printing of expressions without let
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.LetExpression where

import qualified Data.List.NonEmpty as NE (toList)

import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.PrettyPrinter
import Compiler.Prettify.Utils
import Frontend.Inference.Let.Ast
import Frontend.Inference.Signature
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

instance PrettyPrintable TypeSignature where
    prettyPrint = singleLine . prettify

instance PrettyPrintable Expression where
    prettyPrint (Expression name body Nothing) =
        joinPrinters
            [prettyPrint name, prettyPrint OperatorEq, prettyPrint body]
    prettyPrint (Expression name body (Just sig)) =
        multiplePrinters
            [ joinPrinters
                  [prettyPrint name, prettyPrint OperatorQDot, prettyPrint sig]
            , prettyPrint (Expression name body Nothing)
            ]

instance Prettifiable Expression where
    prettify = prettifyAst
