{- |
Module      :  Compiler.Prettify.AstWithKinds
Description :  Pretty-printing of AST with kinds
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty-printing of AST with kinds
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.AstWithKinds where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE (toList)

import Compiler.Prettify.Desugaring.Ast ()
import Compiler.Prettify.LetExpression ()
import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.PrettyPrinter
import Compiler.Prettify.Utils
import Frontend.Inference.Kind.Ast
import Frontend.Inference.Signature
import Frontend.Syntax.Token

printIndentedMap :: (PrettyPrintable a) => HM.HashMap b a -> PrettyPrinter
printIndentedMap =
    withIndent . multiplePrinters . map (prettyPrint . snd) . HM.toList

printList :: (PrettyPrintable a) => [a] -> PrettyPrinter
printList = multiplePrinters . map prettyPrint

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

instance Prettifiable Exp where
    prettify = prettifyAst

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

instance PrettyPrintable Method where
    prettyPrint (Method name type' Nothing) =
        joinPrinters
            [prettyPrint name, prettyPrint OperatorQDot, prettyPrint type']
    prettyPrint (Method name type' (Just body)) =
        multiplePrinters
            [ prettyPrint (Method name type' Nothing)
            , joinPrinters
                  [prettyPrint name, prettyPrint OperatorEq, prettyPrint body]
            ]

instance PrettyPrintable Instance where
    prettyPrint (Instance context class' type' args methods) =
        joinPrinters
            [ prettyPrint KeywordInstance
            , inContext context
            , prettyPrint class'
            , prettyPrint type'
            , notSep args
            , newLine
            , printIndentedMap methods
            ]

instance Prettifiable Instance where
    prettify = prettifyAst

instance PrettyPrintable Class where
    prettyPrint (Class context class' param methods) =
        joinPrinters
            [ prettyPrint KeywordClass
            , inContext context
            , prettyPrint class'
            , prettyPrint param
            , newLine
            , printIndentedMap methods
            ]

instance PrettyPrintable Constructor where
    prettyPrint (Constructor name args fields) =
        joinPrinters [prettyPrint name, notSep args, singleLine $ show fields]

instance PrettyPrintable DataType where
    prettyPrint (DataType context name params deriving' constructors isNewType') =
        joinPrinters
            [ prettyPrint $
              if isNewType'
                  then KeywordNewType
                  else KeywordData
            , inContext context
            , prettyPrint name
            , notSep params
            , inDeriving deriving'
            , newLine
            , printIndentedMap (HM.fromList constructors)
            ]

instance Prettifiable DataType where
    prettify = prettifyAst

instance PrettyPrintable Constraint where
    prettyPrint = singleLine . prettify

instance PrettyPrintable Type where
    prettyPrint = singleLine . prettify

instance PrettyPrintable AstWithKinds where
    prettyPrint (AstWithKinds dataTypes classes instances exps) =
        joinPrinters
            [ printIndentedMap classes
            , newLine
            , printIndentedMap dataTypes
            , newLine
            , printList instances
            , newLine
            , printIndentedMap exps
            ]

instance Prettifiable AstWithKinds where
    prettify = prettifyAst
