{- |
Module      :  Compiler.Prettify.Desugaring.Ast
Description :  Prettifying of desugared ast
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of desugared ast
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.Desugaring.Ast where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE (toList)

import Compiler.Prettify.Ast ()
import Compiler.Prettify.Desugaring.Initial ()
import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.PrettyPrinter
import Compiler.Prettify.Utils
import Frontend.Desugaring.Final.Ast
import Frontend.Syntax.Token

instance PrettyPrintable Export where
    prettyPrint export =
        case export of
            ExportFunction name -> prettyPrint name
            ExportModule name -> prettyPrint name
            ExportDataOrClass name list ->
                joinPrinters [prettyPrint name, prettyPrint list]

instance PrettyPrintable Type where
    prettyPrint type' =
        case type' of
            TypeVar name -> prettyPrint name
            TypeConstr name -> prettyPrint name
            TypeFunction from to ->
                joinPrinters [prettyPrint from, singleLine "->", prettyPrint to]
            TypeApplication func args ->
                inParens $ notSep (func : NE.toList args)

printIndentedMap :: (PrettyPrintable a) => HM.HashMap b a -> PrettyPrinter
printIndentedMap =
    withIndent . multiplePrinters . map (prettyPrint . snd) . HM.toList

printList :: (PrettyPrintable a) => [a] -> PrettyPrinter
printList = multiplePrinters . map prettyPrint

instance PrettyPrintable FixitySignature where
    prettyPrint (FixitySignature name fixity prec) =
        joinPrinters
            [ singleLine (prettify name)
            , singleLine (show fixity)
            , singleLine (show prec)
            ]

instance (PrettyPrintable a) => PrettyPrintable (Expression a) where
    prettyPrint (Expression name body Nothing Nothing) =
        joinPrinters
            [prettyPrint name, prettyPrint OperatorEq, prettyPrint body]
    prettyPrint (Expression name body Nothing (Just fixity)) =
        multiplePrinters
            [ joinPrinters
                  [ prettyPrint name
                  , prettyPrint OperatorQDot
                  , prettyPrint fixity
                  ]
            , prettyPrint (Expression name body Nothing Nothing)
            ]
    prettyPrint (Expression name body (Just sig) fixity) =
        multiplePrinters
            [ joinPrinters
                  [prettyPrint name, prettyPrint OperatorQDot, prettyPrint sig]
            , prettyPrint (Expression name body Nothing fixity)
            ]

instance (PrettyPrintable a) => PrettyPrintable (Method a) where
    prettyPrint (Method name type' body fixity) =
        multiplePrinters
            [ joinPrinters
                  [ prettyPrint name
                  , prettyPrint OperatorQDot
                  , prettyPrint type'
                  ]
            , case body of
                  Just t ->
                      joinPrinters
                          [ prettyPrint name
                          , prettyPrint OperatorEq
                          , prettyPrint t
                          ]
                  Nothing -> joinPrinters []
            , case fixity of
                  Just f ->
                      joinPrinters
                          [ prettyPrint name
                          , prettyPrint OperatorQDot
                          , prettyPrint f
                          ]
                  Nothing -> joinPrinters []
            ]

instance PrettyPrintable TypeSignature where
    prettyPrint (TypeSignature context type') =
        joinPrinters [inContext context, prettyPrint type']

instance PrettyPrintable Constraint where
    prettyPrint constraint =
        case constraint of
            ConstraintParam class' param -> notSep [class', param]
            ConstraintAppliedParam class' param params ->
                joinPrinters
                    [ prettyPrint class'
                    , inParens $
                      joinPrinters
                          [prettyPrint param, notSep $ NE.toList params]
                    ]

instance PrettyPrintable SimpleConstraint where
    prettyPrint (SimpleConstraint class' param) = notSep [class', param]

instance (PrettyPrintable a) => PrettyPrintable (Instance a) where
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

instance (PrettyPrintable a) => PrettyPrintable (Class a) where
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
    prettyPrint (Constructor name args fixity fields) =
        joinPrinters
            [ prettyPrint name
            , notSep args
            , case fixity of
                  Just t -> prettyPrint t
                  Nothing -> joinPrinters []
            , singleLine $ show fields
            ]

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

instance PrettyPrintable TypeSynonym where
    prettyPrint (TypeSynonym name params type') =
        joinPrinters
            [ prettyPrint KeywordType
            , prettyPrint name
            , notSep params
            , prettyPrint OperatorEq
            , prettyPrint type'
            ]

instance (PrettyPrintable a) => PrettyPrintable (Module a) where
    prettyPrint (Module name exports typeSynonyms dataTypes classes instances exps) =
        joinPrinters
            [ prettyPrint KeywordModule
            , prettyPrint name
            , prettyPrint exports
            , newLine
            , printIndentedMap typeSynonyms
            , newLine
            , printIndentedMap dataTypes
            , newLine
            , printIndentedMap classes
            , newLine
            , printList instances
            , newLine
            , printIndentedMap exps
            ]

instance (PrettyPrintable a) => Prettifiable (Module a) where
    prettify = prettifyAst

instance Prettifiable TypeSignature where
    prettify = prettifyAst

instance (PrettyPrintable a) => Prettifiable (Instance a) where
    prettify = prettifyAst

instance (PrettyPrintable a) => Prettifiable (Class a) where
    prettify = prettifyAst

instance (PrettyPrintable a) => Prettifiable (Expression a) where
    prettify = prettifyAst

instance Prettifiable TypeSynonym where
    prettify = prettifyAst

instance Prettifiable DataType where
    prettify = prettifyAst

instance Prettifiable Export where
    prettify = prettifyAst
