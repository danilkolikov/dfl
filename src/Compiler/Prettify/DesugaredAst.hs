{- |
Module      :  Compiler.Prettify.DesugaredAst
Description :  Pretty-printing of desugared AST
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty-printing of desugared AST
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.DesugaredAst where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE (toList)

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

instance (PrettyPrintable a) => PrettyPrintable (ImpExpList a) where
    prettyPrint list =
        case list of
            ImpExpNothing -> singleLine "()"
            ImpExpAll -> singleLine "(..)"
            ImpExpSome elems -> inParens . sepByComma . NE.toList $ elems

instance PrettyPrintable ImpDecl where
    prettyPrint (ImpDecl qualified name asName hiding list) =
        joinPrinters
            [ singleLine "import"
            , singleLine $
              if qualified
                  then "qualified"
                  else ""
            , prettyPrint name
            , case asName of
                  Nothing -> emptyLine
                  Just synonym ->
                      joinPrinters [singleLine "as", prettyPrint synonym]
            , singleLine $
              if hiding
                  then "hiding"
                  else ""
            , prettyPrint list
            ]

instance PrettyPrintable Import where
    prettyPrint imp =
        case imp of
            ImportFunction name -> prettyPrint name
            ImportDataOrClass name list ->
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

instance PrettyPrintable FixitySignature where
    prettyPrint (FixitySignature fixity prec) =
        joinPrinters [singleLine (show fixity), singleLine (show prec)]

instance PrettyPrintable Expression where
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

instance PrettyPrintable TypeSynonym where
    prettyPrint (TypeSynonym name params type') =
        joinPrinters
            [ prettyPrint KeywordType
            , prettyPrint name
            , notSep params
            , prettyPrint OperatorEq
            , prettyPrint type'
            ]

instance PrettyPrintable Module where
    prettyPrint (Module name exports imports typeSynonyms dataTypes classes instances exps) =
        joinPrinters
            [ prettyPrint KeywordModule
            , prettyPrint name
            , prettyPrint exports
            , multiplePrinters $ map prettyPrint imports
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

instance Prettifiable Module where
    prettify = prettifyAst

instance Prettifiable TypeSignature where
    prettify = prettifyAst

instance Prettifiable Instance where
    prettify = prettifyAst

instance PrettyPrintable Header where
    prettyPrint (Header name exports imports) =
        joinPrinters
            [ prettyPrint KeywordModule
            , prettyPrint name
            , prettyPrint exports
            , multiplePrinters $ map prettyPrint imports
            ]

instance Prettifiable Header where
    prettify = prettifyAst
