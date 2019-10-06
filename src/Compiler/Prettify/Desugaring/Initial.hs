{- |
Module      :  Compiler.Prettify.Desugaring.Initial
Description :  Prettifying of initial desugared ast
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of initial desugared ast
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.Desugaring.Initial where

import qualified Data.List.NonEmpty as NE (toList)

import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.PrettyPrinter
import Compiler.Prettify.Utils
import Core.Ident
import Frontend.Desugaring.Initial.Ast
import Frontend.Syntax.Token

instance PrettyPrintable UserDefinedIdent where
    prettyPrint = singleLine . prettify

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

instance (PrettyPrintable a) => Prettifiable (ImpExpList a) where
    prettify = prettifyAst

instance Prettifiable Module where
    prettify = show
