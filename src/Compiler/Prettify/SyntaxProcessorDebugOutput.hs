{- |
Module      :  Compiler.Prettify.SyntaxProcessorDebugOutput
Description :  Prettifying of FixityResolutionOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of FixityResolutionOutput
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.SyntaxProcessorDebugOutput where

import Compiler.Prettify.Ast ()
import Compiler.Prettify.TokenStream ()
import Compiler.Prettify.Utils
import Frontend.Syntax.HeaderProcessor
import Frontend.Syntax.Processor

instance Prettifiable SyntaxProcessorDebugOutput where
    prettify SyntaxProcessorDebugOutput { getSyntaxProcessorDebugOutputInfix = operators
                                        , getSyntaxProcessorDebugOutputInitialAst = initialAst
                                        , getSyntaxProcessorDebugOutputResolvedAst = resolvedAst
                                        } =
        unlineMaybes
            [ prettifyWithHeader "Initial AST: " <$> initialAst
            , prettifyWithHeader "Resolved AST:" <$> resolvedAst
            , prettifyWithHeader "Infix operators:" <$> operators
            ]

instance Prettifiable InfixOperator where
    prettify (InfixOperator _ fixity prec) =
        "fixity: " ++ show fixity ++ ", precedence: " ++ show prec

instance Prettifiable HeaderProcessorDebugOutput where
    prettify HeaderProcessorDebugOutput { getHeaderProcessorDebugOutputTokens = tokens
                                        , getHeaderProcessorDebugOutputHeader = header
                                        } =
        unlineMaybes
            [ prettifyWithHeader "Tokens:" <$> tokens
            , prettifyWithHeader "Header:" <$> header
            ]
