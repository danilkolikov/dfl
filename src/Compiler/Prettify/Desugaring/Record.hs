{- |
Module      :  Compiler.Prettify.Desugaring.Record
Description :  Prettifying of record ast
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of record ast
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.Desugaring.Record where

import Compiler.Prettify.Ast ()
import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.PrettyPrinter
import Frontend.Desugaring.Record.Ast

instance PrettyPrintable Exp where
    prettyPrint = singleLine . show
