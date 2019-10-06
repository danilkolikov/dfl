{- |
Module      :  Compiler.Prettify.Desugaring.Fixity
Description :  Prettifying of fixiity ast
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of fixity ast
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.Desugaring.Fixity where

import Compiler.Prettify.Ast ()
import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.PrettyPrinter
import Frontend.Desugaring.Fixity.Ast

instance PrettyPrintable Exp where
    prettyPrint = singleLine . show
