{- |
Module      :  Compiler.Prettify.DesugaringOutput
Description :  Prettifying of DesugaringOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of DesugaringOutput
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.DesugaringOutput where

import qualified Data.HashMap.Lazy as HM

import Compiler.Prettify.DesugaredAst ()
import Compiler.Prettify.Utils
import Frontend.Desugaring.Processor (DesugaringOutput(..), DesugaringState(..))

instance Prettifiable DesugaringOutput where
    prettify DesugaringOutput { getDesugaringOutputAst = ast
                              , getDesugaringOutputState = state
                              } =
        unlines'
            [prettifyWithHeader "AST:" ast, prettifyWithHeader "State:" state]

instance Prettifiable DesugaringState where
    prettify (DesugaringState typeNames classNames functionNames fields constructors) =
        let listKeys = unwords . map prettify . HM.keys
         in unlines
                [ "Types: " ++ listKeys typeNames
                , "Classes: " ++ listKeys classNames
                , "Functions: " ++ listKeys functionNames
                , "Fields: " ++ listKeys fields
                , "Constructors: " ++ listKeys constructors
                ]
