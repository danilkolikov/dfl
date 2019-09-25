{- |
Module      :  Compiler.Prettify.DesugaringOutput
Description :  Prettifying of DesugaringOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of DesugaringOutput
-}
module Compiler.Prettify.DesugaringOutput where

import qualified Data.HashMap.Lazy as HM

import Compiler.Prettify.DesugaredAst ()
import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.Utils
import Frontend.Desugaring.Processor (DesugaringOutput(..), DesugaringState(..))

prettifyDesugaringOutput :: DesugaringOutput -> String
prettifyDesugaringOutput DesugaringOutput { getDesugaringOutputAst = ast
                                          , getDesugaringOutputState = state
                                          } =
    let astHeader = prettifyHeader "AST:"
        prettyAst = prettifyAst ast
        stateHeader = prettifyHeader "State:"
        prettyState = prettifyState state
     in unlines [astHeader, prettyAst, stateHeader, prettyState]

prettifyState :: DesugaringState -> String
prettifyState (DesugaringState typeNames classNames functionNames fields constructors) =
    let listKeys = unwords . map prettify . HM.keys
     in unlines
            [ "Types: " ++ listKeys typeNames
            , "Classes: " ++ listKeys classNames
            , "Functions: " ++ listKeys functionNames
            , "Fields: " ++ listKeys fields
            , "Constructors: " ++ listKeys constructors
            ]
