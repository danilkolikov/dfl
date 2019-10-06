{- |
Module      :  Compiler.Prettify.Desugaring.Processor
Description :  Prettifying of desugaring processor output
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of desugaring output
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.Desugaring.Processor where

import Compiler.Prettify.Desugaring.Ast ()
import Compiler.Prettify.Desugaring.Checking ()
import Compiler.Prettify.Desugaring.Final ()
import Compiler.Prettify.Desugaring.Fixity ()
import Compiler.Prettify.Desugaring.Grouping ()
import Compiler.Prettify.Desugaring.Initial ()
import Compiler.Prettify.Desugaring.Record ()
import Compiler.Prettify.Utils
import Frontend.Desugaring.Processor

instance Prettifiable DesugaringDebugOutput where
    prettify DesugaringDebugOutput { getDesugaringDebugOutputInitial = initial
                                   , getDesugaringDebugOutputGrouping = grouping
                                   , getDesugaringDebugOutputChecking = checking
                                   , getDesugaringDebugOutputFixity = fixity
                                   , getDesugaringDebugOutputRecord = record
                                   , getDesugaringDebugOutputFinal = final
                                   } =
        unlineMaybes
            [ prettifyWithHeader "Initial desugaring:" <$> initial
            , prettifyWithHeader "Grouping:" <$> grouping
            , prettifyWithHeader "Checking:" <$> checking
            , prettifyWithHeader "Fixity:" <$> fixity
            , prettifyWithHeader "Record:" <$> record
            , prettifyWithHeader "Final:" <$> final
            ]
