{- |
Module      :  Frontend.Desugaring.Processor
Description :  Functions for desugaring of AST
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with functions for desugaring of AST
-}
module Frontend.Desugaring.Processor
    ( desugarParsedModule
    , DesugaringError(..)
    , DesugaringState(..)
    ) where

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Desugaring.Final.ModuleDesugaring (desugarModule)
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Initial.ToModule (desugarToModule)
import qualified Frontend.Syntax.Ast as A
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar parsed module
desugarParsedModule ::
       A.Module
    -> DesugaringState
    -> Either DesugaringError (F.Module, DesugaringState)
desugarParsedModule parsedModule state =
    let initialModule = desugarToModule (withDummyLocation parsedModule)
        result =
            runDesugaringProcessor
                (desugarModule $ getValue initialModule)
                state
     in result
