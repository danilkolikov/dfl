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
    , DesugaringOutput(..)
    , DesugaringState(..)
    , RecordDesugaringError(..)
    , ExpressionDesugaringError(..)
    , emptyDesugaringState
    ) where

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Desugaring.Final.ExpressionDesugaringBase
import Frontend.Desugaring.Final.ModuleDesugaring (desugarModule)
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Final.RecordDesugaring
import Frontend.Desugaring.Initial.ToModule (desugarToModule)
import qualified Frontend.Syntax.Ast as A

-- | Result of desugaring
data DesugaringOutput = DesugaringOutput
    { getDesugaringOutputAst :: F.Module -- ^ Desugared AST
    , getDesugaringOutputState :: DesugaringState -- ^ State of desugaring
    } deriving (Eq, Show)

-- | Desugar parsed module
desugarParsedModule ::
       A.Module A.Body
    -> DesugaringState
    -> Either DesugaringError DesugaringOutput
desugarParsedModule parsedModule state =
    let initialModule = desugarToModule parsedModule
        result = runDesugaringProcessor (desugarModule initialModule) state
     in uncurry DesugaringOutput <$> result
