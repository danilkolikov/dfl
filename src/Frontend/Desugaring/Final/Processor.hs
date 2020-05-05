{- |
Module      :  Frontend.Desugaring.Final.Expression
Description :  Final desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of expressions
-}
module Frontend.Desugaring.Final.Processor
    ( desugarModule
    ) where

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Base
import Frontend.Desugaring.Final.Expression
import qualified Frontend.Desugaring.Record.Ast as R

-- | Does final desugaring of a module
desugarModule :: Module R.Exp -> Module Exp
desugarModule module' =
    runExpressionDesugaringProcessor (resolveModule module') 0

resolveModule :: Module R.Exp -> ExpressionDesugaringProcessor (Module Exp)
resolveModule = mapExpressionM desugarExp
