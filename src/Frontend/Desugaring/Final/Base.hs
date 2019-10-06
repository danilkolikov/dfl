{- |
Module      :  Frontend.Desugaring.Final.Base
Description :  Base functions for desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base functions for desugaring of expressions and related nodes
-}
module Frontend.Desugaring.Final.Base where

import Control.Monad.Trans.State (State, evalState, get, modify)
import Data.List.NonEmpty (NonEmpty)

import Frontend.Desugaring.Final.Ast
import qualified Frontend.Desugaring.Record.Ast as R
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Type for objects which can desugar expressions and raise errors
type ExpressionDesugaringProcessor = State Int

-- | Run an expression desugaring processor
runExpressionDesugaringProcessor :: ExpressionDesugaringProcessor a -> Int -> a
runExpressionDesugaringProcessor = evalState

-- | Generate new identifier
generateNewIdent :: ExpressionDesugaringProcessor (WithLocation Ident)
generateNewIdent = do
    counter <- get
    modify $ \st -> st + 1
    return .
        withDummyLocation .
        IdentGenerated .
        GeneratedIdent GeneratedIdentEnvironmentExpressionDesugaring $
        counter

-- | Statements in `do` blocks or in list comprehension
data PreparedStmt
    = PreparedStmtPattern (WithLocation R.Pattern)
                          (WithLocation Exp)
    | PreparedStmtLet (Expressions Exp)
    | PreparedStmtExp (WithLocation Exp)
    deriving (Show, Eq)

-- | Alternative in `case` expressions
data PreparedAlt
    = PreparedAltSimple (WithLocation R.Pattern) -- ^ Pattern
                        (WithLocation Exp) -- ^ Expression
    | PreparedAltGuarded (WithLocation R.Pattern) -- ^ Pattern
                         (NonEmpty (WithLocation PreparedGuardedExp)) -- ^ Guarded expression
                         (Expressions Exp) -- ^ Where block
    deriving (Show, Eq)

-- | Expression with a guard
data PreparedGuardedExp =
    PreparedGuardedExp (NonEmpty (WithLocation PreparedStmt)) -- ^ List of guards
                       (WithLocation Exp) -- ^ Expression
    deriving (Show, Eq)
