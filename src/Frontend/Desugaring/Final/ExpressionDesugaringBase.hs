{- |
Module      :  Frontend.Desugaring.Final.ExpressionDesugaringBase
Description :  Base structures for desugaring of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base structures for desugaring of expressions and related nodes
-}
module Frontend.Desugaring.Final.ExpressionDesugaringBase where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State (State, get, modify, runState)
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast
import qualified Frontend.Desugaring.Final.ResolvedAst as R
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Errors which may happen during desugaring of expressions
data ExpressionDesugaringError
    = ExpressionDesugaringErrorDuplicatedTypeDeclaration (WithLocation Ident) -- ^ Expression has multiple type declarations
    | ExpressionDesugaringErrorDuplicatedFixityDeclaration (WithLocation Ident) -- ^ Expression has multiple fixity declarations
    | ExpressionDesugaringErrorMissingExpressionDefinition (WithLocation Ident) -- ^ Definition of an expression is missing
    | ExpressionDesugaringErrorMissingMethodType (WithLocation Ident) -- ^ Definition of a method is missing a tupe signature
    | ExpressionDesugaringErrorDifferentNumberOfArguments (WithLocation Ident) -- ^ Declarations of a function have differnt number of arguments
    | ExpressionDesugaringErrorIdentifierIsAlreadyDefined (WithLocation Ident)
                                                          (WithLocation Ident) -- ^ Such identifier is already defined
    deriving (Eq, Show)

-- | Type for objects which can desugar expressions
type ExpressionDesugaringState a = State Int a

-- | Run an expression desugaring state object
runExpressionDesugaringState :: ExpressionDesugaringState a -> Int -> (a, Int)
runExpressionDesugaringState = runState

-- | Type for objects which can desugar expressions and raise errors
type ExpressionDesugaringProcessor a
     = ExceptT ExpressionDesugaringError (State Int) a

-- | Run an expression desugaring processor
runExpressionDesugaringProcessor ::
       ExpressionDesugaringProcessor a
    -> Int
    -> (Either ExpressionDesugaringError a, Int)
runExpressionDesugaringProcessor p = runState (runExceptT p)

-- | Generate new identifier
generateNewIdent :: ExpressionDesugaringState (WithLocation Ident)
generateNewIdent = do
    counter <- get
    modify $ \st -> st + 1
    return .
        withDummyLocation . IdentGenerated IdentEnvironmentExpressionDesugaring $
        counter

-- | Generate new identifier with dummy location
generateNewIdent' :: ExpressionDesugaringProcessor (WithLocation Ident)
generateNewIdent' = lift generateNewIdent

-- | Function raises a DesugaringError
raiseError :: ExpressionDesugaringError -> ExpressionDesugaringProcessor a
raiseError = throwE

-- | Assignment in top-level, `let` or `where` blocks
data PreparedAssignment
    = PreparedAssignmentName (WithLocation Ident)
                             (NE.NonEmpty (WithLocation R.Pattern))
                             (WithLocation Exp) -- ^ Assign some expression to a name
    | PreparedAssignmentPattern (WithLocation R.Pattern)
                                (WithLocation Exp) -- ^ Assign some expression to a pattern
    | PreparedAssignmentType (WithLocation Ident)
                             [WithLocation Constraint]
                             (WithLocation Type) -- ^ Define type of a name
    | PreparedAssignmentFixity (WithLocation Ident)
                               Fixity
                               Int -- ^ Defines fixity of an operator
    deriving (Show, Eq)

-- | Statements in `do` blocks or in list comprehension
data PreparedStmt
    = PreparedStmtPattern (WithLocation R.Pattern)
                          (WithLocation Exp)
    | PreparedStmtLet Expressions
    | PreparedStmtExp (WithLocation Exp)
    deriving (Show, Eq)

-- | Alternative in `case` expressions
data PreparedAlt
    = PreparedAltSimple (WithLocation R.Pattern) -- ^ Pattern
                        (WithLocation Exp) -- ^ Expression
    | PreparedAltGuarded (WithLocation R.Pattern) -- ^ Pattern
                         (NE.NonEmpty (WithLocation PreparedGuardedExp)) -- ^ Guarded expression
                         Expressions -- ^ Where block
    deriving (Show, Eq)

-- | Expression with a guard
data PreparedGuardedExp =
    PreparedGuardedExp (NE.NonEmpty (WithLocation PreparedStmt)) -- ^ List of guards
                       (WithLocation Exp) -- ^ Expression
    deriving (Show, Eq)
