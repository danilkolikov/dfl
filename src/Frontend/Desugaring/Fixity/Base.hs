{- |
Module      :  Frontend.Desugaring.Fixity.Base
Description :  Base functions for fixity resolution
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base functions for fixity resolution
-}
module Frontend.Desugaring.Fixity.Base where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)

import Core.PredefinedIdents
import Frontend.Desugaring.Fixity.Ast
import Frontend.Syntax.Position (SourceLocation(..), WithLocation(..))

-- | Predefined operator for the unary minus
minusFixitySignature :: FixitySignature
minusFixitySignature =
    FixitySignature
        {getFixitySignatureFixity = InfixL, getFixitySignaturePrecedence = 6}

-- | A default fixity signature
defaultFixitySignature :: FixitySignature
defaultFixitySignature =
    FixitySignature
        {getFixitySignatureFixity = InfixL, getFixitySignaturePrecedence = 9}

-- | By default, we assume that implicitly defined operators have left fixity,
--   and their precedence is 9
getUndefinedFixitySignature :: Ident -> FixitySignature
getUndefinedFixitySignature name
    | isMinus name = minusFixitySignature -- Fixity is defined for "-"
    | otherwise = defaultFixitySignature

-- | Checks that an identifier is a minus
isMinus :: Ident -> Bool
isMinus name
    | IdentUserDefined ud <- name = ud == mINUS
    | otherwise = False

-- | Errors which can be raised during resolution of fixity
data FixityResolutionError
    = FixityResolutionErrorMissingOperand SourceLocation -- ^ Binary expression misses an operand
    | FixityResolutionErrorMissingOperator SourceLocation -- ^ Binary expression misses an operator
    | FixityResolutionErrorUnexpectedOperator (WithLocation Ident) -- ^ There is an unexpected operator in a binary expression
    | FixityResolutionErrorFixityConflict (WithLocation Ident) -- ^ Encountered fixity conflict
    | FixityResolutionErrorCannotResolve SourceLocation -- ^ Can't resolve fixity of an expression
    deriving (Show, Eq)

-- | Map of infix operators
type InfixOperators = HM.HashMap Ident FixitySignature

-- | Function looks up a specified operator in a stack.
--   It starts from the top layer, and if this operator is undefined,
--   a 'defaultFixitySignature' is returned.
infixOperatorsLookup :: Ident -> InfixOperators -> FixitySignature
infixOperatorsLookup opName ops =
    fromMaybe (getUndefinedFixitySignature opName) (HM.lookup opName ops)

-- | Resolver of fixity
type FixityResolver = ReaderT InfixOperators (Except FixityResolutionError)

-- | Function runs fixity resolver
runFixityResolver ::
       FixityResolver a -> InfixOperators -> Either FixityResolutionError a
runFixityResolver r st = runExcept (runReaderT r st)

-- | Looks up an operator
lookupOperator :: WithLocation Ident -> FixityResolver FixitySignature
lookupOperator name = infixOperatorsLookup (getValue name) <$> ask

-- | Defines operators
defineOperators :: InfixOperators -> FixityResolver a -> FixityResolver a
defineOperators ops = local (`HM.union` ops)

-- | Raises an error
raiseError :: FixityResolutionError -> FixityResolver a
raiseError = lift . throwE

-- | Type for flattened binary expressions
data FlatInfix a
    = FlatInfixExp (WithLocation a) -- ^ Expression
    | FlatInfixOp (WithLocation Ident) -- ^ Operator
    deriving (Show, Eq)
