{- |
Module      :  Frontend.Desugaring.Final.Utils
Description :  Utility functions for desugaring
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for the final step of desugaring
-}
module Frontend.Desugaring.Final.Utils where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast
import qualified Frontend.Desugaring.Final.ResolvedAst as R
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar a constraint
desugarConstraint :: WithLocation I.Constraint -> WithLocation Constraint
desugarConstraint c
    | (I.Constraint name params paramArgs) <- getValue c =
        c $>
        case paramArgs of
            [] -> ConstraintParam name params
            (s:rest) -> ConstraintType name params (s NE.:| rest)

-- | Desugar a simple class
desugarSimpleClass ::
       WithLocation I.SimpleClass -> WithLocation SimpleConstraint
desugarSimpleClass c
    | (I.SimpleClass name params) <- getValue c =
        c $> SimpleConstraint name params

-- | Make custom pattern constructor
makeIdent :: EntityName -> WithLocation Ident
makeIdent = withDummyLocation . IdentNamed

-- | Make arbitrary expression
makeExp :: EntityName -> WithLocation Exp
makeExp = withDummyLocation . ExpVar . withDummyLocation . IdentNamed

-- | undefined
undefinedExp :: WithLocation Exp
undefinedExp = makeExp uNDEFINED_NAME

-- | True
trueIdent :: WithLocation Ident
trueIdent = makeIdent tRUE_NAME

-- | Make constructors
makeConstr :: EntityName -> WithLocation Exp
makeConstr = withDummyLocation . ExpConstr . withDummyLocation . IdentNamed

-- | Make a tuple of the provided size
makeTuple :: Int -> WithLocation Ident
makeTuple = withDummyLocation . IdentParametrised tUPLE_NAME

-- | Make a pattern
makeRPattern :: EntityName -> WithLocation R.Pattern
makeRPattern =
    withDummyLocation . (`R.PatternConstr` []) . withDummyLocation . IdentNamed

-- | Constructor of an empty list
emptyList :: WithLocation Exp
emptyList = makeConstr lIST_NAME
