{- |
Module      :  Frontend.Desugaring.Final.Util
Description :  Utility functions for desugaring
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for the final step of desugaring
-}
module Frontend.Desugaring.Final.Util where

import Frontend.Desugaring.Final.Ast
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Desugar a constraint
desugarConstraint :: WithLocation I.Constraint -> WithLocation Constraint
desugarConstraint c
    | (I.Constraint name params paramArgs) <- getValue c =
        Constraint name params paramArgs <$ c

desugarSimpleClass :: WithLocation I.SimpleClass -> WithLocation SimpleConstraint
desugarSimpleClass c
  | (I.SimpleClass name params) <- getValue c = SimpleConstraint name params <$ c

-- | Make custom pattern constructor
makePattern :: EntityName -> WithLocation Pattern
makePattern =
    withDummyLocation . (`PatternConstr` []) . withDummyLocation . IdentNamed

-- | Pattern that matches True
truePattern :: WithLocation Pattern
truePattern = makePattern tRUE_NAME

-- | Pattern that matches False
falsePattern :: WithLocation Pattern
falsePattern = makePattern fALSE_NAME

-- | Make arbitrary expression
makeExp :: EntityName -> WithLocation Exp
makeExp = withDummyLocation . ExpVar . withDummyLocation . IdentNamed

-- | undefined
undefinedExp :: WithLocation Exp
undefinedExp = makeExp uNDEFINED_NAME

-- | Make constructors
makeConstr :: EntityName -> WithLocation Exp
makeConstr = withDummyLocation . ExpConstr . withDummyLocation . IdentNamed

makeTuple :: Int -> WithLocation Ident
makeTuple = withDummyLocation . IdentParametrised tUPLE_NAME
