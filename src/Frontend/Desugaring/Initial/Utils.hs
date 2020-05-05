{- |
Module      :  Frontend.Desugaring.Initial.Utils
Description :  Utility functions for desugaring
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for the initial step of desugaring
-}
module Frontend.Desugaring.Initial.Utils where

import Core.Ident (UserDefinedIdent)
import Core.PredefinedIdents
import Frontend.Desugaring.Initial.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Make custom ident
makeIdent :: UserDefinedIdent -> WithLocation UserDefinedIdent
makeIdent = withDummyLocation

-- | Make custom type constructor
makeTypeConstr :: UserDefinedIdent -> WithLocation Type
makeTypeConstr = withDummyLocation . TypeConstr . withDummyLocation

-- | Make custom pattern constructor
makePattern :: UserDefinedIdent -> WithLocation Pattern
makePattern = withDummyLocation . (`PatternConstr` []) . withDummyLocation

-- | Make custom expression
makeExp :: UserDefinedIdent -> WithLocation Exp
makeExp = withDummyLocation . ExpVar . withDummyLocation

-- | Make custom constructor
makeConstr :: UserDefinedIdent -> WithLocation Exp
makeConstr = withDummyLocation . ExpConstr . withDummyLocation

-- | Wildcard pattern
wildcardPattern :: WithLocation Pattern
wildcardPattern = withDummyLocation PatternWildcard

-- | Pattern that matches True
truePattern :: WithLocation Pattern
truePattern = makePattern tRUE

-- | Pattern that matches False
falsePattern :: WithLocation Pattern
falsePattern = makePattern fALSE

-- | undefined
undefinedExp :: WithLocation Exp
undefinedExp = makeExp uNDEFINED
