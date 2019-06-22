{- |
Module      :  Frontend.Desugaring.Initial.Utils
Description :  Utility functions for desugaring
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for the initial step of desugaring
-}
module Frontend.Desugaring.Initial.Utils where

import Frontend.Desugaring.Initial.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Make custom named ident
makeIdent :: EntityName -> WithLocation Ident
makeIdent = makeIdent' . IdentNamed

-- | Make custom ident
makeIdent' :: Ident -> WithLocation Ident
makeIdent' = withDummyLocation

-- | Make custom named type constructor
makeTypeConstr :: EntityName -> WithLocation Type
makeTypeConstr = makeTypeConstr' . IdentNamed

-- | Make custom type constructor
makeTypeConstr' :: Ident -> WithLocation Type
makeTypeConstr' = withDummyLocation . TypeConstr . withDummyLocation

-- | Make custom named pattern constructor
makePattern :: EntityName -> WithLocation Pattern
makePattern = makePattern' . IdentNamed

-- | Make custom pattern constructor
makePattern' :: Ident -> WithLocation Pattern
makePattern' = withDummyLocation . (`PatternConstr` []) . withDummyLocation

-- | Make custom named expression
makeExp :: EntityName -> WithLocation Exp
makeExp = makeExp' . IdentNamed

-- | Make custom expression
makeExp' :: Ident -> WithLocation Exp
makeExp' = withDummyLocation . ExpVar . withDummyLocation

-- | Make custom named constructor
makeConstr :: EntityName -> WithLocation Exp
makeConstr = makeConstr' . IdentNamed

-- | Make custom constructor
makeConstr' :: Ident -> WithLocation Exp
makeConstr' = withDummyLocation . ExpConstr . withDummyLocation

-- | Wildcard pattern
wildcardPattern :: WithLocation Pattern
wildcardPattern = withDummyLocation PatternWildcard

-- | Pattern that matches True
truePattern :: WithLocation Pattern
truePattern = makePattern tRUE_NAME

-- | Pattern that matches False
falsePattern :: WithLocation Pattern
falsePattern = makePattern fALSE_NAME

-- | undefined
undefinedExp :: WithLocation Exp
undefinedExp = makeExp uNDEFINED_NAME
