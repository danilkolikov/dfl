{- |
Module      :  Frontend.Desugaring.Final.Util
Description :  Utility functions for desugaring
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for the final step of desugaring
-}
module Frontend.Desugaring.Final.Util where

import Core.PredefinedIdents
import Frontend.Desugaring.Final.Ast
import qualified Frontend.Desugaring.Record.Ast as R
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Make custom pattern constructor
makeIdent :: UserDefinedIdent -> WithLocation Ident
makeIdent = withDummyLocation . IdentUserDefined

-- | Make arbitrary expression
makeExp :: UserDefinedIdent -> WithLocation Exp
makeExp = withDummyLocation . ExpVar . makeIdent

-- | undefined
undefinedExp :: WithLocation Exp
undefinedExp = makeExp uNDEFINED

-- | True
trueIdent :: WithLocation Ident
trueIdent = makeIdent tRUE

-- | Make a tuple of the provided size
makeTuple :: Int -> WithLocation Ident
makeTuple = makeIdent . tUPLE

-- | Make constructors
makeConstr :: UserDefinedIdent -> WithLocation Exp
makeConstr = withDummyLocation . ExpConstr . makeIdent

-- | Make a pattern
makeRPattern :: UserDefinedIdent -> WithLocation R.Pattern
makeRPattern = withDummyLocation . (`R.PatternConstr` []) . makeIdent

-- | Constructor of an empty list
emptyList :: WithLocation Exp
emptyList = makeConstr lIST
