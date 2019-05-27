{- |
Module      :  Frontend.Desugaring.ToNewConstr
Description :  Desugaring of AST nodes to NewConstr
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing NewConstr-s.
-}
module Frontend.Desugaring.ToNewConstr where

import qualified Frontend.Desugaring.Ast as D
import Frontend.Desugaring.ToIdent (desugarToIdent)
import Frontend.Desugaring.ToType (desugarToType)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to NewConstr
class DesugarToNewConstr a where
    desugarToNewConstr :: a -> WithLocation D.NewConstr -- ^ Desugar object to NewConstr

instance (DesugarToNewConstr a) => DesugarToNewConstr (WithLocation a) where
    desugarToNewConstr = (getValue . desugarToNewConstr <$>)

instance DesugarToNewConstr NewConstr where
    desugarToNewConstr (NewConstrSimple name type') =
        withDummyLocation $
        D.NewConstrSimple (desugarToIdent name) (desugarToType type')
    desugarToNewConstr (NewConstrNamed name fName type') =
        withDummyLocation $
        D.NewConstrNamed
            (desugarToIdent name)
            (desugarToIdent fName)
            (desugarToType type')
