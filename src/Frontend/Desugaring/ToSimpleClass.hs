{- |
Module      :  Frontend.Desugaring.ToSimpleClass
Description :  Desugaring of AST nodes to SimpleClass
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing SimpleClass-s.
-}
module Frontend.Desugaring.ToSimpleClass
    ( DesugarToSimpleClass(..)
    ) where

import qualified Frontend.Desugaring.Ast as D
import Frontend.Desugaring.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for objects which can be desugared to SimpleClass
class DesugarToSimpleClass a where
    desugarToSimpleClass :: a -> WithLocation D.SimpleClass -- ^ Desugar object to SimpleClass

instance (DesugarToSimpleClass a) => DesugarToSimpleClass (WithLocation a) where
    desugarToSimpleClass = (getValue . desugarToSimpleClass <$>)

instance DesugarToSimpleClass SimpleClass where
    desugarToSimpleClass (SimpleClass name var) =
        withDummyLocation $
        D.SimpleClass (desugarToIdent name) (desugarToIdent var)
