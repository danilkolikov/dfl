{- |
Module      :  Frontend.Desugaring.ToInst
Description :  Desugaring of AST nodes to Inst
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Insts-s.
-}
module Frontend.Desugaring.ToInst
    ( DesugarToInst(..)
    ) where

import qualified Frontend.Desugaring.Ast as D
import Frontend.Desugaring.ToIdent (desugarToIdent)
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for objects which can be desugared to Inst-s
class DesugarToInst a where
    desugarToInst :: a -> WithLocation D.Inst -- ^ Desugar object to Inst

instance (DesugarToInst a) => DesugarToInst (WithLocation a) where
    desugarToInst = (getValue . desugarToInst <$>)

instance DesugarToInst Inst where
    desugarToInst (InstNamed name vars) =
        withDummyLocation $
        D.Inst (desugarToIdent name) (map desugarToIdent vars)
    desugarToInst (InstTuple f s rest) =
        withDummyLocation $
        D.Inst
            (withDummyLocation $
             D.IdentParametrised tUPLE_NAME (length rest + 2))
            (map desugarToIdent (f : s : rest))
    desugarToInst (InstList t) =
        withDummyLocation $
        D.Inst (withDummyLocation $ D.IdentNamed lIST_NAME) [desugarToIdent t]
    desugarToInst (InstFunction from to) =
        withDummyLocation $
        D.Inst
            (withDummyLocation $ D.IdentNamed fUNCTION_NAME)
            (map desugarToIdent [from, to])
