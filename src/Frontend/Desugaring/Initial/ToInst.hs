{- |
Module      :  Frontend.Desugaring.Initial.ToInst
Description :  Desugaring of AST nodes to Inst
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Insts-s.
-}
module Frontend.Desugaring.Initial.ToInst
    ( desugarToInst
    ) where

import Data.Functor (($>))

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.Utils
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..))

-- | Desugar object to Inst
desugarToInst :: WithLocation Inst -> WithLocation D.Inst
desugarToInst inst =
    inst $>
    case getValue inst of
        InstNamed name vars ->
            D.Inst (desugarToIdent name) (map desugarToIdent vars)
        InstTuple f s rest ->
            D.Inst
                (makeIdent' $ D.IdentParametrised tUPLE_NAME (length rest + 2))
                (map desugarToIdent (f : s : rest))
        InstList t -> D.Inst (makeIdent lIST_NAME) [desugarToIdent t]
        InstFunction from to ->
            D.Inst (makeIdent fUNCTION_NAME) (map desugarToIdent [from, to])
