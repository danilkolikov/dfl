{- |
Module      :  Frontend.Desugaring.Util
Description :  Helper functions for desugaring of AST
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Helper functions for desugaring of AST
-}
module Frontend.Desugaring.Util where

import qualified Data.List.NonEmpty as NE (NonEmpty(..), toList)

import Frontend.Desugaring.ToIdent(desugarToIdent)
import Frontend.Desugaring.ToConstraint(desugarToConstraint)
import Frontend.Desugaring.ToType(desugarToType)
import Frontend.Desugaring.AstWraps
import qualified Frontend.Desugaring.Ast as D
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)
import Frontend.Syntax.EntityName
import Frontend.Syntax.Token

desugarGenDecl ::
       (WithLocation D.Ident -> [WithLocation D.Constraint] -> WithLocation D.Type -> a)
    -> GenDecl
    -> [WithLocation a]
desugarGenDecl _ GenDeclFixity {} = []
desugarGenDecl wrap (GenDeclTypeSig vars context type') =
    let desugaredContext = map desugarToConstraint context
        desugaredType = desugarToType type'
     in NE.toList $
        fmap
            (\var ->
                 withDummyLocation $
                 wrap (desugarToIdent var) desugaredContext desugaredType)
            vars

desugarFunLHS ::
       FunLHS -> (WithLocation D.Ident, NE.NonEmpty (WithLocation APat))
desugarFunLHS (FunLHSSimple var pats) = (desugarToIdent var, pats)
desugarFunLHS (FunLHSInfix l op r) =
    (desugarToIdent op, wrapToAPat' l NE.:| [wrapToAPat' r])
desugarFunLHS (FunLHSNested lhs pats) =
    let (ident, f NE.:| rest) = desugarFunLHS (getValue lhs)
     in (ident, f NE.:| (rest ++ NE.toList pats))

rhsToExp :: RHS -> WithLocation Exp
rhsToExp (RHSSimple exp' decls) = declsToExp decls exp'
rhsToExp (RHSGuarded gdrhs decls) =
   let pat = combinePats []
       exp' = combineExps []
       alt =
           withDummyLocation $
           AltGuarded pat (fmap (gdRHSToGdPat <$>) gdrhs) decls
       otherwise' =
           withDummyLocation $ AltSimple wildcardPattern undefinedExp []
       case' = LExpCase exp' (alt NE.:| [otherwise'])
    in withDummyLocation . wrapToExp . wrapToInfixExp $ case'

combinePats :: [WithLocation Pat] -> WithLocation Pat
combinePats [] = withDummyLocation $ gConToPat GConUnit
combinePats [f] = f
combinePats (f:s:pats) =
    withDummyLocation . wrapToPat . wrapToLPat $ APatTuple f s pats

combineExps :: [WithLocation Exp] -> WithLocation Exp
combineExps [] = withDummyLocation $ gConToExp GConUnit
combineExps [f] = f
combineExps (f:s:rest) =
    withDummyLocation . wrapToExp . wrapToInfixExp . wrapToLExp $
    AExpTuple f s rest

wildcardPattern :: WithLocation Pat
wildcardPattern = withDummyLocation . wrapToPat . wrapToLPat $ APatWildcard

undefinedExp :: WithLocation Exp
undefinedExp =
    wrapToExp' . wrapToInfixExp' . wrapToLExp' . entityNameToAExp $
    uNDEFINED_NAME

entityNameToExp :: EntityName -> WithLocation D.Exp
entityNameToExp =
    withDummyLocation . D.ExpVar . withDummyLocation . D.IdentNamed

entityNameToAExp :: EntityName -> WithLocation AExp
entityNameToAExp = varIdToAExp . VarId . head

entityNameToPat :: EntityName -> WithLocation Pat
entityNameToPat =
    withDummyLocation .
    gConToPat .
    GConNamed . withDummyLocation . FuncLabelId . Qualified [] . ConId . head

varIdToAExp :: VarId -> WithLocation AExp
varIdToAExp =
    withDummyLocation .
    AExpVariable . withDummyLocation . FuncLabelId . Qualified []
