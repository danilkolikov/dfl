{- |
Module      :  Frontend.Desugaring.Initial.ToExp
Description :  Desugaring of AST nodes to Exp and Assignments
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Exp-s and Assignment-s.

Module contains functions for these 2 objects, because they're mutually recursive
-}
module Frontend.Desugaring.Initial.ToExp
    ( DesugarToExp(..)
    , desugarToAssignment
    , desugarFunLHS
    , desugarGenDecl
    , desugarToAlt
    , desugarStmt
    , desugarQual
    , desugarOperator
    , desugarToBinding
    , desugarGuard
    , desugarGdPat
    , withDecls
    ) where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE (NonEmpty(..), init, last, toList)

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToConst (desugarToConst)
import Frontend.Desugaring.Initial.ToConstraint (desugarToConstraint)
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.ToPattern (desugarToPattern)
import Frontend.Desugaring.Initial.ToType (desugarToType)
import Frontend.Desugaring.Initial.Utils
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token

-- | Desugar declaration to Assignment
desugarToAssignment :: WithLocation Decl -> [WithLocation D.Assignment] -- ^ Desugar object to assignments
desugarToAssignment decl =
    map (decl $>) $
    case getValue decl of
        (DeclGenDecl genDecl) ->
            desugarGenDecl D.AssignmentType D.AssignmentFixity genDecl
        (DeclFunction lhs rhs) ->
            [ case getValue lhs of
                  Left fun ->
                      let (ident, pats) = desugarFunLHS fun
                          desugaredRHS = desugarToExp rhs
                       in D.AssignmentName ident pats desugaredRHS
                  Right pat ->
                      let desugaredPat = desugarToPattern (lhs $> pat)
                          desugaredRHS = desugarToExp rhs
                       in D.AssignmentPattern desugaredPat desugaredRHS
            ]

-- | Class for types which can be desugared to Exp
class DesugarToExp a where
    desugarToExp :: WithLocation a -> WithLocation D.Exp -- ^ Desugar object to exp

instance DesugarToExp RHS where
    desugarToExp rhs =
        rhs $>
        case getValue rhs of
            RHSSimple exp' decls ->
                let desugaredExp = desugarToExp exp'
                    desugaredDecls = concatMap desugarToAssignment decls
                 in withDecls desugaredDecls desugaredExp
            RHSGuarded gdrhs decls ->
                let desugaredDecls = concatMap desugarToAssignment decls
                    gdRHSToGdPat (GdRHS pat exp') = GdPat pat exp'
                    guardedExp = fmap (desugarGdPat . (gdRHSToGdPat <$>)) gdrhs
                    unitPat = makePattern uNIT_NAME
                    alt = rhs $> D.AltGuarded unitPat guardedExp desugaredDecls
                    unitExp = makeExp uNIT_NAME
                 in D.ExpCase unitExp (alt NE.:| [])

instance DesugarToExp Exp where
    desugarToExp exp' =
        case getValue exp' of
            ExpSimple e -> desugarToExp e
            ExpTyped e context type' ->
                let desugaredE = desugarToExp e
                    desugaredContext = map desugarToConstraint context
                    desugaredType = desugarToType type'
                 in exp' $> D.ExpTyped desugaredE desugaredContext desugaredType

instance DesugarToExp InfixExp where
    desugarToExp infixExp =
        case getValue infixExp of
            InfixExpLExp exp' -> desugarToExp exp'
            InfixExpNegated _ exp' ->
                let desugaredExp = desugarToExp exp'
                 in infixExp $>
                    D.ExpApplication
                        (makeExp nEGATE_NAME)
                        (desugaredExp NE.:| [])
            InfixExpApplication l op r ->
                let func = desugarOperator op
                    args = fmap desugarToExp (l NE.:| [r])
                 in infixExp $> D.ExpApplication func args

instance DesugarToExp LExp where
    desugarToExp lExp =
        lExp $>
        case getValue lExp of
            LExpApplication args ->
                case args of
                    f NE.:| [] -> getValue $ desugarToExp f
                    f NE.:| (s:rest) ->
                        D.ExpApplication
                            (desugarToExp f)
                            (fmap desugarToExp (s NE.:| rest))
            LExpAbstraction pats exp' ->
                let desugaredPatterns = fmap desugarToPattern pats
                    desugaredExp = desugarToExp exp'
                 in D.ExpAbstraction desugaredPatterns desugaredExp
            LExpLet decls exp' ->
                let desugaredDecls = concatMap desugarToAssignment decls
                    desugaredExp = desugarToExp exp'
                 in withDecls desugaredDecls desugaredExp
            LExpIf cond true false ->
                let desugaredCond = desugarToExp cond
                    desugaredTrue = desugarToExp true
                    desugaredFalse = desugarToExp false
                    trueAlt =
                        desugaredTrue $> D.AltSimple truePattern desugaredTrue
                    falseAlt =
                        desugaredFalse $>
                        D.AltSimple falsePattern desugaredFalse
                 in D.ExpCase desugaredCond (trueAlt NE.:| [falseAlt])
            LExpCase exp' alts ->
                let desugaredExp = desugarToExp exp'
                    desugaredAlts = fmap desugarToAlt alts
                 in D.ExpCase desugaredExp desugaredAlts
            LExpDo stmts ->
                let desugaredStmts = fmap desugarStmt stmts
                    initialStmts = NE.init desugaredStmts
                    lastStmt = NE.last desugaredStmts
                 in case getValue lastStmt of
                        D.StmtExp exp' -> D.ExpDo initialStmts exp'
                        -- This case should have been filtered earlier
                        _ -> error "Unexpected statement in do"

instance DesugarToExp AExp where
    desugarToExp aExp =
        aExp $>
        case getValue aExp of
            AExpVariable name -> D.ExpVar (desugarToIdent name)
            AExpConstructor name -> D.ExpConstr (desugarToIdent name)
            AExpLiteral lit -> D.ExpConst (desugarToConst lit)
            AExpParens exp' -> getValue $ desugarToExp exp'
            AExpTuple f s rest ->
                let args = f NE.:| (s : rest)
                    desugaredArgs = fmap desugarToExp args
                    function =
                        makeExp' $
                        D.IdentParametrised tUPLE_NAME $ length rest + 2
                 in D.ExpApplication function desugaredArgs
            AExpList (f NE.:| rest) ->
                let desugaredL = desugarToExp f
                    desugaredR =
                        case rest of
                            [] -> makeConstr lIST_NAME
                            (s:exps) ->
                                desugarToExp (aExp $> AExpList (s NE.:| exps))
                    function = makeConstr cOLON_NAME
                 in D.ExpApplication function (desugaredL NE.:| [desugaredR])
            AExpSequence f s e ->
                let desugaredF = desugarToExp f
                    desugaredS = desugarToExp <$> s
                    desugaredE = desugarToExp <$> e
                 in case (desugaredS, desugaredE) of
                        (Nothing, Nothing) ->
                            let function = makeExp eNUM_FROM_NAME
                             in D.ExpApplication function (desugaredF NE.:| [])
                        (Just s', Nothing) ->
                            let function = makeExp eNUM_FROM_THEN_NAME
                             in D.ExpApplication
                                    function
                                    (desugaredF NE.:| [s'])
                        (Nothing, Just e') ->
                            let function = makeExp eNUM_FROM_TO_NAME
                             in D.ExpApplication
                                    function
                                    (desugaredF NE.:| [e'])
                        (Just s', Just e') ->
                            let function = makeExp eNUM_FROM_THEN_TO_NAME
                             in D.ExpApplication
                                    function
                                    (desugaredF NE.:| [s', e'])
            AExpListCompr exp' quals ->
                let desugaredExp = desugarToExp exp'
                    desugaredQuals = fmap desugarQual quals
                 in D.ExpListCompr desugaredExp desugaredQuals
            AExpLeftSection exp' op ->
                let desugaredExp = desugarToExp exp'
                    desugaredOp = desugarOperator op
                 in D.ExpLeftSection desugaredExp desugaredOp
            AExpRightSection op exp' ->
                let desugaredOp = desugarOperator op
                    desugaredExp = desugarToExp exp'
                 in D.ExpRightSection desugaredOp desugaredExp
            AExpRecordConstr name binds ->
                let desugaredName = desugarToIdent name
                    desugaredBinds = map desugarToBinding binds
                 in D.ExpRecordConstr desugaredName desugaredBinds
            AExpRecordUpdate exp' binds ->
                let desugaredExp = desugarToExp exp'
                    desugaredBinds = fmap desugarToBinding binds
                 in D.ExpRecordUpdate desugaredExp desugaredBinds

-- Helper functions
-- | Desugar expression to Alt
desugarToAlt :: WithLocation Alt -> WithLocation D.Alt
desugarToAlt alt =
    alt $>
    case getValue alt of
        AltSimple pat exp' decls ->
            let desugaredPat = desugarToPattern pat
                desugaredExp = desugarToExp exp'
                desugaredDecls = concatMap desugarToAssignment decls
                expWithDecls = exp' $> withDecls desugaredDecls desugaredExp
             in D.AltSimple desugaredPat expWithDecls
        AltGuarded pat gdpats decls ->
            let desugaredPat = desugarToPattern pat
                desugaredDecls = concatMap desugarToAssignment decls
                rhs = fmap desugarGdPat gdpats
             in D.AltGuarded desugaredPat rhs desugaredDecls

-- | Desugar expression to bindings
desugarToBinding :: WithLocation FBind -> WithLocation D.Binding
desugarToBinding fbind
    | FBind name exp' <- getValue fbind =
        fbind $> D.Binding (desugarToIdent name) (desugarToExp exp')

-- | Desugar statements
desugarStmt :: WithLocation Stmt -> WithLocation D.Stmt
desugarStmt stmt =
    stmt $>
    case getValue stmt of
        StmtPat pat exp' ->
            let desugaredPat = desugarToPattern pat
                desugaredExp = desugarToExp exp'
             in D.StmtPattern desugaredPat desugaredExp
        StmtLet decls ->
            let desugaredDecls = concatMap desugarToAssignment decls
             in D.StmtLet desugaredDecls
        StmtExp exp' ->
            let desugaredExp = desugarToExp exp'
             in D.StmtExp desugaredExp

-- | Desugar qualifiers
desugarQual :: WithLocation Qual -> WithLocation D.Stmt
desugarQual stmt =
    stmt $>
    case getValue stmt of
        QualGenerator pat exp' ->
            let desugaredPat = desugarToPattern pat
                desugaredExp = desugarToExp exp'
             in D.StmtPattern desugaredPat desugaredExp
        QualLet decls ->
            let desugaredDecls = concatMap desugarToAssignment decls
             in D.StmtLet desugaredDecls
        QualGuard exp' ->
            let desugaredExp = desugarToExp exp'
             in D.StmtExp desugaredExp

-- | Desugar guards
desugarGuard :: WithLocation Guard -> WithLocation D.Stmt
desugarGuard stmt =
    stmt $>
    case getValue stmt of
        GuardPattern pat exp' ->
            let desugaredPat = desugarToPattern pat
                desugaredExp = desugarToExp exp'
             in D.StmtPattern desugaredPat desugaredExp
        GuardLet decls ->
            let desugaredDecls = concatMap desugarToAssignment decls
             in D.StmtLet desugaredDecls
        GuardExpr exp' ->
            let desugaredExp = desugarToExp exp'
             in D.StmtExp desugaredExp

-- | Desugar GdPat
desugarGdPat :: WithLocation GdPat -> WithLocation D.GuardedExp
desugarGdPat gdPat
    | GdPat guards exp' <- getValue gdPat =
        let desugaredGuards = fmap desugarGuard guards
            desugaredExp = desugarToExp exp'
         in gdPat $> D.GuardedExp desugaredGuards desugaredExp

desugarFixity :: Fixity -> D.Fixity
desugarFixity fixity =
    case fixity of
        Infix -> D.Infix
        InfixL -> D.InfixL
        InfixR -> D.InfixR

-- | Desugar fixty and type declarations
desugarGenDecl ::
       (WithLocation D.Ident -> [WithLocation D.Constraint] -> WithLocation D.Type -> a)
    -> (WithLocation D.Ident -> D.Fixity -> Int -> a)
    -> WithLocation GenDecl
    -> [a]
desugarGenDecl wrapType wrapFixity genDecl =
    case getValue genDecl of
        GenDeclFixity fixity prec ops ->
            let (IntT prec') = getValue prec
                fixity' = getValue fixity
             in NE.toList $
                fmap
                    (\var ->
                         wrapFixity
                             (desugarToIdent var)
                             (desugarFixity fixity')
                             prec')
                    ops
        GenDeclTypeSig vars context type' ->
            let desugaredContext = map desugarToConstraint context
                desugaredType = desugarToType type'
             in NE.toList $
                fmap
                    (\var ->
                         wrapType
                             (desugarToIdent var)
                             desugaredContext
                             desugaredType)
                    vars

-- | Desugar left hand side of a function
desugarFunLHS ::
       FunLHS -> (WithLocation D.Ident, NE.NonEmpty (WithLocation D.Pattern))
desugarFunLHS (FunLHSSimple var pats) =
    (desugarToIdent var, fmap desugarToPattern pats)
desugarFunLHS (FunLHSInfix l op r) =
    (desugarToIdent op, fmap desugarToPattern (l NE.:| [r]))
desugarFunLHS (FunLHSNested lhs pats) =
    let (ident, f NE.:| rest) = desugarFunLHS (getValue lhs)
     in (ident, f NE.:| (rest ++ NE.toList (fmap desugarToPattern pats)))

-- | Make let block
withDecls :: [WithLocation D.Assignment] -> WithLocation D.Exp -> D.Exp
withDecls [] = getValue
withDecls decls = D.ExpLet decls

-- | Wrap operator to an expression
desugarOperator :: WithLocation QOp -> WithLocation D.Exp
desugarOperator op =
    let desugared = desugarToIdent op
        wrapper =
            case getValue op of
                Left _ -> D.ExpVar
                Right _ -> D.ExpConstr
     in op $> wrapper desugared
