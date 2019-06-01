{- |
Module      :  Frontend.Desugaring.Initial.ToExp
Description :  Desugaring of AST nodes to Exp and Assignments
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Exp-s and Assignment-s.

Module contains functions for these 2 objects, because they're mutually recursive
-}
module Frontend.Desugaring.Initial.ToExp
    ( DesugarToAssignment(..)
    , DesugarToExp(..)
    , desugarFunLHS
    , desugarGenDecl
    ) where

import Data.Functor ((<$))
import qualified Data.List.NonEmpty as NE (NonEmpty(..), init, last, toList)

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToConst (desugarToConst)
import Frontend.Desugaring.Initial.ToConstraint (desugarToConstraint)
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.ToPattern (desugarToPattern)
import Frontend.Desugaring.Initial.ToType (desugarToType)
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to Assignment-s
class DesugarToAssignment a where
    desugarToAssignment :: a -> [WithLocation D.Assignment] -- ^ Desugar object to assignments

instance (DesugarToAssignment a) => DesugarToAssignment (WithLocation a) where
    desugarToAssignment = sequence . ((getValue <$>) . desugarToAssignment <$>)

instance DesugarToAssignment Decl where
    desugarToAssignment (DeclGenDecl genDecl) = desugarToAssignment genDecl
    desugarToAssignment (DeclFunction lhs rhs) =
        [ withDummyLocation $
          case getValue lhs of
              Left fun ->
                  let (ident, pats) = desugarFunLHS fun
                      desugaredRHS = desugarToExp rhs
                   in D.AssignmentName ident pats desugaredRHS
              Right pat ->
                  let desugaredPat = desugarToPattern pat
                      desugaredRHS = desugarToExp rhs
                   in D.AssignmentPattern desugaredPat desugaredRHS
        ]

instance DesugarToAssignment GenDecl where
    desugarToAssignment = desugarGenDecl D.AssignmentType

-- | Class for types which can be desugared to Exp
class DesugarToExp a where
    desugarToExp :: a -> WithLocation D.Exp -- ^ Desugar object to exp

instance (DesugarToExp a) => DesugarToExp (WithLocation a) where
    desugarToExp = (getValue . desugarToExp <$>)

instance DesugarToExp RHS where
    desugarToExp (RHSSimple exp' decls) =
        let desugaredExp = desugarToExp exp'
            desugaredDecls = concatMap desugarToAssignment decls
         in withDecls desugaredDecls desugaredExp
    desugarToExp (RHSGuarded gdrhs decls) =
        let gdRHSToGdPat (GdRHS pat exp') = GdPat pat exp'
            desugaredGuards = desugarGdPats $ fmap (gdRHSToGdPat <$>) gdrhs
            desugaredDecls = concatMap desugarToAssignment decls
         in withDecls desugaredDecls desugaredGuards

instance DesugarToExp Exp where
    desugarToExp (ExpSimple exp') = desugarToExp exp'
    desugarToExp (ExpTyped exp' context type') =
        let desugaredE = desugarToExp exp'
            desugaredContext = map desugarToConstraint context
            desugaredType = desugarToType type'
         in withDummyLocation $
            D.ExpTyped desugaredE desugaredContext desugaredType

instance DesugarToExp InfixExp where
    desugarToExp (InfixExpLExp exp') = desugarToExp exp'
    desugarToExp (InfixExpNegated _ exp') =
        let desugaredExp = desugarToExp exp'
         in withDummyLocation $
            D.ExpApplication (makeExp nEGATE_NAME) (desugaredExp NE.:| [])
    desugarToExp (InfixExpApplication l op r) =
        let name = desugarToIdent op
            func = D.ExpVar name <$ name
            args = fmap desugarToExp (l NE.:| [r])
         in withDummyLocation $ D.ExpApplication func args

instance DesugarToExp LExp where
    desugarToExp (LExpApplication args) =
        case args of
            (f NE.:| []) -> desugarToExp f
            (f NE.:| (s:rest)) ->
                withDummyLocation $
                D.ExpApplication
                    (desugarToExp f)
                    (fmap desugarToExp (s NE.:| rest))
    desugarToExp (LExpAbstraction pats exp') =
        let desugaredPatterns = fmap desugarToPattern pats
            desugaredExp = desugarToExp exp'
         in withDummyLocation $ D.ExpAbstraction desugaredPatterns desugaredExp
    desugarToExp (LExpLet decls exp') =
        let desugaredDecls = concatMap desugarToAssignment decls
            desugaredExp = desugarToExp exp'
         in withDecls desugaredDecls desugaredExp
    desugarToExp (LExpIf cond true false) =
        let desugaredCond = desugarToExp cond
            desugaredTrue = desugarToExp true
            desugaredFalse = desugarToExp false
            trueAlt = withDummyLocation $ D.Alt truePattern desugaredTrue
            falseAlt = withDummyLocation $ D.Alt falsePattern desugaredFalse
         in withDummyLocation $
            D.ExpCase desugaredCond (trueAlt NE.:| [falseAlt])
    desugarToExp (LExpCase exp' alts) =
        let desugaredExp = desugarToExp exp'
            desugaredAlts = fmap desugarToAlt alts
         in withDummyLocation $ D.ExpCase desugaredExp desugaredAlts
    desugarToExp (LExpDo stmts) =
        let desugaredStmts = fmap desugarStmt stmts
            initialStmts = NE.init desugaredStmts
            lastStmt = NE.last desugaredStmts
         in case getValue lastStmt of
                D.StmtExp exp' -> withDummyLocation $ D.ExpDo initialStmts exp'
                _ -> undefined -- This case should be filtered earlier

instance DesugarToExp AExp where
    desugarToExp (AExpVariable name) =
        withDummyLocation $ D.ExpVar (desugarToIdent name)
    desugarToExp (AExpConstructor name) =
        withDummyLocation $ D.ExpConstr (desugarToIdent name)
    desugarToExp (AExpLiteral lit) =
        withDummyLocation $ D.ExpConst (desugarToConst lit)
    desugarToExp (AExpParens exp') = desugarToExp exp'
    desugarToExp (AExpTuple f s rest) =
        let args = f NE.:| (s : rest)
            desugaredArgs = fmap desugarToExp args
            function =
                withDummyLocation .
                D.ExpVar . withDummyLocation . D.IdentParametrised tUPLE_NAME $
                length rest + 2
         in withDummyLocation $ D.ExpApplication function desugaredArgs
    desugarToExp (AExpList (f NE.:| rest)) =
        let desugaredL = desugarToExp f
            desugaredR =
                case rest of
                    [] -> makeConstr lIST_NAME
                    (s:exps) -> desugarToExp (AExpList (s NE.:| exps))
            function = makeExp cOLON_NAME
         in withDummyLocation $
            D.ExpApplication function (desugaredL NE.:| [desugaredR])
    desugarToExp (AExpSequence f s e) =
        let desugaredF = desugarToExp f
            desugaredS = desugarToExp <$> s
            desugaredE = desugarToExp <$> e
         in withDummyLocation $
            case (desugaredS, desugaredE) of
                (Nothing, Nothing) ->
                    let function = makeExp eNUM_FROM_NAME
                     in D.ExpApplication function (desugaredF NE.:| [])
                (Just s', Nothing) ->
                    let function = makeExp eNUM_FROM_THEN_NAME
                     in D.ExpApplication function (desugaredF NE.:| [s'])
                (Nothing, Just e') ->
                    let function = makeExp eNUM_FROM_TO_NAME
                     in D.ExpApplication function (desugaredF NE.:| [e'])
                (Just s', Just e') ->
                    let function = makeExp eNUM_FROM_THEN_TO_NAME
                     in D.ExpApplication function (desugaredF NE.:| [s', e'])
    desugarToExp (AExpListCompr exp' quals) =
        let desugaredExp = desugarToExp exp'
            desugaredQuals = fmap desugarQual quals
         in withDummyLocation $ D.ExpListCompr desugaredExp desugaredQuals
    desugarToExp (AExpLeftSection exp' op) =
        let desugaredExp = desugarToExp exp'
            desugaredOp = desugarToIdent op
         in withDummyLocation $ D.ExpLeftSection desugaredExp desugaredOp
    desugarToExp (AExpRightSection op exp') =
        let desugaredOp = desugarToIdent op
            desugaredExp = desugarToExp exp'
         in withDummyLocation $ D.ExpRightSection desugaredOp desugaredExp
    desugarToExp (AExpRecordConstr name binds) =
        let desugaredName = desugarToIdent name
            desugaredBinds = map desugarToBinding binds
         in withDummyLocation $ D.ExpRecordConstr desugaredName desugaredBinds
    desugarToExp (AExpRecordUpdate exp' binds) =
        let desugaredExp = desugarToExp exp'
            desugaredBinds = fmap desugarToBinding binds
         in withDummyLocation $ D.ExpRecordUpdate desugaredExp desugaredBinds

-- Helper functions
-- | Desugar expression to Alt
desugarToAlt :: WithLocation Alt -> WithLocation D.Alt
desugarToAlt alt =
    case getValue alt of
        (AltSimple pat exp' decls) ->
            let desugaredPat = desugarToPattern pat
                desugaredExp = desugarToExp exp'
                desugaredDecls = concatMap desugarToAssignment decls
                expWithDecls = withDecls desugaredDecls desugaredExp
             in D.Alt desugaredPat expWithDecls <$ alt
        (AltGuarded pat gdpats decls) ->
            let desugaredPat = desugarToPattern pat
                desugaredDecls = concatMap desugarToAssignment decls
                rhs = desugarGdPats gdpats
                expWithDecls = withDecls desugaredDecls rhs
             in D.Alt desugaredPat expWithDecls <$ alt

-- | Desugar expression to bindings
desugarToBinding :: WithLocation FBind -> WithLocation D.Binding
desugarToBinding fbind
    | (FBind name exp') <- getValue fbind =
        D.Binding (desugarToIdent name) (desugarToExp exp') <$ fbind

-- | Desugar statements
desugarStmt :: WithLocation Stmt -> WithLocation D.Stmt
desugarStmt stmt =
    case getValue stmt of
        (StmtPat pat exp') ->
            let desugaredPat = desugarToPattern pat
                desugaredExp = desugarToExp exp'
             in D.StmtPattern desugaredPat desugaredExp <$ stmt
        (StmtLet decls) ->
            let desugaredDecls = concatMap desugarToAssignment decls
             in D.StmtLet desugaredDecls <$ stmt
        (StmtExp exp') ->
            let desugaredExp = desugarToExp exp'
             in D.StmtExp desugaredExp <$ stmt

-- | Desugar qualifiers
desugarQual :: WithLocation Qual -> WithLocation D.Stmt
desugarQual stmt =
    case getValue stmt of
        (QualGenerator pat exp') ->
            let desugaredPat = desugarToPattern pat
                desugaredExp = desugarToExp exp'
             in D.StmtPattern desugaredPat desugaredExp <$ stmt
        (QualLet decls) ->
            let desugaredDecls = concatMap desugarToAssignment decls
             in D.StmtLet desugaredDecls <$ stmt
        (QualGuard exp') ->
            let desugaredExp = desugarToExp exp'
             in D.StmtExp desugaredExp <$ stmt

-- | Desugar fixty and type declarations
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

-- | Desugar left hand side of a function
desugarFunLHS ::
       FunLHS -> (WithLocation D.Ident, NE.NonEmpty (WithLocation D.Pattern))
desugarFunLHS (FunLHSSimple var pats) = (desugarToIdent var, fmap desugarToPattern pats)
desugarFunLHS (FunLHSInfix l op r) =
    (desugarToIdent op, fmap desugarToPattern (l NE.:| [r]))
desugarFunLHS (FunLHSNested lhs pats) =
    let (ident, f NE.:| rest) = desugarFunLHS (getValue lhs)
     in (ident, f NE.:| (rest ++ NE.toList (fmap desugarToPattern pats)))

-- | Desugar guarded patterns
desugarGdPats :: NE.NonEmpty (WithLocation GdPat) -> WithLocation D.Exp
desugarGdPats (first NE.:| rest) =
    let desugaredRest =
            case rest of
                [] -> undefinedExp
                (s:others) -> desugarGdPats (s NE.:| others)
        (GdPat guards exp') = getValue first
        desugaredExp = desugarToExp exp'
     in desugarGuards guards desugaredExp desugaredRest

-- | Desugar guards
desugarGuards ::
       NE.NonEmpty (WithLocation Guard)
    -> WithLocation D.Exp
    -> WithLocation D.Exp
    -> WithLocation D.Exp
desugarGuards (first NE.:| rest) success failure =
    let desugaredRest =
            case rest of
                [] -> success
                (s:others) -> desugarGuards (s NE.:| others) success failure
     in case getValue first of
            (GuardPattern pat exp') ->
                let desugaredPat = desugarToPattern pat
                    desugaredExp = desugarToExp exp'
                    alt = D.Alt desugaredPat desugaredRest <$ first
                    altFailure =
                        withDummyLocation $ D.Alt wildcardPattern failure
                 in withDummyLocation $
                    D.ExpCase desugaredExp (alt NE.:| [altFailure])
            (GuardLet decls) ->
                let desugaredDecls = concatMap desugarToAssignment decls
                 in withDecls desugaredDecls desugaredRest
            (GuardExpr exp') ->
                let desugaredExp = desugarToExp exp'
                    altSuccess =
                        withDummyLocation $ D.Alt truePattern desugaredRest
                    altFailure = withDummyLocation $ D.Alt falsePattern failure
                 in withDummyLocation $
                    D.ExpCase desugaredExp (altSuccess NE.:| [altFailure])

-- | Wildcard pattern
wildcardPattern :: WithLocation D.Pattern
wildcardPattern = withDummyLocation D.PatternWildcard

-- | Make custom pattern constructor
makePattern :: EntityName -> WithLocation D.Pattern
makePattern =
    withDummyLocation .
    (`D.PatternConstr` []) . withDummyLocation . D.IdentNamed

-- | Pattern that matches True
truePattern :: WithLocation D.Pattern
truePattern = makePattern tRUE_NAME

-- | Pattern that matches False
falsePattern :: WithLocation D.Pattern
falsePattern = makePattern fALSE_NAME

-- | Make arbitrary expression
makeExp :: EntityName -> WithLocation D.Exp
makeExp = withDummyLocation . D.ExpVar . withDummyLocation . D.IdentNamed

-- | undefined
undefinedExp :: WithLocation D.Exp
undefinedExp = makeExp uNDEFINED_NAME

-- | Make constructors
makeConstr :: EntityName -> WithLocation D.Exp
makeConstr = withDummyLocation . D.ExpConstr . withDummyLocation . D.IdentNamed

-- | Make let block
withDecls ::
       [WithLocation D.Assignment] -> WithLocation D.Exp -> WithLocation D.Exp
withDecls [] = id
withDecls decls = withDummyLocation . D.ExpLet decls
