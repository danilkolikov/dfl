{- |
Module      :  Frontend.Desugaring.ToExp
Description :  Desugaring of AST nodes to Exp and Assignments
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Exp-s and Assignment-s.

Module contains functions for these 2 objects, because they're mutually recursive
-}
module Frontend.Desugaring.ToExp
    ( DesugarToAssignment(..)
    , DesugarToExp(..)
    ) where

import Control.Monad (liftM2)
import Data.Functor ((<$))
import qualified Data.List.NonEmpty as NE (NonEmpty(..), toList)

import qualified Frontend.Desugaring.Ast as D
import Frontend.Desugaring.AstWraps
import Frontend.Desugaring.IdentGenerator (IdentGenerator, newVarId)
import Frontend.Desugaring.ToConst (desugarToConst)
import Frontend.Desugaring.ToIdent (desugarToIdent)
import Frontend.Desugaring.ToPattern (desugarToPattern)
import Frontend.Desugaring.Util
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to Assignment-s
class DesugarToAssignment a where
    desugarToAssignment :: a -> IdentGenerator [WithLocation D.Assignment] -- ^ Desugar object to assignments

instance (DesugarToAssignment a) => DesugarToAssignment (WithLocation a) where
    desugarToAssignment x = do
        res <- desugarToAssignment (getValue x)
        return [getValue el <$ x | el <- res]

instance DesugarToAssignment Decl where
    desugarToAssignment (DeclGenDecl genDecl) = desugarToAssignment genDecl
    desugarToAssignment (DeclFunction lhs rhs) =
        case getValue lhs of
            Left fun -> do
                let (ident, pats) = desugarFunLHS fun
                    exp' = getValue <$> (rhsToExp <$> rhs)
                res <- desugarToExp $ LExpAbstraction pats exp'
                return [withDummyLocation $ D.AssignmentName ident res]
            Right pat ->
                return .
                withDummyLocation .
                D.AssignmentPattern (desugarToPattern (pat <$ lhs)) <$>
                desugarToExp rhs

instance DesugarToAssignment GenDecl where
    desugarToAssignment = return . desugarGenDecl D.AssignmentType

-- | Class for types which can be desugared to Exp
class DesugarToExp a where
    desugarToExp :: a -> IdentGenerator (WithLocation D.Exp) -- ^ Desugar object to exp

instance (DesugarToExp a) => DesugarToExp (WithLocation a) where
    desugarToExp = sequence . ((getValue <$>) . desugarToExp <$>)

instance DesugarToExp RHS where
    desugarToExp = desugarToExp . rhsToExp

instance DesugarToExp Exp where
    desugarToExp (ExpSimple e) = desugarToExp e
    desugarToExp (ExpTyped e context type') = do
        newIdent <- newVarId
        let newVar = FuncLabelId newIdent
            varExp = qVarToExp newQVar
            newQVar = FuncLabelId (Qualified [] newIdent)
            varPat = varToPat newVar
            typeSig =
                GenDeclTypeSig (withDummyLocation newVar NE.:| []) context type'
            eExp = wrapToExp' e
            letExp =
                LExpLet
                    [ withDummyLocation . DeclGenDecl . withDummyLocation $
                      typeSig
                    , withDummyLocation $
                      DeclFunction
                          (withDummyLocation . Right $ varPat)
                          (withSameLocation (`RHSSimple` []) eExp)
                    ]
                    (withDummyLocation varExp)
        desugarToExp letExp

instance DesugarToExp InfixExp where
    desugarToExp (InfixExpLExp e) = desugarToExp e
    desugarToExp (InfixExpNegated _ e) =
        desugarToExp $
        LExpApplication
            (entityNameToAExp nEGATE_NAME NE.:|
             [withDummyLocation . wrapToAExp $ ExpSimple e])
    desugarToExp (InfixExpApplication l op r) =
        desugarToExp $
        LExpApplication
            (qOpToAExp op NE.:|
             [ withDummyLocation . wrapToAExp $ ExpSimple l
             , withDummyLocation . wrapToAExp $ ExpSimple r
             ])

instance DesugarToExp LExp where
    desugarToExp (LExpApplication args) =
        case args of
            (f NE.:| []) -> desugarToExp f
            (f NE.:| (s:rest)) ->
                withDummyLocation <$>
                liftM2
                    D.ExpApplication
                    (desugarToExp f)
                    (mapM desugarToExp (s NE.:| rest))
    desugarToExp (LExpAbstraction pats exp') = do
        varIds <- mapM (const newVarId) pats
        let patVars = fmap (withDummyLocation . varToPat . FuncLabelId) varIds
            expVars =
                fmap
                    (withDummyLocation . qVarToExp . FuncLabelId . Qualified [])
                    varIds
            idents = fmap desugarToIdent varIds
            pat = combinePats . NE.toList $ patVars
            combinedExp = combineExps . NE.toList $ expVars
            alt = withSameLocation (\x -> AltSimple pat x []) exp'
            otherwise' =
                withDummyLocation $ AltSimple wildcardPattern undefinedExp []
            case' =
                withSameLocation
                    (\x -> LExpCase combinedExp (x NE.:| [otherwise']))
                    alt
        desugared <- desugarToExp case'
        return . withDummyLocation $ D.ExpAbstraction idents desugared
    desugarToExp (LExpLet decls exp') =
        withDummyLocation <$>
        liftM2
            D.ExpLet
            (concat <$> mapM desugarToAssignment decls)
            (desugarToExp exp')
    desugarToExp (LExpIf cond true false) =
        desugarToExp $
        let true' = entityNameToPat tRUE_NAME
            false' = entityNameToPat fALSE_NAME
         in LExpCase
                cond
                (withDummyLocation (AltSimple true' true []) NE.:|
                 [withDummyLocation $ AltSimple false' false []])
    desugarToExp (LExpCase exp' alts) =
        withDummyLocation <$>
        liftM2 D.ExpCase (desugarToExp exp') (mapM desugarToAlt alts)
    desugarToExp (LExpDo stmts)
        | stmt NE.:| [] <- stmts =
            case getValue stmt of
                StmtExp exp' -> desugarToExp (exp' <$ stmt)
                _ -> undefined -- Should be filtered earlier
        | stmt NE.:| (next:rest) <- stmts =
            let doRest = wrapToExp . wrapToInfixExp $ LExpDo (next NE.:| rest)
             in case getValue stmt of
                    StmtExp exp' ->
                        let aExp = wrapToAExp' exp'
                            restAExp = withDummyLocation . wrapToAExp $ doRest
                            function = entityNameToAExp iGNORING_BIND_NAME
                            application =
                                LExpApplication
                                    (function NE.:| [aExp, restAExp])
                         in desugarToExp application
                    StmtLet decls ->
                        desugarToExp $ LExpLet decls (withDummyLocation doRest)
                    StmtPat pat exp' -> do
                        varId <- newVarId
                        let var = withDummyLocation $ FuncLabelId varId
                            aPat = wrapToAPat' pat
                            lhs =
                                withDummyLocation $
                                FunLHSSimple var (aPat NE.:| [])
                            rhs =
                                withDummyLocation $
                                RHSSimple (withDummyLocation doRest) []
                            decl =
                                withDummyLocation $
                                DeclFunction (Left <$> lhs) rhs
                            function = entityNameToAExp bIND_NAME
                            aExp = wrapToAExp' exp'
                            varAExp = varIdToAExp varId
                            application =
                                withDummyLocation . wrapToExp . wrapToInfixExp $
                                LExpApplication (function NE.:| [aExp, varAExp])
                            let' = LExpLet [decl] application
                        desugarToExp let'

instance DesugarToExp AExp where
    desugarToExp (AExpVariable name) =
        return . withDummyLocation $ D.ExpVar (desugarToIdent name)
    desugarToExp (AExpConstructor name) =
        return . withDummyLocation $ D.ExpVar (desugarToIdent name)
    desugarToExp (AExpLiteral lit) =
        return . withDummyLocation $ D.ExpConst (desugarToConst lit)
    desugarToExp (AExpParens exp') = desugarToExp exp'
    desugarToExp (AExpTuple f s rest) = do
        desugaredF <- desugarToExp f
        desugaredS <- desugarToExp s
        desugaredRest <- mapM desugarToExp rest
        let function =
                withDummyLocation .
                D.ExpVar . withDummyLocation . D.IdentParametrised tUPLE_NAME $
                length rest + 2
        return . withDummyLocation $
            D.ExpApplication
                function
                (desugaredF NE.:| (desugaredS : desugaredRest))
    desugarToExp (AExpList (f NE.:| rest)) = do
        desugaredL <- desugarToExp f
        desugaredR <-
            case rest of
                [] ->
                    return . withDummyLocation . D.ExpVar . desugarToIdent $
                    GConList
                (s:exps) -> desugarToExp (AExpList (s NE.:| exps))
        let function = entityNameToExp cOLON_NAME
        return . withDummyLocation $
            D.ExpApplication function (desugaredL NE.:| [desugaredR])
    desugarToExp (AExpSequence f s e) = do
        desugaredF <- desugarToExp f
        case (s, e) of
            (Nothing, Nothing) -> do
                let function = entityNameToExp eNUM_FROM_NAME
                return . withDummyLocation $
                    D.ExpApplication function (desugaredF NE.:| [])
            (Just s', Nothing) -> do
                desugaredS <- desugarToExp s'
                let function = entityNameToExp eNUM_FROM_THEN_NAME
                return . withDummyLocation $
                    D.ExpApplication function (desugaredF NE.:| [desugaredS])
            (Nothing, Just e') -> do
                desugaredE <- desugarToExp e'
                let function = entityNameToExp eNUM_FROM_TO_NAME
                return . withDummyLocation $
                    D.ExpApplication function (desugaredF NE.:| [desugaredE])
            (Just s', Just e') -> do
                desugaredS <- desugarToExp s'
                desugaredE <- desugarToExp e'
                let function = entityNameToExp eNUM_FROM_THEN_TO_NAME
                return . withDummyLocation $
                    D.ExpApplication
                        function
                        (desugaredF NE.:| [desugaredS, desugaredE])
    desugarToExp (AExpListCompr exp' (qual NE.:| rest)) =
        let restExp =
                withDummyLocation . wrapToExp . wrapToInfixExp . wrapToLExp $
                case rest of
                    [] -> AExpList (exp' NE.:| [])
                    (s:quals) -> AExpListCompr exp' (s NE.:| quals)
         in case getValue qual of
                (QualLet decls) -> desugarToExp $ LExpLet decls restExp
                (QualGuard cond) ->
                    desugarToExp $
                    LExpIf
                        cond
                        restExp
                        (withDummyLocation . gConToExp $ GConUnit)
                (QualGenerator pat e) -> do
                    varId <- newVarId
                    let var = withDummyLocation $ FuncLabelId varId
                        aPat = wrapToAPat' pat
                        lhs =
                            withDummyLocation $ FunLHSSimple var (aPat NE.:| [])
                        rhs = withDummyLocation $ RHSSimple restExp []
                        decl =
                            withDummyLocation $ DeclFunction (Left <$> lhs) rhs
                        function = entityNameToAExp cONCAT_MAP_NAME
                        aExp = wrapToAExp' e
                        varAExp = varIdToAExp varId
                        application =
                            withDummyLocation . wrapToExp . wrapToInfixExp $
                            LExpApplication (function NE.:| [aExp, varAExp])
                        let' = LExpLet [decl] application
                    desugarToExp let'
    desugarToExp (AExpLeftSection l op) = do
        varId <- newVarId
        let var = FuncLabelId varId
            qVar = FuncLabelId (Qualified [] varId)
            right = withDummyLocation $ AExpVariable (withDummyLocation qVar)
            pat =
                withDummyLocation $ APatVariable (withDummyLocation var) Nothing
            func = qOpToAExp op
            left = wrapToAExp' . wrapToExp' $ l
            application =
                withDummyLocation . wrapToExp . wrapToInfixExp $
                LExpApplication (func NE.:| [left, right])
            abstr = LExpAbstraction (pat NE.:| []) application
        desugarToExp abstr
    desugarToExp (AExpRightSection op r) = do
        varId <- newVarId
        let var = FuncLabelId varId
            qVar = FuncLabelId (Qualified [] varId)
            left = withDummyLocation $ AExpVariable (withDummyLocation qVar)
            pat =
                withDummyLocation $ APatVariable (withDummyLocation var) Nothing
            func = qOpToAExp op
            right = wrapToAExp' . wrapToExp' $ r
            application =
                withDummyLocation . wrapToExp . wrapToInfixExp $
                LExpApplication (func NE.:| [left, right])
            abstr = LExpAbstraction (pat NE.:| []) application
        desugarToExp abstr
    desugarToExp (AExpRecordConstr name binds) =
        withDummyLocation . D.ExpRecordConstr (desugarToIdent name) <$>
        mapM desugarToBinding binds
    desugarToExp (AExpRecordUpdate exp' binds) =
        withDummyLocation <$>
        liftM2
            D.ExpRecordUpdate
            (desugarToExp exp')
            (mapM desugarToBinding binds)

-- | Class for types which can be desugared to Alt
class DesugarToAlt a where
    desugarToAlt :: a -> IdentGenerator (WithLocation D.Alt) -- ^ Desugar object to alt

instance (DesugarToAlt a) => DesugarToAlt (WithLocation a) where
    desugarToAlt = sequence . ((getValue <$>) . desugarToAlt <$>)

instance DesugarToAlt Alt where
    desugarToAlt (AltSimple pat exp' decls) = do
        expr <- desugarToExp $ declsToExp decls exp'
        return . withDummyLocation $ D.Alt (desugarToPattern pat) expr
    desugarToAlt (AltGuarded pat gdpats decls) = do
        expr <- desugarToExp $ declsToExp decls (desugarGdPats gdpats)
        return . withDummyLocation $ D.Alt (desugarToPattern pat) expr

-- | Class for types which can be desugared to Binding
class DesugarToBinding a where
    desugarToBinding :: a -> IdentGenerator (WithLocation D.Binding) -- ^ Desugar object to Binding

instance (DesugarToBinding a) => DesugarToBinding (WithLocation a) where
    desugarToBinding = sequence . ((getValue <$>) . desugarToBinding <$>)

instance DesugarToBinding FBind where
    desugarToBinding (FBind name exp') =
        withDummyLocation . D.Binding (desugarToIdent name) <$>
        desugarToExp exp'

-- Helper functions
desugarGdPats :: NE.NonEmpty (WithLocation GdPat) -> WithLocation Exp
desugarGdPats (WithLocation (GdPat guards exp') _ NE.:| rest)
    | [] <- rest = desugarGuards guards exp' undefinedExp
    | (next:gds) <- rest =
        desugarGuards guards exp' (desugarGdPats (next NE.:| gds))

desugarGuards ::
       NE.NonEmpty (WithLocation Guard)
    -> WithLocation Exp
    -> WithLocation Exp
    -> WithLocation Exp
desugarGuards (guard NE.:| rest) res otherwise' =
    let exp' =
            case rest of
                [] -> res
                (next:guards) ->
                    desugarGuards (next NE.:| guards) res otherwise'
     in case getValue guard of
            GuardLet decls ->
                (wrapToExp . wrapToInfixExp $ LExpLet decls exp') <$ guard
            GuardExpr cond ->
                (wrapToExp . wrapToInfixExp $
                 LExpIf (wrapToExp' cond) exp' otherwise') <$
                guard
            GuardPattern pat expr ->
                (wrapToExp . wrapToInfixExp $
                 LExpCase
                     (wrapToExp' expr)
                     (withDummyLocation (AltSimple pat exp' []) NE.:|
                      [ withDummyLocation
                            (AltSimple wildcardPattern otherwise' [])
                      ])) <$
                guard
