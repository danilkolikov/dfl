{- |
Module      :  Frontend.Syntax.AstChecker
Description :  Checker of Abstract Syntax Tree
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Checker of Abstract Syntax Tree of DFL. Ensures that parsed tree is correct,
according to the specification of
<https:ororwww.haskell.orgoronlinereportorhaskell2010orhaskellch10.html Haskell 2010>.
-}
module Frontend.Syntax.AstChecker
    ( AstChecker
    , AstCheckerError(..)
    , AstCheckable(..)
    ) where

import Data.List (find)
import qualified Data.List.NonEmpty as NE

import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Errors which can found by checking AST
data AstCheckerError
    = AstCheckerErrorSameTyVar (WithLocation TyVar)
                               (WithLocation TyVar) -- ^ Some type variables in instance declaration are same
    | AstCheckerErrorLastStatement (WithLocation Stmt) -- ^ Last statement in Do is let or assignment
    deriving (Show, Eq)

-- | Type for checker of AST
type AstChecker = Either AstCheckerError ()

-- | Type for classes which can be checked
class AstCheckable a where
    checkAst :: a -> AstChecker -- ^ Function for checking of AST

instance (AstCheckable a) => AstCheckable (Maybe a) where
    checkAst = maybe correct checkAst

instance (AstCheckable a, AstCheckable b) => AstCheckable (Either a b) where
    checkAst = either checkAst checkAst

instance (AstCheckable a) => AstCheckable [a] where
    checkAst = mapM_ checkAst

instance (AstCheckable a) => AstCheckable (NE.NonEmpty a) where
    checkAst = mapM_ checkAst

instance (AstCheckable a) => AstCheckable (WithLocation a) where
    checkAst (WithLocation x _) = checkAst x

instance AstCheckable Module where
    checkAst (ModuleExplicit _ _ body) = checkAst body
    checkAst (ModuleImplicit body) = checkAst body

instance AstCheckable Body where
    checkAst (Body _ topDecl) = checkAst topDecl

instance AstCheckable TopDecl where
    checkAst (TopDeclInstance _ _ inst decls) = checkAst inst >> checkAst decls
    checkAst (TopDeclClass _ _ _ decls) = checkAst decls
    checkAst (TopDeclDecl decl) = checkAst decl
    checkAst _ = correct

instance AstCheckable Inst where
    checkAst (InstNamed _ ts) = checkDifferentTyVars ts
    checkAst (InstTuple f s rest) = checkDifferentTyVars (f : s : rest)
    checkAst (InstFunction from to) = checkDifferentTyVars [from, to]
    checkAst _ = correct

instance AstCheckable Decl where
    checkAst DeclGenDecl {} = correct
    checkAst (DeclFunction _ rhs) = checkAst rhs

instance AstCheckable CDecl where
    checkAst CDeclGenDecl {} = correct
    checkAst (CDeclFunction _ rhs) = checkAst rhs

instance AstCheckable IDecl where
    checkAst (IDeclFunction _ rhs) = checkAst rhs

instance AstCheckable RHS where
    checkAst (RHSSimple exp' decls) = checkAst exp' >> checkAst decls
    checkAst (RHSGuarded gdrhs decls) = checkAst gdrhs >> checkAst decls

instance AstCheckable GdRHS where
    checkAst (GdRHS guards exp') = checkAst guards >> checkAst exp'

instance AstCheckable Guard where
    checkAst (GuardPattern _ exp') = checkAst exp'
    checkAst (GuardLet decls) = checkAst decls
    checkAst (GuardExpr e) = checkAst e

instance AstCheckable Exp where
    checkAst (ExpSimple e) = checkAst e
    checkAst (ExpTyped e _ _) = checkAst e

instance AstCheckable InfixExp where
    checkAst (InfixExpApplication l _ r) = checkAst l >> checkAst r
    checkAst (InfixExpNegated _ exp') = checkAst exp'
    checkAst (InfixExpLExp exp') = checkAst exp'

instance AstCheckable LExp where
    checkAst (LExpAbstraction _ exp') = checkAst exp'
    checkAst (LExpLet decls exp') = checkAst decls >> checkAst exp'
    checkAst (LExpIf cond true false) = mapM_ checkAst [cond, true, false]
    checkAst (LExpCase exp' alts) = checkAst exp' >> checkAst alts
    checkAst (LExpDo stmts) = checkLastStatement >> checkAst stmts
      where
        checkLastStatement :: AstChecker
        checkLastStatement =
            let lastStatement = NE.last stmts
             in case getValue lastStatement of
                    StmtExp _ -> correct
                    _ -> Left $ AstCheckerErrorLastStatement lastStatement
    checkAst (LExpApplication exps) = checkAst exps

instance AstCheckable AExp where
    checkAst (AExpParens exp') = checkAst exp'
    checkAst (AExpTuple f s rest) = mapM_ checkAst (f : s : rest)
    checkAst (AExpList exps) = checkAst exps
    checkAst (AExpSequence s f e) = checkAst s >> checkAst f >> checkAst e
    checkAst (AExpListCompr exp' quals) = checkAst exp' >> checkAst quals
    checkAst (AExpLeftSection exp' _) = checkAst exp'
    checkAst (AExpRightSection _ exp') = checkAst exp'
    checkAst (AExpRecordUpdate exp' binds) = checkAst exp' >> checkAst binds
    checkAst (AExpRecordConstr _ binds) = checkAst binds
    checkAst _ = correct

instance AstCheckable Alt where
    checkAst (AltSimple _ exp' decls) = checkAst exp' >> checkAst decls
    checkAst (AltGuarded _ gdpat decls) = checkAst gdpat >> checkAst decls

instance AstCheckable Qual where
    checkAst (QualGenerator _ exp') = checkAst exp'
    checkAst (QualLet decls) = checkAst decls
    checkAst (QualGuard exp') = checkAst exp'

instance AstCheckable Stmt where
    checkAst (StmtPat _ exp') = checkAst exp'
    checkAst (StmtLet decls) = checkAst decls
    checkAst (StmtExp exp') = checkAst exp'

instance AstCheckable GdPat where
    checkAst (GdPat guards exp') = checkAst guards >> checkAst exp'

instance AstCheckable FBind where
    checkAst (FBind _ exp') = checkAst exp'

-- Helper functions
correct :: AstChecker
correct = return ()

-- | Check that all type variables in the provided list are different
checkDifferentTyVars :: [WithLocation TyVar] -> AstChecker
checkDifferentTyVars [] = correct
checkDifferentTyVars [_] = correct
checkDifferentTyVars (t:rest) =
    case find ((== getValue t) . getValue) rest of
        Nothing -> checkDifferentTyVars rest
        Just s -> Left (AstCheckerErrorSameTyVar t s)
