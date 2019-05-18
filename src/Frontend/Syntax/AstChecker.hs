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
    , AstCheckerError
    , AstCheckable(..)
    ) where

import Data.List (find)
import qualified Data.List.NonEmpty as NE

import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Errors which can found by checking AST
data AstCheckerError =
    AstCheckerErrorSameTyVar (WithLocation TyVar)
                             (WithLocation TyVar) -- ^ Some type variables in instance declaration are same
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
    checkAst (TopDeclInstance _ _ inst _) = checkAst inst
    checkAst _ = correct

instance AstCheckable Inst where
    checkAst (InstNamed _ ts) = checkDifferentTyVars ts
    checkAst (InstTuple f s rest) = checkDifferentTyVars (f : s : rest)
    checkAst (InstFunction from to) = checkDifferentTyVars [from, to]
    checkAst _ = correct

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
