{- |
Module      :  Frontend.Inference.Type.WithDependencies
Description :  Functions for dependency resolutions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for resolution of dependencies of expressions
-}
module Frontend.Inference.Type.WithDependencies
    ( getExpressionsDependencyGraph
    ) where

import Control.Monad.Trans.Reader (Reader, ask, local, runReader)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import Util.DependencyResolver (Dependencies, DependencyGraph)
import Frontend.Inference.Let.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Get graph of dependencies between expressions
getExpressionsDependencyGraph ::
       HS.HashSet Ident -> Expressions -> DependencyGraph Ident
getExpressionsDependencyGraph definedIdents exprs =
    let getSingleExpressionDependencies expr =
            runReader (getExpressionDependencies expr) definedIdents
     in HM.map getSingleExpressionDependencies exprs

-- | Type of a dependency getter
type DependencyGetter = Reader (HS.HashSet Ident) (Dependencies Ident)

-- | Get dependencies of an expression
getExpressionDependencies :: Expression -> DependencyGetter
getExpressionDependencies Expression {getExpressionBody = body} =
    getExpDependencies body

-- | Get dependencies of an Exp
getExpDependencies :: WithLocation Exp -> DependencyGetter
getExpDependencies expr =
    case getValue expr of
        ExpVar name -> getIdentDependencies name
        ExpConstr _ -> return HS.empty
        ExpConst _ -> return HS.empty
        ExpApplication func args ->
            HS.unions <$> mapM getExpDependencies (func : NE.toList args)
        ExpAbstraction name innerExp ->
            local (HS.insert (getValue name)) (getExpDependencies innerExp)
        ExpCase name _ args success failure -> do
            nameDeps <- getIdentDependencies name
            successDeps <-
                local
                    (HS.union (HS.fromList $ map getValue args))
                    (getExpDependencies success)
            failureDeps <- getIdentDependencies failure
            return $ HS.unions [nameDeps, successDeps, failureDeps]

-- | Get dependencies of a single ident
getIdentDependencies :: WithLocation Ident -> DependencyGetter
getIdentDependencies name = do
    defined <- ask
    return $
        if HS.member (getValue name) defined
            then HS.empty
            else HS.singleton (getValue name)
