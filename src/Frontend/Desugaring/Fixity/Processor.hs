{- |
Module      :  Frontend.Desugaring.Fixity.Expression
Description :  Resolution of fixity of expressions and patterns
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Fixity resolution of expressions in DFL
-}
module Frontend.Desugaring.Fixity.Processor
    ( resolveModuleFixity
    , FixityResolutionError(..)
    , InfixOperators
    ) where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Fixity.Ast
import Frontend.Desugaring.Fixity.Base
import Frontend.Desugaring.Fixity.Expression
import qualified Frontend.Desugaring.Grouping.Ast as G

-- | Resolves fixity of expressions in a module
resolveModuleFixity ::
       InfixOperators
    -> Module G.Exp
    -> Either FixityResolutionError (Module Exp)
resolveModuleFixity importedOperators module' =
    runFixityResolver (resolveModule module') importedOperators

resolveModule :: Module G.Exp -> FixityResolver (Module Exp)
resolveModule module'
    | Module { getModuleDataTypes = dataTypes
             , getModuleClasses = classes
             , getModuleExpressions = expressions
             } <- module' =
        let topLevelOperators =
                collectTopLevelOperators dataTypes classes expressions
         in defineOperators topLevelOperators $
            mapExpressionM resolveExp module'

collectTopLevelOperators ::
       DataTypes -> Classes G.Exp -> Expressions G.Exp -> InfixOperators
collectTopLevelOperators dataTypes classes expressions =
    let collectConstructorSignatures =
            HM.mapMaybe getConstructorFixity . HM.fromList
        dataTypeOperators =
            mconcat .
            map (collectConstructorSignatures . getDataTypeConstructors) $
            HM.elems dataTypes
        collectMethodSignatures = HM.mapMaybe getMethodFixity
        classOperators =
            mconcat . map (collectMethodSignatures . getClassMethods) $
            HM.elems classes
        expressionsOperators = collectSignatures expressions
     in dataTypeOperators <> classOperators <> expressionsOperators
