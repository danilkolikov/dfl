{- |
Module      :  Compiler.Prettify.FixityResolutionOutput
Description :  Prettifying of FixityResolutionOutput
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of FixityResolutionOutput
-}
module Compiler.Prettify.FixityResolutionOutput where

import qualified Data.HashMap.Lazy as HM

import Compiler.Prettify.Ast ()
import Compiler.Prettify.PrettyPrintable
import Compiler.Prettify.Utils
import Frontend.Syntax.EntityName
import Frontend.Syntax.Processor
    ( FixityResolutionOutput(..)
    , InfixOperator(..)
    , InfixOperators
    )

prettifyFixityResolutionOutput :: FixityResolutionOutput -> String
prettifyFixityResolutionOutput FixityResolutionOutput { getFixityResolutionOutputAst = ast
                                                      , getFixityResolutionOutputOperators = operators
                                                      } =
    let astHeader = prettifyHeader "AST:"
        prettyAst = prettifyAst ast
        operatorsHeader = prettifyHeader "Operators:"
        prettyOperators = prettifyOperators operators
     in unlines [astHeader, prettyAst, operatorsHeader, prettyOperators]

prettifyOperators :: InfixOperators -> String
prettifyOperators = unlines . map (uncurry prettifyOperator) . HM.toList

prettifyOperator :: EntityName -> InfixOperator -> String
prettifyOperator name infixOp =
    unwords [prettifyEntityName name, "-", prettifyInfixOperator infixOp]

prettifyInfixOperator :: InfixOperator -> String
prettifyInfixOperator (InfixOperator _ fixity prec) =
    "fixity: " ++ show fixity ++ ", precedence: " ++ show prec
