{- |
Module      :  Frontend.Desugaring.Checking.Export
Description :  Disambiguation of exports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Type for objects which check exports for ambiguity.
-}
module Frontend.Desugaring.Checking.Export where

import Control.Monad (liftM2)
import Data.Functor (($>))

import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Grouping.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Checks idents in an export for ambiguity
checkExport :: WithLocation Export -> CheckingProcessor (WithLocation Export)
checkExport export =
    (export $>) <$>
    case getValue export of
        ExportFunction name -> ExportFunction <$> checkExpressionName name
        ExportDataOrClass name list ->
            liftM2
                ExportDataOrClass
                (checkTypeName name)
                (traverse checkExpressionName list)
        ExportModule name -> return $ ExportModule name
