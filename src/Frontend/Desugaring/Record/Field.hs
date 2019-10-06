{- |
Module      :  Frontend.Desugaring.Record.Field
Description :  Functions for desugaring field getters
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for desugaring field getters
-}
module Frontend.Desugaring.Record.Field where

import Control.Monad.Trans.Class (lift)
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)

import Frontend.Desugaring.Record.Ast
import Frontend.Desugaring.Record.Base
import Frontend.Desugaring.Record.Expression
import Frontend.Desugaring.Record.Util
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Makes getterns for fields in data types
makeFieldGetters :: DataTypes -> RecordDesugaringProcessor (Expressions Exp)
makeFieldGetters dataTypes =
    lift (HM.unions <$> mapM makeDataTypeFieldGetters (HM.elems dataTypes))

-- | Make field getters for a data type
makeDataTypeFieldGetters :: DataType -> RecordDesugaringExcept (Expressions Exp)
makeDataTypeFieldGetters dataType =
    let constructors = map snd $ getDataTypeConstructors dataType
        fields =
            HS.toList . HS.unions . map (HM.keysSet . getConstructorFields) $
            constructors
     in HM.fromList <$> mapM (makeFieldGetter constructors) fields

-- | Make a single field getter for a data type
makeFieldGetter ::
       [Constructor] -> Ident -> RecordDesugaringExcept (Ident, Expression Exp)
makeFieldGetter constructors field = do
    let requiredConstructors = filterRequiredConstructors [field] constructors
    alternatives <-
        mapM
            (makeGetterAlternative (withDummyLocation field))
            requiredConstructors
    let newIdent = makeIdent (-1) -- Hacky way to get a new identifier
        newExp = withDummyLocation . ExpVar $ newIdent
        newPattern = withDummyLocation $ PatternVar newIdent Nothing
        caseExp = withDummyLocation $ ExpCase newExp (NE.fromList alternatives)
        abstraction =
            withDummyLocation $ ExpAbstraction (newPattern NE.:| []) caseExp
        expression =
            Expression
                { getExpressionName = withDummyLocation field
                , getExpressionBody = abstraction
                , getExpressionType = Nothing
                , getExpressionFixity = Nothing
                }
    return (field, expression)

-- | Create one alternative in the field getter
makeGetterAlternative ::
       WithLocation Ident
    -> Constructor
    -> RecordDesugaringExcept (WithLocation Alt)
makeGetterAlternative field constructor = do
    let makeMissing _ = (withDummyLocation PatternWildcard, Nothing)
        preparedField =
            ( field
            , \p ->
                  ( makePattern p
                  , Just . withDummyLocation . ExpVar $ makeIdent p))
    resolvedBindings <- resolveBindings constructor makeMissing [preparedField]
    let (idents, exps) = unzip resolvedBindings
        constr = getConstructorName constructor
        [expr] = catMaybes exps
        pattern' = withDummyLocation $ PatternConstr constr idents
    return . withDummyLocation $ AltSimple pattern' expr
