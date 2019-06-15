{- |
Module      :  Frontend.Desugaring.Final.PatternDesugaring
Description :  Final desugaring of patterns
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of patterns
-}
module Frontend.Desugaring.Final.PatternDesugaring
    ( PreparedAlt(..),
      desugarPatternsToAbstraction
    , desugarCasesToAbstraction
    , desugarAltsToAbstraction
    , desugarPattern
    , desugarAlts
    ) where

import Control.Monad (replicateM)
import Data.Functor ((<$))
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Processor
import Frontend.Desugaring.Final.Util
import qualified Frontend.Desugaring.Final.ResolvedAst as R
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Structure for desugaring of patterns
data PreparedAlt =
    PreparedAlt (WithLocation R.Pattern)
                (WithLocation Exp)

-- | Desugar a non-empty list of patterns to an abstraction
desugarPatternsToAbstraction ::
       Int
    -> NE.NonEmpty (NE.NonEmpty (WithLocation R.Pattern), WithLocation Exp)
    -> DesugaringProcessor (WithLocation Exp)
desugarPatternsToAbstraction nPatterns patterns = do
    newIdents <- replicateM nPatterns generateNewIdent'
    let tupleIdent = makeTuple nPatterns
        combinePattern (pat NE.:| rest) =
            case rest of
                [] -> pat
                _ -> withDummyLocation $ R.PatternConstr tupleIdent (pat : rest)
        makeAlt (pats, exp') =
            let combinedPattern = combinePattern pats
             in PreparedAlt combinedPattern exp'
    abstraction <- desugarAltsToAbstraction (fmap makeAlt patterns)
    let varExps@(firstVar NE.:| restVars) =
            fmap (withDummyLocation . ExpVar) (NE.fromList newIdents)
        combinedExpr =
            case restVars of
                [] -> firstVar
                _ ->
                    let tupleExp = withDummyLocation $ ExpVar tupleIdent
                     in withDummyLocation $ ExpApplication tupleExp varExps
        application =
            withDummyLocation $
            ExpApplication abstraction (combinedExpr NE.:| [])
        makeAbstraction ident = withDummyLocation . ExpAbstraction ident
    return $ foldr makeAbstraction application newIdents

-- | Desugar list of prepared alternatives to an abstraction
desugarAltsToAbstraction ::
       NE.NonEmpty PreparedAlt -> DesugaringProcessor (WithLocation Exp)
desugarAltsToAbstraction =
    let desugarAlt ident elseIdent (PreparedAlt pat exp') =
            desugarPattern ident pat exp' elseIdent
     in desugarCasesToAbstraction desugarAlt

-- | Desugar list of objects to an abstraction
desugarCasesToAbstraction ::
       (WithLocation Ident -> WithLocation Ident -> a -> DesugaringProcessor (WithLocation Exp))
    -> NE.NonEmpty a
    -> DesugaringProcessor (WithLocation Exp)
desugarCasesToAbstraction desugarCase cases = do
    newIdent <- generateNewIdent'
    case' <- desugarAlts newIdent desugarCase cases
    return . withDummyLocation $ ExpAbstraction newIdent case'

-- | Desugar list of alternatives
desugarAlts ::
       WithLocation Ident
    -> (WithLocation Ident -> WithLocation Ident -> a -> DesugaringProcessor (WithLocation Exp))
    -> NE.NonEmpty a
    -> DesugaringProcessor (WithLocation Exp)
desugarAlts ident desugarA (first NE.:| alts) = do
    desugaredAlts <-
        case alts of
            [] -> return undefinedExp
            (s:rest) -> desugarAlts ident desugarA (s NE.:| rest)
    elseIdent <- generateNewIdent'
    result <- desugarA ident elseIdent first
    let resultAbs = withDummyLocation $ ExpAbstraction elseIdent result
    return . withDummyLocation $
        ExpApplication resultAbs (desugaredAlts NE.:| [])

-- | Desugar single pattern
desugarPattern ::
       WithLocation Ident
    -> WithLocation R.Pattern
    -> WithLocation Exp
    -> WithLocation Ident
    -> DesugaringProcessor (WithLocation Exp)
desugarPattern ident patter success failure =
    case getValue patter of
        (R.PatternVar name pat) ->
            let abstraction = withDummyLocation $ ExpAbstraction name success
             in case pat of
                    Nothing -> return abstraction
                    Just asPattern ->
                        let newSuccess =
                                withDummyLocation $
                                ExpApplication
                                    abstraction
                                    (withDummyLocation (ExpVar ident) NE.:| [])
                         in desugarPattern ident asPattern newSuccess failure
        R.PatternWildcard -> return success
        (R.PatternConst c) -> do
            newIdent <- generateNewIdent'
            let application =
                    withDummyLocation $
                    ExpApplication
                        (makeExp eQUAL_NAME)
                        (withDummyLocation (ExpVar ident) NE.:|
                         [withDummyLocation (ExpConst c)])
                case' =
                    withDummyLocation $
                    ExpCase newIdent truePattern success failure
            return . withDummyLocation $
                ExpApplication case' (application NE.:| [])
        (R.PatternConstr name args) -> do
            newIdents <- mapM (const generateNewIdent') args
            let makeCases [] = return success
                makeCases ((ident', pattern'):rest) = do
                    res <- makeCases rest
                    desugarPattern ident' pattern' res failure
            cases <- makeCases (zip newIdents args)
            let pattern' = PatternConstr name newIdents <$ patter
                case' = ExpCase ident pattern' cases failure
            return . withDummyLocation $ case'
