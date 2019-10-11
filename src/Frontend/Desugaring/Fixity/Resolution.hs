{- |
Module      :  Frontend.Desugaring.Fixity.Resolution
Description :  Resolution of fixity of operators
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Fixity resolution of operators in DFL. Follows algorithms, defined in
<https:ororwww.haskell.orgoronlinereportorhaskell2010orhaskellch10.html Haskell 2010>.
-}
module Frontend.Desugaring.Fixity.Resolution
    ( resolveFixity
    , resolveFlatInfix
    , InfixOperation(..)
    ) where

import qualified Data.List.NonEmpty as NE

import Core.PredefinedIdents
import Frontend.Desugaring.Fixity.Ast
import Frontend.Desugaring.Fixity.Base
import Frontend.Syntax.Position
    ( SourceLocation(..)
    , WithLocation(..)
    , dummyLocation
    , withDummyLocation
    )

-- | Class for types representing binary expressions
class InfixOperation a where
    makeBinary ::
           WithLocation a
        -> WithLocation Ident
        -> WithLocation a
        -> a -- ^ Make binary expression
    makeUnary :: WithLocation a -> a -- ^ Make unary expression

instance InfixOperation Pattern where
    makeBinary l op r = PatternConstr op [l, r]
    makeUnary = error "Unary minus in patterns is not supported"

instance InfixOperation Exp where
    makeBinary l op r =
        ExpApplication (withDummyLocation $ ExpVar op) (l NE.:| [r])
    makeUnary x =
        ExpApplication
            (withDummyLocation . ExpVar . withDummyLocation $
             IdentUserDefined mINUS)
            (x NE.:| [])

-- | Resolves fixity of a binary expression
resolveFixity :: (InfixOperation a) => [FlatInfix a] -> FixityResolver a
resolveFixity x = do
    (res, rest) <- resolveFlatInfix x
    case rest of
        [] -> return $ getValue res
        f:_ ->
            raiseError $
            FixityResolutionErrorCannotResolve (getSourceLocation f)

-- | Resolve fixity of an expression, represented as a list of flat infix expressions
resolveFlatInfix ::
       (InfixOperation a)
    => [FlatInfix a]
    -> FixityResolver (WithLocation a, [FlatInfix a])
resolveFlatInfix =
    let dummySignature =
            FixitySignature
                { getFixitySignatureName = undefined
                , getFixitySignatureFixity = Infix
                , getFixitySignaturePrecedence = -1
                }
     in startResolution dummySignature dummyLocation

-- | Start resolution and handle unary expressions
startResolution ::
       (InfixOperation a)
    => FixitySignature
    -> SourceLocation
    -> [FlatInfix a]
    -> FixityResolver (WithLocation a, [FlatInfix a])
startResolution infixOp@(FixitySignature _ _ prec) loc list
    | [] <- list = raiseError $ FixityResolutionErrorMissingOperand loc
    | FlatInfixOp m@(WithLocation op' loc'):rest <- list =
        if isMinus op'
            then if prec < 6
                     then do
                         (r, rest') <-
                             startResolution minusFixitySignature loc' rest
                         continueResolution
                             infixOp
                             (WithLocation (makeUnary r) (betweenTokens m r))
                             rest'
                     else raiseError $ FixityResolutionErrorFixityConflict m
            else raiseError $ FixityResolutionErrorUnexpectedOperator m
    | FlatInfixExp e:rest <- list = continueResolution infixOp e rest

-- | Continue resolution and handle binary expressions
continueResolution ::
       (InfixOperation a)
    => FixitySignature
    -> WithLocation a
    -> [FlatInfix a]
    -> FixityResolver (WithLocation a, [FlatInfix a])
continueResolution infixOp1@(FixitySignature _ fix1 prec1) e1 list
    | [] <- list = return (e1, [])
    | FlatInfixExp e2:_ <- list =
        raiseError $ FixityResolutionErrorMissingOperator (getLocation e2)
    | FlatInfixOp op2:rest <- list = do
        infixOp2 <- lookupOperator op2
        case infixOp2 of
            (FixitySignature _ fix2 prec2)
                | prec1 == prec2 && (fix1 /= fix2 || fix1 == Infix) ->
                    raiseError $ FixityResolutionErrorFixityConflict op2
                | prec1 > prec2 || (prec1 == prec2 && fix1 == InfixL) ->
                    return (e1, list)
                | otherwise -> do
                    (r, rest') <-
                        startResolution infixOp2 (getLocation op2) rest
                    continueResolution
                        infixOp1
                        (WithLocation (makeBinary e1 op2 r) (betweenTokens e1 r))
                        rest'

-- | Get location between two objects
betweenTokens :: WithLocation a -> WithLocation b -> SourceLocation
betweenTokens (WithLocation _ (SourceLocation st _)) (WithLocation _ (SourceLocation _ en)) =
    SourceLocation st en

-- | Get source location from a flat infix expression
getSourceLocation :: FlatInfix a -> SourceLocation
getSourceLocation (FlatInfixExp e) = getLocation e
getSourceLocation (FlatInfixOp op) = getLocation op
