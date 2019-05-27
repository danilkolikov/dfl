{- |
Module      :  Frontend.Desugaring.Initial.AstWraps
Description :  Helper functions for wrapping of AST nodes
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Helper functions for wrapping of AST nodes.
-}
module Frontend.Desugaring.Initial.AstWraps where

import Data.Functor ((<$))
import Data.List.NonEmpty (NonEmpty(..))

import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

wrapToExp :: InfixExp -> Exp
wrapToExp = ExpSimple . withDummyLocation

wrapToExp' :: WithLocation InfixExp -> WithLocation Exp
wrapToExp' x = ExpSimple x <$ x

wrapToInfixExp :: LExp -> InfixExp
wrapToInfixExp = InfixExpLExp . withDummyLocation

wrapToInfixExp' :: WithLocation LExp -> WithLocation InfixExp
wrapToInfixExp' x = InfixExpLExp x <$ x

wrapToLExp :: AExp -> LExp
wrapToLExp = LExpApplication . (:| []) . withDummyLocation

wrapToLExp' :: WithLocation AExp -> WithLocation LExp
wrapToLExp' x = LExpApplication (x :| []) <$ x

wrapToAExp :: Exp -> AExp
wrapToAExp = AExpParens . withDummyLocation

wrapToAExp' :: WithLocation Exp -> WithLocation AExp
wrapToAExp' x = AExpParens x <$ x

wrapToPat :: LPat -> Pat
wrapToPat = PatSimple . withDummyLocation

wrapToPat' :: WithLocation LPat -> WithLocation Pat
wrapToPat' x = PatSimple x <$ x

wrapToLPat :: APat -> LPat
wrapToLPat = LPatSimple . withDummyLocation

wrapToLPat' :: WithLocation APat -> WithLocation LPat
wrapToLPat' x = LPatSimple x <$ x

wrapToAPat :: Pat -> APat
wrapToAPat = APatParens . withDummyLocation

wrapToAPat' :: WithLocation Pat -> WithLocation APat
wrapToAPat' x = APatParens x <$ x

withSameLocation :: (WithLocation a -> b) -> WithLocation a -> WithLocation b
withSameLocation f x = f x <$ x

gdRHSToGdPat :: GdRHS -> GdPat
gdRHSToGdPat (GdRHS guards exp') = GdPat guards exp'

qOpToQVar :: QOp -> Either QVar QCon
qOpToQVar (Left (OpLabelId varId)) = Left $ FuncLabelId varId
qOpToQVar (Left (OpLabelSym varSym)) = Left $ FuncLabelSym varSym
qOpToQVar (Right (OpLabelSym gCon)) = Right $ FuncLabelSym gCon
qOpToQVar (Right (OpLabelId conId)) = Right $ FuncLabelId conId

qOpToAExp :: WithLocation QOp -> WithLocation AExp
qOpToAExp qOp =
    let qVar = qOpToQVar <$> qOp
        aExp =
            case getValue qVar of
                Left var -> AExpVariable (var <$ qVar)
                Right con -> AExpConstructor (GConNamed (con <$ qVar) <$ qVar)
     in aExp <$ qOp

qVarToExp :: QVar -> Exp
qVarToExp =
    wrapToExp . wrapToInfixExp . wrapToLExp . AExpVariable . withDummyLocation

varToPat :: Var -> Pat
varToPat = wrapToPat . wrapToLPat . (`APatVariable` Nothing) . withDummyLocation

gConToPat :: GCon -> Pat
gConToPat = wrapToPat . wrapToLPat . APatConstructor . withDummyLocation

gConToExp :: GCon -> Exp
gConToExp =
    wrapToExp .
    wrapToInfixExp . wrapToLExp . AExpConstructor . withDummyLocation

declsToExp :: [WithLocation Decl] -> WithLocation Exp -> WithLocation Exp
declsToExp [] exp' = exp'
declsToExp decls exp' =
    withDummyLocation . wrapToExp . wrapToInfixExp $ LExpLet decls exp'
