{- |
Module      :  Frontend.Desugaring.Initial.ToConstrTest
Description :  Tests for desugaring of object to Constr-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Constr-s
-}
module Frontend.Desugaring.Initial.ToConstrTest
    ( testSuite
    , getConstrExample
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToConstr
    ( desugarToConstr
    , desugarToFieldDecl
    )
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Desugaring.Initial.ToTypeTest (getTypeExample)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getConstrExample :: RandomSelector (WithLocation Constr, WithLocation D.Constr)
getConstrExample =
    selectFromRandom
        [ do (nameEx, nameRes) <- getIdentExample
             (argsEx, argsRes) <- randomList 3 getTypeExample
             withSameLocation $
                 return
                     ( ConstrSimple nameEx argsEx
                     , D.ConstrSimple nameRes argsRes)
        , do (opEx, opRes) <- getIdentExample
             (lEx, lRes) <- getTypeExample
             (rEx, rRes) <- getTypeExample
             withSameLocation $
                 return
                     ( ConstrInfix lEx opEx rEx
                     , D.ConstrSimple opRes [lRes, rRes])
        , do (nameEx, nameRes) <- getIdentExample
             (argsEx, argsRes) <- randomList 2 getFieldDeclExample
             withSameLocation $
                 return
                     ( ConstrRecord nameEx argsEx
                     , D.ConstrRecord nameRes (concat argsRes))
        ]

getFieldDeclExample ::
       RandomSelector (WithLocation FieldDecl, [WithLocation D.FieldDecl])
getFieldDeclExample = do
    (varsEx, varsRes) <- randomNonEmpty 3 getIdentExample
    (typeEx, typeRes) <- getTypeExample
    loc <- getRandomSourceLocation
    let processSingle var = WithLocation (D.FieldDecl var typeRes) loc
        ex = FieldDecl varsEx typeEx
    return (WithLocation ex loc, map processSingle (NE.toList varsRes))

testSuite :: IO ()
testSuite =
    hspec $ do
        describe "desugarToConstr" $
            it "should desugar Constr" $
            checkDesugaring desugarToConstr getConstrExample
        describe "desugarToFieldDecl" $
            it "should desugar FieldDecl" $
            checkDesugaring desugarToFieldDecl getFieldDeclExample
