{- |
Module      :  Frontend.Desugaring.Initial.ToTopDeclTest
Description :  Tests for desugaring of object to TopDecl-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to TopDecl-s
-}
module Frontend.Desugaring.Initial.ToTopDeclTest
    ( testSuite
    , getTopDeclExample
    ) where

import Test.Hspec

import Data.Functor (($>))

import qualified Frontend.Desugaring.Initial.Ast as D (TopDecl(..))
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToClassAssignmentTest
    ( getClassAssignmentExample
    )
import Frontend.Desugaring.Initial.ToConstrTest (getConstrExample)
import Frontend.Desugaring.Initial.ToConstraintTest (getConstraintExample)
import Frontend.Desugaring.Initial.ToExpTest (getAssignmentExample)
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Desugaring.Initial.ToInstAssignmentTest
    ( getInstAssignmentExample
    )
import Frontend.Desugaring.Initial.ToInstTest (getInstExample)
import Frontend.Desugaring.Initial.ToNewConstrTest (getNewConstrExample)
import Frontend.Desugaring.Initial.ToSimpleClassTest (getSimpleClassExample)
import Frontend.Desugaring.Initial.ToSimpleTypeTest (getSimpleTypeExample)
import Frontend.Desugaring.Initial.ToTopDecl (desugarToTopDecl)
import Frontend.Desugaring.Initial.ToTypeTest (getTypeExample)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getTopDeclExample ::
       RandomSelector (WithLocation TopDecl, [WithLocation D.TopDecl])
getTopDeclExample = do
    let withLocation rs = do
            (ex, res) <- rs
            loc <- getRandomSourceLocation
            return (WithLocation ex loc, [WithLocation res loc])
    selectFromRandom
        [ withLocation $ do
              (nameEx, nameRes) <- getSimpleTypeExample
              (typeEx, typeRes) <- getTypeExample
              return (TopDeclType nameEx typeEx, D.TopDeclType nameRes typeRes)
        , withLocation $ do
              (contextEx, contextRes) <- randomList 2 getConstraintExample
              (nameEx, nameRes) <- getSimpleTypeExample
              (constrEx, constrRes) <- randomList 2 getConstrExample
              (derivingEx, derivingRes) <- randomList 2 getIdentExample
              return
                  ( TopDeclData contextEx nameEx constrEx derivingEx
                  , D.TopDeclData contextRes nameRes constrRes derivingRes)
        , withLocation $ do
              (contextEx, contextRes) <- randomList 2 getConstraintExample
              (nameEx, nameRes) <- getSimpleTypeExample
              (constrEx, constrRes) <- getNewConstrExample
              (derivingEx, derivingRes) <- randomList 2 getIdentExample
              return
                  ( TopDeclNewType contextEx nameEx constrEx derivingEx
                  , D.TopDeclNewType contextRes nameRes constrRes derivingRes)
        , withLocation $ do
              (contextEx, contextRes) <- randomList 2 getSimpleClassExample
              (nameEx, nameRes) <- getIdentExample
              (varEx, varRes) <- getIdentExample
              (methodsEx, methodsRes) <- randomList 3 getClassAssignmentExample
              return
                  ( TopDeclClass contextEx nameEx varEx methodsEx
                  , D.TopDeclClass contextRes nameRes varRes (concat methodsRes))
        , withLocation $ do
              (contextEx, contextRes) <- randomList 2 getSimpleClassExample
              (nameEx, nameRes) <- getIdentExample
              (instEx, instRes) <- getInstExample
              (methodsEx, methodsRes) <- randomList 3 getInstAssignmentExample
              return
                  ( TopDeclInstance contextEx nameEx instEx methodsEx
                  , D.TopDeclInstance contextRes nameRes instRes methodsRes)
        , do (assignEx, assignRes) <- getAssignmentExample
             return
                 ( assignEx $> TopDeclDecl assignEx
                 , map (\res -> res $> D.TopDeclAssignment res) assignRes)
        ]

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToTopDecl" $
    it "should desugar TopDecl" $
    checkDesugaring 10 2 desugarToTopDecl getTopDeclExample
