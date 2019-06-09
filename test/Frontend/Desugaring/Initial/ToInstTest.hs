{- |
Module      :  Frontend.Desugaring.Initial.ToInstTest
Description :  Tests for desugaring of object to Inst-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Inst-s
-}
module Frontend.Desugaring.Initial.ToInstTest
    ( testSuite
    , getInstExample
    ) where

import Test.Hspec

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Desugaring.Initial.ToInst (desugarToInst)
import Frontend.Desugaring.Initial.Util
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getInstExample :: RandomSelector (WithLocation Inst, WithLocation D.Inst)
getInstExample =
    selectFromRandom
        [ do (nameEx, nameRes) <- getIdentExample
             (argsEx, argsRes) <- randomList 2 getIdentExample
             withSameLocation $
                 return (InstNamed nameEx argsEx, D.Inst nameRes argsRes)
        , do (firstEx, firstRes) <- getIdentExample
             (secondEx, secondRes) <- getIdentExample
             (restEx, restRes) <- randomList 2 getIdentExample
             let ident = makeIdent' $ D.IdentParametrised tUPLE_NAME 4
             withSameLocation $
                 return
                     ( InstTuple firstEx secondEx restEx
                     , D.Inst ident (firstRes : secondRes : restRes))
        , do (argEx, argRes) <- getIdentExample
             let ident = makeIdent lIST_NAME
             withSameLocation $ return (InstList argEx, D.Inst ident [argRes])
        , do (fromEx, fromRes) <- getIdentExample
             (toEx, toRes) <- getIdentExample
             let ident = makeIdent fUNCTION_NAME
             withSameLocation $
                 return
                     (InstFunction fromEx toEx, D.Inst ident [fromRes, toRes])
        ]

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToInst" $
    it "should desugar Inst" $ checkDesugaring 10 1 desugarToInst getInstExample
