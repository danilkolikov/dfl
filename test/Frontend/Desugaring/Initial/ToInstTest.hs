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

import Core.Ident
import Core.PredefinedIdents
import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdentTest
    ( getIdentExample
    , getSimpleIdentExample
    )
import Frontend.Desugaring.Initial.ToInst (desugarToInst)
import Frontend.Desugaring.Initial.Utils
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getInstExample :: RandomSelector (WithLocation Inst, WithLocation D.Inst)
getInstExample =
    selectFromRandom
        [ do (nameEx, nameRes) <- getIdentExample
             (argsEx, argsRes) <-
                 randomList 2 (withSameLocation getSimpleIdentExample)
             withSameLocation $
                 return
                     ( InstNamed nameEx argsEx
                     , D.Inst nameRes (map (IdentSimple <$>) argsRes))
        , do (firstEx, firstRes) <- withSameLocation getSimpleIdentExample
             (secondEx, secondRes) <- withSameLocation getSimpleIdentExample
             (restEx, restRes) <-
                 randomList 2 (withSameLocation getSimpleIdentExample)
             let ident = makeIdent $ tUPLE 4
             withSameLocation $
                 return
                     ( InstTuple firstEx secondEx restEx
                     , D.Inst
                           ident
                           (map (IdentSimple <$>) $
                            firstRes : secondRes : restRes))
        , do (argEx, argRes) <- withSameLocation getSimpleIdentExample
             let ident = makeIdent lIST
             withSameLocation $
                 return
                     ( InstList argEx
                     , D.Inst ident $ map (IdentSimple <$>) [argRes])
        , do (fromEx, fromRes) <- withSameLocation getSimpleIdentExample
             (toEx, toRes) <- withSameLocation getSimpleIdentExample
             let ident = makeIdent fUNCTION
             withSameLocation $
                 return
                     ( InstFunction fromEx toEx
                     , D.Inst ident $ map (IdentSimple <$>) [fromRes, toRes])
        ]

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToInst" $
    it "should desugar Inst" $ checkDesugaring desugarToInst getInstExample
