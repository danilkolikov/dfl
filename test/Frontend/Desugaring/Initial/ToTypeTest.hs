{- |
Module      :  Frontend.Desugaring.Initial.ToTypeTest
Description :  Tests for desugaring of object to Type-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Type-s
-}
module Frontend.Desugaring.Initial.ToTypeTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D (Ident(..), Type(..))
import Frontend.Desugaring.Initial.ToType (desugarToType)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position
    ( WithLocation(..)
    , sourceLocation
    , withDummyLocation
    )
import Frontend.Syntax.Token

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToType" $ do
        let aType1 =
                withDummyLocation . ATypeVar . withDummyLocation . VarId $ "a"
            bType1 = withDummyLocation $ BType (aType1 NE.:| [])
            type1 = withDummyLocation $ Type (bType1 NE.:| [])
            type1Expected =
                withDummyLocation . D.TypeVar . withDummyLocation . D.IdentNamed $
                ["a"]
            aType2 =
                withDummyLocation . ATypeVar . withDummyLocation . VarId $ "b"
            bType2 = withDummyLocation $ BType (aType2 NE.:| [])
            type2 = withDummyLocation $ Type (bType2 NE.:| [])
            type2Expected =
                withDummyLocation . D.TypeVar . withDummyLocation . D.IdentNamed $
                ["b"]
        it "should desugar ATypes" $ do
            desugarToType
                (withDummyLocation $
                 ATypeConstructor
                     (WithLocation GTyConList (sourceLocation 1 2 3 4))) `shouldBe`
                withDummyLocation
                    (D.TypeConstr
                         (WithLocation
                              (D.IdentNamed ["[]"])
                              (sourceLocation 1 2 3 4)))
            desugarToType
                (withDummyLocation $
                 ATypeVar (WithLocation (VarId "a") (sourceLocation 1 2 3 4))) `shouldBe`
                withDummyLocation
                    (D.TypeVar
                         (WithLocation
                              (D.IdentNamed ["a"])
                              (sourceLocation 1 2 3 4)))
            desugarToType (withDummyLocation $ ATypeTuple type1 type2 []) `shouldBe`
                withDummyLocation
                    (D.TypeApplication
                         (withDummyLocation
                              (D.TypeConstr
                                   (withDummyLocation
                                        (D.IdentParametrised ["(,)"] 2))))
                         (type1Expected NE.:| [type2Expected]))
            desugarToType (withDummyLocation $ ATypeList type1) `shouldBe`
                withDummyLocation
                    (D.TypeApplication
                         (withDummyLocation
                              (D.TypeConstr
                                   (withDummyLocation (D.IdentNamed ["[]"]))))
                         (type1Expected NE.:| []))
        it "should desugar BType" $
            desugarToType (withDummyLocation $ BType (aType1 NE.:| [aType2])) `shouldBe`
            withDummyLocation
                (D.TypeApplication type1Expected (type2Expected NE.:| []))
        it "should desugar AType" $
            desugarToType (withDummyLocation $ Type (bType1 NE.:| [bType2])) `shouldBe`
            withDummyLocation
                (D.TypeApplication
                     (withDummyLocation
                          (D.TypeConstr
                               (withDummyLocation (D.IdentNamed ["->"]))))
                     (type1Expected NE.:| [type2Expected]))
        it "should keep track of positions" $
            desugarToType
                (WithLocation (getValue type1) (sourceLocation 1 2 3 4)) `shouldBe`
            WithLocation (getValue type1Expected) (sourceLocation 1 2 3 4)
