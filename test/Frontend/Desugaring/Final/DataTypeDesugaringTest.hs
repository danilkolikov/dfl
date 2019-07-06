{- |
Module      :  Frontend.Desugaring.Final.DataTypeDesugaringTest
Description :  Tests for desugaring of data types
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of data types
-}
module Frontend.Desugaring.Final.DataTypeDesugaringTest
    ( testSuite
    ) where

import Test.Hspec

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast hiding (getDataTypeConstructors)
import Frontend.Desugaring.Final.DataTypeDesugaring
import Frontend.Desugaring.Final.Processor
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (withDummyLocation)

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarDataType" $ do
        it "ignores all definitions, except for data type" $
            runDesugaringProcessor
                (desugarDataType (I.TopDeclAssignment undefined))
                emptyDesugaringState `shouldBe`
            Right (Nothing, emptyDesugaringState)
        let typeName = IdentNamed ["Type"]
            typeName' = withDummyLocation typeName
            typeArgs = [withDummyLocation $ IdentNamed ["a"]]
            type' =
                withDummyLocation $
                TypeConstr (withDummyLocation $ IdentNamed ["A"])
            simpleType = withDummyLocation $ I.SimpleType typeName' typeArgs
        it "desugars data types" $ do
            let constrName1 = IdentNamed ["Constr1"]
                constrName1' = withDummyLocation constrName1
                getter1 = IdentNamed ["getType1"]
                getter1' = withDummyLocation getter1
                getter2 = IdentNamed ["getType2"]
                getter2' = withDummyLocation getter2
                constr1 =
                    withDummyLocation $
                    I.ConstrRecord
                        constrName1'
                        [ withDummyLocation $ I.FieldDecl getter1' type'
                        , withDummyLocation $ I.FieldDecl getter2' type'
                        ]
                constrName2 = IdentNamed ["Constr2"]
                constrName2' = withDummyLocation constrName2
                constr2 = withDummyLocation $ I.ConstrSimple constrName2' []
                constrs = [constr1, constr2]
                topDecl = I.TopDeclData [] simpleType constrs []
                resConstr1 =
                    ( constrName1
                    , Constructor
                          constrName1'
                          [type', type']
                          (HM.fromList [(getter1, 0), (getter2, 1)]))
                resConstr2 = (constrName2, Constructor constrName2' [] HM.empty)
                res =
                    DataType
                        []
                        typeName'
                        typeArgs
                        []
                        [resConstr1, resConstr2]
                        False
            runDesugaringProcessor
                (desugarDataType topDecl)
                emptyDesugaringState `shouldBe`
                Right
                    ( Just (typeName, res)
                    , emptyDesugaringState
                          { getDefinedTypeNames =
                                HM.singleton typeName typeName'
                          , getDataTypeFields =
                                HM.fromList [(getter1, res), (getter2, res)]
                          , getDataTypeConstructors =
                                HM.fromList
                                    [(constrName1, res), (constrName2, res)]
                          })
        it "desugars simple newtypes" $ do
            let newConstr =
                    withDummyLocation $ I.NewConstrSimple typeName' type'
                topDecl = I.TopDeclNewType [] simpleType newConstr []
                resConstr = (typeName, Constructor typeName' [type'] HM.empty)
                res = DataType [] typeName' typeArgs [] [resConstr] True
            runDesugaringProcessor
                (desugarDataType topDecl)
                emptyDesugaringState `shouldBe`
                Right
                    ( Just (typeName, res)
                    , emptyDesugaringState
                          { getDefinedTypeNames =
                                HM.singleton typeName typeName'
                          , getDataTypeConstructors = HM.singleton typeName res
                          })
        it "desugars record-style newtypes" $ do
            let getter = IdentNamed ["getType"]
                getter' = withDummyLocation getter
                newConstr =
                    withDummyLocation $
                    I.NewConstrRecord typeName' getter' type'
                topDecl = I.TopDeclNewType [] simpleType newConstr []
                resConstr =
                    ( typeName
                    , Constructor typeName' [type'] (HM.singleton getter 0))
                res = DataType [] typeName' typeArgs [] [resConstr] True
            runDesugaringProcessor
                (desugarDataType topDecl)
                emptyDesugaringState `shouldBe`
                Right
                    ( Just (typeName, res)
                    , emptyDesugaringState
                          { getDefinedTypeNames =
                                HM.singleton typeName typeName'
                          , getDataTypeFields = HM.singleton getter res
                          , getDataTypeConstructors = HM.singleton typeName res
                          })
