{- |
Module      :  Frontend.Desugaring.Record.Test
Description :  Tests for record desugaring
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the processor of desugaring of records
-}
module Frontend.Desugaring.Record.Test
    ( testSuite
    ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Test.Hspec

import Core.PredefinedIdents
import qualified Frontend.Desugaring.Final.Ast as F
import qualified Frontend.Desugaring.Fixity.Ast as I
import Frontend.Desugaring.Record.Ast
import Frontend.Desugaring.Record.Base
import Frontend.Desugaring.Record.Expression
import Frontend.Syntax.Position (withDummyLocation)

testSuite :: IO ()
testSuite =
    hspec $ do
        let emptyDataType =
                F.DataType
                    []
                    (withDummyLocation . IdentUserDefined . IdentSimple $
                     IdentNamed "Void")
                    []
                    []
                    []
                    False
        describe "findDataTypeByField" $ do
            let name = IdentUserDefined . IdentSimple $ IdentNamed "abc"
                name' = withDummyLocation name
            it "finds a data type by the field name" $
                runRecordDesugaringProcessor
                    (findDataTypeByField name')
                    (HM.singleton name emptyDataType)
                    HM.empty `shouldBe`
                Right emptyDataType
            it "fails when there is no data type with a such field" $
                runRecordDesugaringProcessor
                    (findDataTypeByField name')
                    HM.empty
                    HM.empty `shouldBe`
                Left (RecordDesugaringErrorUnknownField name')
        describe "findDataTypeByConstructor" $ do
            let name = IdentUserDefined . IdentSimple $ IdentNamed "Abc"
                name' = withDummyLocation name
            it "finds a data type by the constructor name" $
                runRecordDesugaringProcessor
                    (findDataTypeByConstructor name')
                    HM.empty
                    (HM.singleton name emptyDataType) `shouldBe`
                Right emptyDataType
            it "fails when there is no data type with a such field" $
                runRecordDesugaringProcessor
                    (findDataTypeByConstructor name')
                    HM.empty
                    HM.empty `shouldBe`
                Left (RecordDesugaringErrorUnknownConstructor name')
        describe "lookupConstructor" $ do
            let unitName = IdentUserDefined . IdentSimple $ IdentNamed "Unit"
                unitName' = withDummyLocation unitName
                unitConstructor = F.Constructor unitName' [] HM.empty
                unitDataType =
                    F.DataType
                        []
                        unitName'
                        []
                        []
                        [(unitName, unitConstructor)]
                        False
            it "finds constructor by a name" $
                runRecordDesugaringExcept
                    (lookupConstructor unitName' unitDataType) `shouldBe`
                Right unitConstructor
            it "failes when there is no such constructor" $
                runRecordDesugaringExcept
                    (lookupConstructor unitName' emptyDataType) `shouldBe`
                Left (RecordDesugaringErrorUnknownConstructor unitName')
        let field1 = IdentUserDefined . IdentSimple $ IdentNamed "field1"
            field1' = withDummyLocation field1
            field2 = IdentUserDefined . IdentSimple $ IdentNamed "field2"
            field2' = withDummyLocation field2
            field3 = IdentUserDefined . IdentSimple $ IdentNamed "field3"
            field3' = withDummyLocation field3
        describe "checkForDuplicateFields" $ do
            it "succeeds when all fields are different" $
                runRecordDesugaringExcept
                    (checkForDuplicateFields [(field1', 'a'), (field2', 'b')]) `shouldBe`
                Right (HM.fromList [(field1, field1'), (field2, field2')])
            it "fails when there are duplicate fields" $
                runRecordDesugaringExcept
                    (checkForDuplicateFields [(field1', 'a'), (field1', 'a')]) `shouldBe`
                Left (RecordDesugaringErrorDuplicateField field1' field1')
        let type' =
                withDummyLocation .
                TypeConstr .
                withDummyLocation . IdentUserDefined . IdentSimple . IdentNamed $
                "Int"
            constrName = IdentUserDefined . IdentSimple $ IdentNamed "Tuple"
            constrName' = withDummyLocation constrName
            constructor =
                F.Constructor
                    constrName'
                    [type', type']
                    (HM.fromList [(field1, 0), (field2, 1)])
            dataType =
                F.DataType
                    []
                    constrName'
                    []
                    []
                    [(constrName, constructor)]
                    False
        describe "resolveBindings" $ do
            it "should resolve bindings" $
                runRecordDesugaringExcept
                    (resolveBindings
                         constructor
                         id
                         [(field1', const (-1)), (field2', const (-2))]) `shouldBe`
                Right [-1, -2]
            it "should fill missing arguments" $ do
                runRecordDesugaringExcept
                    (resolveBindings constructor id [(field1', const (-1))]) `shouldBe`
                    Right [-1, 1]
                runRecordDesugaringExcept
                    (resolveBindings constructor id [(field2', const (-2))]) `shouldBe`
                    Right [0, -2]
            it "should raise an error in case of unknown field" $
                runRecordDesugaringExcept
                    (resolveBindings
                         constructor
                         id
                         [(field3', const (-1)), (field2', const (-2))]) `shouldBe`
                Left (RecordDesugaringErrorUnknownField field3')
        describe "filterRequiredConstructors" $
            it "should find constructors, containing all provided fields" $ do
                filterRequiredConstructors
                    [field1, field2]
                    [constructor, constructor] `shouldBe`
                    [constructor, constructor]
                filterRequiredConstructors
                    [field1, field3]
                    [constructor, constructor] `shouldBe`
                    []
        describe "makeUpdateAlternative" $ do
            let exp' =
                    withDummyLocation .
                    ExpVar .
                    withDummyLocation .
                    IdentUserDefined . IdentSimple . IdentNamed $
                    "x"
                makePattern' =
                    withDummyLocation .
                    (`PatternVar` Nothing) .
                    withDummyLocation .
                    IdentGenerated .
                    GeneratedIdent GeneratedIdentEnvironmentRecordDesugaring
                expectedPattern =
                    withDummyLocation $
                    PatternConstr constrName' [makePattern' 0, makePattern' 1]
                expectedExp =
                    withDummyLocation $
                    ExpApplication
                        (withDummyLocation $ ExpConstr constrName')
                        ((withDummyLocation . ExpVar . withDummyLocation $
                          IdentGenerated
                              (GeneratedIdent
                                   GeneratedIdentEnvironmentRecordDesugaring
                                   0)) NE.:|
                         [exp'])
            it "makes alternative for record update" $
                runRecordDesugaringExcept
                    (makeUpdateAlternative [(field2', exp')] constructor) `shouldBe`
                Right
                    (withDummyLocation $ AltSimple expectedPattern expectedExp)
        describe "desugarPattern" $ do
            let pVar =
                    withDummyLocation $
                    I.PatternVar
                        (withDummyLocation . IdentUserDefined . IdentSimple $
                         IdentNamed "a")
                        Nothing
                pVarRes =
                    withDummyLocation $
                    PatternVar
                        (withDummyLocation . IdentUserDefined . IdentSimple $
                         IdentNamed "a")
                        Nothing
                pBinding = withDummyLocation $ I.PatternBinding field2' pVar
                pRecord =
                    withDummyLocation $ I.PatternRecord constrName' [pBinding]
                pRecordRes =
                    withDummyLocation $
                    PatternConstr
                        constrName'
                        [withDummyLocation PatternWildcard, pVarRes]
            -- Desugaring of patterns is trivial, except for records
            it "desugars records" $
                runRecordDesugaringProcessor
                    (desugarPattern pRecord)
                    HM.empty
                    (HM.singleton constrName dataType) `shouldBe`
                Right pRecordRes
        describe "desugarExp" $ do
            let var =
                    withDummyLocation $
                    I.ExpVar
                        (withDummyLocation . IdentUserDefined . IdentSimple $
                         IdentNamed "a")
                varRes =
                    withDummyLocation $
                    ExpVar
                        (withDummyLocation . IdentUserDefined . IdentSimple $
                         IdentNamed "a")
                binding = withDummyLocation $ I.Binding field2' var
              -- Desugaring of exps is trivial, except for records
            it "desugars construction of records" $ do
                let exp' =
                        withDummyLocation $
                        I.ExpRecordConstr constrName' [binding]
                    expRes =
                        withDummyLocation $
                        ExpApplication
                            (withDummyLocation . ExpConstr $ constrName')
                            ((withDummyLocation .
                              ExpVar . withDummyLocation . IdentUserDefined $
                              uNDEFINED) NE.:|
                             [varRes])
                runRecordDesugaringProcessor
                    (desugarExp exp')
                    HM.empty
                    (HM.singleton constrName dataType) `shouldBe`
                    Right expRes
            it "desugars updates of records" $ do
                let object =
                        withDummyLocation $
                        I.ExpVar
                            (withDummyLocation . IdentUserDefined . IdentSimple $
                             IdentNamed "x")
                    objectRes =
                        withDummyLocation $
                        ExpVar
                            (withDummyLocation . IdentUserDefined . IdentSimple $
                             IdentNamed "x")
                    exp' =
                        withDummyLocation $
                        I.ExpRecordUpdate object (binding NE.:| [])
                    makePattern' =
                        withDummyLocation .
                        (`PatternVar` Nothing) .
                        withDummyLocation .
                        IdentGenerated .
                        GeneratedIdent GeneratedIdentEnvironmentRecordDesugaring
                    expectedPattern =
                        withDummyLocation $
                        PatternConstr
                            constrName'
                            [makePattern' 0, makePattern' 1]
                    expectedExp =
                        withDummyLocation $
                        ExpApplication
                            (withDummyLocation $ ExpConstr constrName')
                            ((withDummyLocation . ExpVar . withDummyLocation $
                              IdentGenerated
                                  (GeneratedIdent
                                       GeneratedIdentEnvironmentRecordDesugaring
                                       0)) NE.:|
                             [varRes])
                    expRes =
                        withDummyLocation $
                        ExpCase
                            objectRes
                            (withDummyLocation
                                 (AltSimple expectedPattern expectedExp) NE.:|
                             [])
                runRecordDesugaringProcessor
                    (desugarExp exp')
                    (HM.singleton field2 dataType)
                    HM.empty `shouldBe`
                    Right expRes
