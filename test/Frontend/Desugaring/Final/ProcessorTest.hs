{- |
Module      :  Frontend.Desugaring.Final.ProcessorTest
Description :  Tests for the desugaring processor of DFL grammar
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the processor of the final step of desugaring of DFL grammar
-}
module Frontend.Desugaring.Final.ProcessorTest
    ( testSuite
    ) where

import Test.Hspec

import Control.Monad (replicateM)
import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Processor
import Frontend.Syntax.Position
    ( WithLocation(..)
    , sourceLocation
    , withDummyLocation
    )

testSuite :: IO ()
testSuite =
    hspec $ do
        describe "defineTypeName" $ do
            let typeName = IdentNamed ["Type"]
                typeName1 = withDummyLocation typeName
            it "defines a new type name" $
                runDesugaringProcessor
                    (defineTypeName typeName1)
                    emptyDesugaringState `shouldBe`
                Right
                    ( ()
                    , emptyDesugaringState
                          { getDefinedTypeNames =
                                HM.singleton typeName typeName1
                          })
            let typeName2 = WithLocation typeName (sourceLocation 1 2 3 4)
            it "throws if a type name is already defined" $
                runDesugaringProcessor
                    (defineTypeName typeName2)
                    (emptyDesugaringState
                         {getDefinedTypeNames = HM.singleton typeName typeName1}) `shouldBe`
                Left (DesugaringErrorNameConflict typeName2 typeName1)
        describe "defineClassName" $ do
            let className = IdentNamed ["Class"]
                className1 = withDummyLocation className
            it "defines a new class name" $
                runDesugaringProcessor
                    (defineClassName className1)
                    emptyDesugaringState `shouldBe`
                Right
                    ( ()
                    , emptyDesugaringState
                          { getDefinedClassNames =
                                HM.singleton className className1
                          })
            let className2 = WithLocation className (sourceLocation 1 2 3 4)
            it "throws if a class name is already defined" $
                runDesugaringProcessor
                    (defineClassName className2)
                    (emptyDesugaringState
                         { getDefinedClassNames =
                               HM.singleton className className1
                         }) `shouldBe`
                Left (DesugaringErrorNameConflict className2 className1)
        describe "defineFunctionName" $ do
            let functionName = IdentNamed ["function"]
                functionName1 = withDummyLocation functionName
            it "defines a new function name" $
                runDesugaringProcessor
                    (defineFunctionName functionName1)
                    emptyDesugaringState `shouldBe`
                Right
                    ( ()
                    , emptyDesugaringState
                          { getDefinedFunctionNames =
                                HM.singleton functionName functionName1
                          })
            let functionName2 =
                    WithLocation functionName (sourceLocation 1 2 3 4)
            it "throws if a function name is already defined" $
                runDesugaringProcessor
                    (defineFunctionName functionName2)
                    (emptyDesugaringState
                         { getDefinedFunctionNames =
                               HM.singleton functionName functionName1
                         }) `shouldBe`
                Left (DesugaringErrorNameConflict functionName2 functionName1)
        describe "defineDataTypeField" $ do
            let fieldName = IdentNamed ["field"]
                fieldName' = withDummyLocation fieldName
                dataType =
                    DataType
                        []
                        (withDummyLocation $ IdentNamed ["Type"])
                        []
                        []
                        []
            it "defines a data type field and a function" $
                runDesugaringProcessor
                    (defineDataTypeField fieldName' dataType)
                    emptyDesugaringState `shouldBe`
                Right
                    ( ()
                    , emptyDesugaringState
                          { getDefinedFunctionNames =
                                HM.singleton fieldName fieldName'
                          , getDataTypeFields = HM.singleton fieldName dataType
                          })
        describe "defineNewTypeField" $ do
            let fieldName = IdentNamed ["field"]
                fieldName' = withDummyLocation fieldName
                newTypeName = IdentNamed ["NewType"]
                newTypeName' = withDummyLocation newTypeName
                newType =
                    NewType
                        []
                        newTypeName'
                        []
                        []
                        (newTypeName, Constructor newTypeName' [] HM.empty)
            it "defines a newtype field and a function" $
                runDesugaringProcessor
                    (defineNewTypeField fieldName' newType)
                    emptyDesugaringState `shouldBe`
                Right
                    ( ()
                    , emptyDesugaringState
                          { getDefinedFunctionNames =
                                HM.singleton fieldName fieldName'
                          , getNewTypeFields = HM.singleton fieldName newType
                          })
        describe "generateNewIdent" $
            it "generates new idents" $
            runDesugaringProcessor
                (replicateM 5 generateNewIdent)
                emptyDesugaringState `shouldBe`
            Right
                ( [IdentGenerated i | i <- [0 .. 4]]
                , emptyDesugaringState {getCurrentIdentCounter = 5})
        describe "collectHashMap" $
            it "collects items to a hash map" $ do
                let collector :: Int -> DesugaringProcessor (Maybe (Ident, Int))
                    collector x =
                        if even x
                            then (\ident -> Just (ident, x)) <$>
                                 generateNewIdent
                            else return Nothing
                runDesugaringProcessor
                    (collectHashMap collector (map withDummyLocation [1 .. 4]))
                    emptyDesugaringState `shouldBe`
                    Right
                        ( HM.fromList
                              [(IdentGenerated 0, 2), (IdentGenerated 1, 4)]
                        , emptyDesugaringState {getCurrentIdentCounter = 2})
