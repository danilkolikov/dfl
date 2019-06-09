{- |
Module      :  Frontend.Desugaring.Initial.ToIdentTest
Description :  Tests for desugaring of object to Ident-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Ident-s
-}
module Frontend.Desugaring.Initial.ToIdentTest
    ( testSuite
    , getIdentExample
    ) where

import Test.Hspec hiding (example)

import Data.Functor (($>))

import Frontend.Desugaring.Initial.Ast (Ident(..))
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdent (DesugarToIdent(..))
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token
import Frontend.Utils.RandomSelector

type IdentExample a = RandomSelector (WithLocation a, WithLocation Ident)

class WithIdentExamples a where
    getIdentExample :: IdentExample a

instance (WithIdentExamples a, WithIdentExamples b) =>
         WithIdentExamples (FuncLabel a b) where
    getIdentExample =
        selectFromRandom
            [ do (example, res) <- getIdentExample
                 return (FuncLabelId <$> example, res)
            , do (example, res) <- getIdentExample
                 return (FuncLabelSym <$> example, res)
            ]

instance (WithIdentExamples a, WithIdentExamples b) =>
         WithIdentExamples (OpLabel a b) where
    getIdentExample =
        selectFromRandom
            [ do (example, res) <- getIdentExample
                 return (OpLabelId <$> example, res)
            , do (example, res) <- getIdentExample
                 return (OpLabelSym <$> example, res)
            ]

instance (WithIdentExamples a, WithIdentExamples b) =>
         WithIdentExamples (Either a b) where
    getIdentExample =
        selectFromRandom
            [ do (example, res) <- getIdentExample
                 return (Left <$> example, res)
            , do (example, res) <- getIdentExample
                 return (Right <$> example, res)
            ]

instance WithIdentExamples VarId where
    getIdentExample = withSameLocation $ return (VarId "id", IdentNamed ["id"])

instance WithIdentExamples ConId where
    getIdentExample =
        withSameLocation $ return (ConId "Con", IdentNamed ["Con"])

instance WithIdentExamples VarSym where
    getIdentExample = withSameLocation $ return (VarSym "+", IdentNamed ["+"])

instance WithIdentExamples ConSym where
    getIdentExample = withSameLocation $ return (ConSym ":|", IdentNamed [":|"])

instance (WithIdentExamples a) => WithIdentExamples (Qualified a) where
    getIdentExample = do
        ex <- getIdentExample
        let (WithLocation example l1, WithLocation (IdentNamed name) l2) = ex
            newExample = Qualified [ConId "Module", ConId "Nested"] example
            newRes = IdentNamed $ ["Module", "Nested"] ++ name
        return (WithLocation newExample l1, WithLocation newRes l2)

instance WithIdentExamples GConSym where
    getIdentExample =
        selectFromRandom
            [ withSameLocation $ return (GConSymColon, IdentNamed cOLON_NAME)
            , do (WithLocation example loc, res) <- getIdentExample
                 return (WithLocation (GConSymOp example) loc, res)
            ]

instance WithIdentExamples GCon where
    getIdentExample =
        selectFromRandom
            [ withSameLocation $ return (GConList, IdentNamed lIST_NAME)
            , withSameLocation $ return (GConUnit, IdentNamed uNIT_NAME)
            , withSameLocation $
              return (GConTuple 5, IdentParametrised tUPLE_NAME 5)
            , do (example, res) <- getIdentExample
                 return (example $> GConNamed example, res)
            ]

instance WithIdentExamples GTyCon where
    getIdentExample =
        selectFromRandom
            [ withSameLocation $ return (GTyConList, IdentNamed lIST_NAME)
            , withSameLocation $ return (GTyConUnit, IdentNamed uNIT_NAME)
            , withSameLocation $
              return (GTyConTuple 5, IdentParametrised tUPLE_NAME 5)
            , withSameLocation $
              return (GTyConFunction, IdentNamed fUNCTION_NAME)
            , do (example, res) <- getIdentExample
                 return (example $> GTyConNamed example, res)
            ]

instance WithIdentExamples DClass where
    getIdentExample = do
        (example, res) <- getIdentExample
        return (example $> DClass example, res)

checkIdentDesugaring ::
       (WithIdentExamples a, DesugarToIdent a) => IdentExample a -> Expectation
checkIdentDesugaring = checkDesugaring 10 1 desugarToIdent

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToIdent" $ do
        it "should desugar simple names" $ do
            checkIdentDesugaring (getIdentExample :: IdentExample VarId)
            checkIdentDesugaring (getIdentExample :: IdentExample ConId)
            checkIdentDesugaring (getIdentExample :: IdentExample VarSym)
            checkIdentDesugaring (getIdentExample :: IdentExample ConSym)
        it "should desugar qualified IDs" $
            checkIdentDesugaring
                (getIdentExample :: IdentExample (Qualified VarId))
        it "should desugar GCon" $
            checkIdentDesugaring (getIdentExample :: IdentExample GCon)
        it "should desugar GTyCon" $
            checkIdentDesugaring (getIdentExample :: IdentExample GTyCon)
        it "should desugar GConSym" $
            checkIdentDesugaring (getIdentExample :: IdentExample GConSym)
        it "should desugar DClass" $
            checkIdentDesugaring (getIdentExample :: IdentExample DClass)
