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
    , getSimpleIdentExample
    , IdentExample
    ) where

import Test.Hspec hiding (example)

import Data.Functor (($>))

import Core.Ident
import Core.PredefinedIdents
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdent
    ( DesugarToIdent(..)
    , DesugarToSimpleIdent(..)
    )
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token
import Frontend.Utils.RandomSelector

type IdentExample a
     = RandomSelector (WithLocation a, WithLocation UserDefinedIdent)

type SimpleIdentExample a = RandomSelector (a, SimpleIdent)

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

class WithSimpleIdentExamples a where
    getSimpleIdentExample :: SimpleIdentExample a

instance WithSimpleIdentExamples VarId where
    getSimpleIdentExample = return (VarId "id", IdentNamed "id")

instance WithSimpleIdentExamples ConId where
    getSimpleIdentExample = return (ConId "Con", IdentNamed "Con")

instance WithSimpleIdentExamples VarSym where
    getSimpleIdentExample = return (VarSym "+", IdentNamed "+")

instance WithSimpleIdentExamples ConSym where
    getSimpleIdentExample = return (ConSym ":|", IdentNamed ":|")

instance WithIdentExamples VarId where
    getIdentExample =
        withSameLocation $ return (VarId "id", IdentSimple $ IdentNamed "id")

instance WithIdentExamples ConId where
    getIdentExample =
        withSameLocation $ return (ConId "Con", IdentSimple $ IdentNamed "Con")

instance WithIdentExamples VarSym where
    getIdentExample =
        withSameLocation $ return (VarSym "+", IdentSimple $ IdentNamed "+")

instance WithIdentExamples ConSym where
    getIdentExample =
        withSameLocation $ return (ConSym ":|", IdentSimple $ IdentNamed ":|")

instance (WithSimpleIdentExamples a, WithSimpleIdentExamples b) =>
         WithSimpleIdentExamples (FuncLabel a b) where
    getSimpleIdentExample =
        selectFromRandom
            [ do (example, res) <- getSimpleIdentExample
                 return (FuncLabelId example, res)
            , do (example, res) <- getSimpleIdentExample
                 return (FuncLabelSym example, res)
            ]

instance (WithSimpleIdentExamples a, WithSimpleIdentExamples b) =>
         WithSimpleIdentExamples (OpLabel a b) where
    getSimpleIdentExample =
        selectFromRandom
            [ do (example, res) <- getSimpleIdentExample
                 return (OpLabelId example, res)
            , do (example, res) <- getSimpleIdentExample
                 return (OpLabelSym example, res)
            ]

instance (WithSimpleIdentExamples a) => WithIdentExamples (Qualified a) where
    getIdentExample = do
        ex <- withSameLocation getSimpleIdentExample
        let (WithLocation example l1, WithLocation name l2) = ex
            newExample = Qualified [ConId "Module", ConId "Nested"] example
            newRes = IdentQualified ["Module", "Nested"] name
        return (WithLocation newExample l1, WithLocation newRes l2)

instance WithIdentExamples GConSym where
    getIdentExample =
        selectFromRandom
            [ withSameLocation $ return (GConSymColon, cOLON)
            , do (WithLocation example loc, res) <- getIdentExample
                 return (WithLocation (GConSymOp example) loc, res)
            ]

instance WithIdentExamples GCon where
    getIdentExample =
        selectFromRandom
            [ withSameLocation $ return (GConList, lIST)
            , withSameLocation $ return (GConUnit, uNIT)
            , withSameLocation $ return (GConTuple 5, tUPLE 5)
            , do (example, res) <- getIdentExample
                 return (example $> GConNamed example, res)
            ]

instance WithIdentExamples GTyCon where
    getIdentExample =
        selectFromRandom
            [ withSameLocation $ return (GTyConList, lIST)
            , withSameLocation $ return (GTyConUnit, uNIT)
            , withSameLocation $ return (GTyConTuple 5, tUPLE 5)
            , withSameLocation $ return (GTyConFunction, fUNCTION)
            , do (example, res) <- getIdentExample
                 return (example $> GTyConNamed example, res)
            ]

instance WithIdentExamples DClass where
    getIdentExample = do
        (example, res) <- getIdentExample
        return (example $> DClass example, res)

checkIdentDesugaring ::
       (WithIdentExamples a, DesugarToIdent a) => IdentExample a -> Expectation
checkIdentDesugaring = checkDesugaring desugarToIdent

checkSimpleIdentDesugaring ::
       (WithSimpleIdentExamples a, DesugarToSimpleIdent a)
    => SimpleIdentExample a
    -> Expectation
checkSimpleIdentDesugaring = checkDesugaring desugarToSimpleIdent

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToIdent" $ do
        it "should desugar simple names" $ do
            checkSimpleIdentDesugaring
                (getSimpleIdentExample :: SimpleIdentExample VarId)
            checkSimpleIdentDesugaring
                (getSimpleIdentExample :: SimpleIdentExample ConId)
            checkSimpleIdentDesugaring
                (getSimpleIdentExample :: SimpleIdentExample VarSym)
            checkSimpleIdentDesugaring
                (getSimpleIdentExample :: SimpleIdentExample ConSym)
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
