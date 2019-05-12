{- |
Module      :  Frontend.Syntax.Utils.AstExamples
Description :  Generator of random examples for AST
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module generates random abstract syntax trees.
-}
module Frontend.Syntax.Utils.AstExamples where

import Control.Applicative (liftA2)
import Control.Monad (replicateM)
import qualified Data.HashMap.Lazy as HM (elems)

import Frontend.Syntax.Ast
import Frontend.Syntax.Token
import Frontend.Syntax.Utils.RandomSelector (RandomSelector, selectRandom)

-- | Class of types which have examples for testing
class WithExamples a where
    getExample :: RandomSelector a -- ^ Select one random example
    -- | Select few random examples
    getExamples ::
           Int -- ^ Number of examples to select
        -> RandomSelector [a]
    getExamples n = replicateM n getExample

instance (WithExamples a) => WithExamples (Maybe a) where
    getExample = do
        example <- getExample
        selectRandom [Nothing, Just example]

instance (WithExamples a, WithExamples b) => WithExamples (Either a b) where
    getExample = do
        first <- getExample
        second <- getExample
        selectRandom [Left first, Right second]

instance (WithExamples a) => WithExamples [a] where
    getExample = do
        first <- getExample
        second <- getExample
        selectRandom [[], [first], [second], [first, second]]

instance WithExamples IntT where
    getExample = selectRandom $ map IntT [0, 1, 2]

instance WithExamples FloatT where
    getExample = selectRandom $ map FloatT [0.0, 1.0, 2.0]

instance WithExamples CharT where
    getExample = selectRandom $ map CharT ['a', 'b', 'c']

instance WithExamples StringT where
    getExample = selectRandom $ map StringT ["a", "b", "c"]

instance WithExamples Keyword where
    getExample = selectRandom $ HM.elems keywords

instance WithExamples Operator where
    getExample = selectRandom $ HM.elems operators

instance WithExamples Special where
    getExample = selectRandom $ HM.elems specialSymbols

instance WithExamples EOF where
    getExample = return EOF

instance WithExamples ConId where
    getExample = selectRandom [ConId "Constructor", ConId "Type", ConId "Class"]

instance WithExamples ConSym where
    getExample = selectRandom [ConSym ":", ConSym ":|", ConSym ":+"]

instance WithExamples VarId where
    getExample = selectRandom [VarId "foo", VarId "a", VarId "x"]

instance WithExamples VarSym where
    getExample = selectRandom [VarSym "+", VarSym "*", VarSym "-"]

instance (WithExamples a) => WithExamples (Qualified a) where
    getExample = liftA2 Qualified getExample getExample
