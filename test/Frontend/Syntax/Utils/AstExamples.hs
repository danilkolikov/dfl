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
import Frontend.Syntax.Position (WithLocation(..), sourceLocation)
import Frontend.Syntax.Token
import Frontend.Syntax.Utils.RandomSelector
    ( RandomSelector
    , selectFromRandom
    , selectRandom
    )

-- | Class of types which have examples for testing
class WithExamples a where
    getExample :: RandomSelector a -- ^ Select one random example
    -- | Select few random examples
    getExamples ::
           Int -- ^ Number of examples to select
        -> RandomSelector [a]
    getExamples n = replicateM n getExample

instance (WithExamples a) => WithExamples (Maybe a) where
    getExample = selectFromRandom [return Nothing, liftE1 Just]

instance (WithExamples a, WithExamples b) => WithExamples (Either a b) where
    getExample = selectFromRandom [liftE1 Left, liftE1 Right]

instance (WithExamples a) => WithExamples [a] where
    getExample =
        selectFromRandom [return [], liftE1 (: []), liftE2 (\x y -> [x, y])]

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

instance (WithExamples a) => WithExamples (WithLocation a) where
    getExample = (`WithLocation` sourceLocation 1 1 1 1) <$> getExample

instance WithExamples Literal where
    getExample =
        selectFromRandom
            [ liftE1 LiteralInteger
            , liftE1 LiteralFloat
            , liftE1 LiteralChar
            , liftE1 LiteralString
            ]

instance WithExamples GCon where
    getExample =
        selectFromRandom
            [ liftE1 GConNamed
            , return GConUnit
            , return GConList
            , selectRandom [GConTuple 2, GConTuple 3]
            ]

instance WithExamples GTyCon where
    getExample =
        selectFromRandom
            [ liftE1 GTyConNamed
            , return GTyConUnit
            , return GTyConList
            , selectRandom [GTyConTuple 2, GTyConTuple 3]
            , return GTyConFunction
            ]

instance (WithExamples a, WithExamples b) => WithExamples (FuncLabel a b) where
    getExample = selectFromRandom [liftE1 FuncLabelId, liftE1 FuncLabelSym]

instance (WithExamples a, WithExamples b) => WithExamples (OpLabel a b) where
    getExample = selectFromRandom [liftE1 OpLabelSym, liftE1 OpLabelId]

instance WithExamples GConSym where
    getExample = selectFromRandom [return GConSymColon, liftE1 GConSymOp]


-- Helper functions

-- | Gets a random value and applies the provided function to it
liftE1 :: (WithExamples a) => (a -> b) -> RandomSelector b
liftE1 f = f <$> getExample

-- | Gets 2 random values and applies the provided function to them
liftE2 :: (WithExamples a, WithExamples b) => (a -> b -> c) -> RandomSelector c
liftE2 f = liftA2 f getExample getExample
