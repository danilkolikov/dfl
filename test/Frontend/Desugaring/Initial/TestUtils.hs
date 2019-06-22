{- |
Module      :  Frontend.Desugaring.Initial.TestUtils
Description :  Utility functins
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for testing of desugaring
-}
module Frontend.Desugaring.Initial.TestUtils where

import Test.Hspec hiding (example)

import Control.Monad (liftM2, replicateM)
import Data.Bifunctor (bimap)
import qualified Data.List.NonEmpty as NE

import Frontend.Syntax.Position
import Frontend.Utils.RandomSelector

mAX_LINE :: Int
mAX_LINE = 1000

mAX_COLUMN :: Int
mAX_COLUMN = 1000

sEED :: Int
sEED = 42

n_TEST_CASES :: Int
n_TEST_CASES = 10

dEPTH :: Int
dEPTH = 2

getRandomLine :: RandomSelector Int
getRandomLine = randomNumber mAX_LINE

getRandomColumn :: RandomSelector Int
getRandomColumn = randomNumber mAX_COLUMN

getRandomSourcePosition :: RandomSelector SourcePosition
getRandomSourcePosition = liftM2 SourcePosition getRandomLine getRandomColumn

getRandomSourceLocation :: RandomSelector SourceLocation
getRandomSourceLocation =
    liftM2 SourceLocation getRandomSourcePosition getRandomSourcePosition

withSameLocation ::
       RandomSelector (a, b) -> RandomSelector (WithLocation a, WithLocation b)
withSameLocation rs = do
    location <- getRandomSourceLocation
    bimap (`WithLocation` location) (`WithLocation` location) <$> rs

randomList :: Int -> RandomSelector (a, b) -> RandomSelector ([a], [b])
randomList n rs = unzip <$> replicateM n rs

randomNonEmpty ::
       Int
    -> RandomSelector (a, b)
    -> RandomSelector (NE.NonEmpty a, NE.NonEmpty b)
randomNonEmpty n rs
    | n > 0 = bimap NE.fromList NE.fromList <$> randomList n rs
    | otherwise = undefined

randomMaybe :: RandomSelector (a, b) -> RandomSelector (Maybe a, Maybe b)
randomMaybe rs =
    selectFromRandom [return (Nothing, Nothing), bimap Just Just <$> rs]

checkDesugaring ::
       (Eq b, Show b) => (a -> b) -> RandomSelector (a, b) -> Expectation
checkDesugaring desugar rs =
    let checkSingle (example, res) = desugar example `shouldBe` res
        getExamples = replicateM n_TEST_CASES rs
        examples = evalRandomSelector getExamples sEED dEPTH
     in mapM_ checkSingle examples
