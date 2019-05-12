{- |
Module      :  Frontend.Syntax.Utils.RandomSelector
Description :  Selector of random values
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module defines functions for selection of a random value from a list.
-}
module Frontend.Syntax.Utils.RandomSelector
    ( RandomSelector
    , evalRandomSelector
    , selectRandom
    , selectFromRandom
    , selectFromRandomRecursive
    ) where

import qualified Control.Monad.Trans.State as ST (State, evalState, get, modify)
import System.Random (StdGen, mkStdGen, randomR)

data RandomSelectorState = RandomSelectorState
    { getRandomGen :: StdGen
    , getCurrentDepth :: Int
    }

-- | Type of object which selects random value
type RandomSelector a = ST.State RandomSelectorState a

-- | Select a random value
evalRandomSelector ::
       RandomSelector a -- ^ Select of random values
    -> Int -- ^ Seed for initialisation of random generator
    -> Int -- ^ Maximal depth of generated structures
    -> a -- ^ Selected random value
evalRandomSelector r seed depth =
    ST.evalState
        r
        (RandomSelectorState
             {getRandomGen = mkStdGen seed, getCurrentDepth = depth})

-- | Select random value from the provided list
selectRandom :: [a] -> RandomSelector a
selectRandom = selectFromRandom . map return

-- | Select a random value from the provided list of random selectors
selectFromRandom :: [RandomSelector a] -> RandomSelector a
selectFromRandom rs = do
    state <- ST.get
    let (pos, nextGen) = randomR (0, length rs - 1) (getRandomGen state)
        selected = rs !! pos
    ST.modify $ \st -> st {getRandomGen = nextGen}
    selected

selectFromRandomRecursive ::
       [RandomSelector a] -- ^ Non-recursive alternatives
    -> [RandomSelector a] -- ^ Recursive alternatives
    -> RandomSelector a
selectFromRandomRecursive nonRec rec = do
    state <- ST.get
    let currentDepth = getCurrentDepth state
        reachedMinDepth = currentDepth <= 0
        alts =
            if reachedMinDepth
                then nonRec
                else nonRec ++ rec
        (pos, nextGen) = randomR (0, length alts - 1) (getRandomGen state)
        selected = alts !! pos
    -- If we selected non-recursive alternative, don't modify depth
    -- Otherwise, decrease depth, select 1 alternative and set the depth back
    if pos < length nonRec
        then do
            ST.modify $ \st -> st {getRandomGen = nextGen}
            selected
        else do
            ST.modify $ \st ->
                st {getRandomGen = nextGen, getCurrentDepth = currentDepth - 1}
            res <- selected
          -- After we got result from a chosen selector, we should reset depth back
            ST.modify $ \st -> st {getCurrentDepth = currentDepth}
            return res
