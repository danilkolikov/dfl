{- |
Module      :  Frontend.Utils.RandomSelector
Description :  Selector of random values
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module defines functions for selection of a random value from a list.
-}
module Frontend.Utils.RandomSelector
    ( RandomSelector
    , evalRandomSelector
    , selectRandom
    , selectFromRandom
    , selectFromRandomWeighted
    , selectFromRandomRecursive
    , selectFromRandomRecursiveWeighted
    , withDecreasedDepth
    , randomNumber
    , randomNumberWeighted
    ) where

import qualified Control.Monad.Trans.State as ST (State, evalState, get, modify)
import Data.List (findIndex, inits)
import Data.Maybe (fromJust)
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

-- | Generate random number from [0, n)
randomNumber :: Int -> RandomSelector Int
randomNumber n = do
    randomGen <- getRandomGen <$> ST.get
    let (res, nextGen) = randomR (0, n - 1) randomGen
    ST.modify $ \st -> st {getRandomGen = nextGen}
    return res

-- | Get a random number from the specified distribution.
randomNumberWeighted ::
       [Int] -- ^ Inverse probability of i-th value
    -> RandomSelector Int
randomNumberWeighted weights = do
    randomSum <- randomNumber (sum weights)
    return $ fromJust $ findIndex (> randomSum) (map sum (tail $ inits weights))

-- | Select random value from the provided list
selectRandom :: [a] -> RandomSelector a
selectRandom = selectFromRandom . map return

-- | Select a random value from the provided list of random selectors
selectFromRandom' ::
       (b -> RandomSelector a) -- ^ Getter of a random selector
    -> ([b] -> RandomSelector Int) -- ^ Getter of a random position
    -> [b] -- ^ List of selectors
    -> RandomSelector a
selectFromRandom' getRandom getPos rs = getPos rs >>= getRandom . (rs !!)

-- | Select a random value from the provided list of random selectors
selectFromRandom :: [RandomSelector a] -> RandomSelector a
selectFromRandom = selectFromRandom' id (randomNumber . length)

-- | Select a random value from the provided list of random selectors
--   with specified weights. Selector with a higher weight will have a higher
--   probability to be selected
selectFromRandomWeighted :: [(RandomSelector a, Int)] -> RandomSelector a
selectFromRandomWeighted =
    selectFromRandom' fst (randomNumberWeighted . map snd)

-- | Get a random value with a decreased depth
withDecreasedDepth :: RandomSelector a -> RandomSelector a
withDecreasedDepth r = do
    currentDepth <- getCurrentDepth <$> ST.get
    ST.modify $ \st -> st {getCurrentDepth = currentDepth - 1}
    result <- r
    ST.modify $ \st -> st {getCurrentDepth = currentDepth}
    return result

-- | Select a random value from the provided list of random selectors, keeping
--   track of the depth of recursion
selectFromRandomRecursive' ::
       (b -> RandomSelector a) -- ^ Getter of a selector
    -> ([b] -> RandomSelector Int) -- ^ Getter of a random position
    -> [b] -- ^ Non-recursive alternatives
    -> [b] -- ^ Recursive alternatives
    -> RandomSelector a
selectFromRandomRecursive' getRandom getPos nonRec rec = do
    currentDepth <- getCurrentDepth <$> ST.get
    let alts =
            if currentDepth <= 0
                then nonRec
                else nonRec ++ rec
    pos <- getPos alts
    let selected = getRandom $ alts !! pos
    -- If we selected non-recursive alternative, don't modify depth
    -- Otherwise, decrease depth, select 1 alternative and set the depth back
    if pos < length nonRec
        then selected
        else withDecreasedDepth selected

-- | Select a random value from the provided list of random selectors, keeping
--   track of the depth of recursion
selectFromRandomRecursive ::
       [RandomSelector a] -- ^ Non-recursive alternatives
    -> [RandomSelector a] -- ^ Recursive alternatives
    -> RandomSelector a
selectFromRandomRecursive =
    selectFromRandomRecursive' id (randomNumber . length)

-- | Select a random value from the provided list of random selectors, where
--   each selector have custom probability of being selected. Keeps
--   track of the depth of recursion
selectFromRandomRecursiveWeighted ::
       [(RandomSelector a, Int)] -- ^ Non-recursive alternatives
    -> [(RandomSelector a, Int)] -- ^ Recursive alternatives
    -> RandomSelector a
selectFromRandomRecursiveWeighted =
    selectFromRandomRecursive' fst (randomNumberWeighted . map snd)
