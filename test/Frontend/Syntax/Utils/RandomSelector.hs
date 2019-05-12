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
    ) where

import qualified Control.Monad.Trans.State as ST (State, evalState, get, put)
import System.Random (StdGen, mkStdGen, randomR)

-- | Type of object which selects random value
type RandomSelector a = ST.State StdGen a

-- | Select a random value
evalRandomSelector ::
       RandomSelector a -- ^ Select of random values
    -> Int -- ^ Seed for initialisation of random generator
    -> a -- ^ Selected random value
evalRandomSelector r seed = ST.evalState r (mkStdGen seed)

-- | Select random value from the supplied list
selectRandom :: [a] -> RandomSelector a
selectRandom alts = do
    randomGen <- ST.get
    let (pos, nextGen) = randomR (0, length alts - 1) randomGen
        selected = alts !! pos
    ST.put nextGen
    return selected
