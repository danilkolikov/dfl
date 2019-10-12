{- |
Module      :  Util.HashMap
Description :  Helper functions for hashmaps
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Helper functions for hashmaps
-}
module Util.HashMap where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.Maybe (fromMaybe)

import Data.Hashable (Hashable)

-- | Monadically maps values of a map
mapHashMapM ::
       (Eq k, Hashable k, Monad m)
    => (v -> m a)
    -> HM.HashMap k v
    -> m (HM.HashMap k a)
mapHashMapM f hashMap =
    let applySecond (p, s) = (\x -> (p, x)) <$> f s
     in HM.fromList <$> mapM applySecond (HM.toList hashMap)

-- | Monadically maps values of a map with key
mapHashMapWithKeyM ::
       (Eq k, Hashable k, Monad m)
    => (k -> v -> m a)
    -> HM.HashMap k v
    -> m (HM.HashMap k a)
mapHashMapWithKeyM f hashMap =
    let applySecond (k, v) = (\x -> (k, x)) <$> f k v
     in HM.fromList <$> mapM applySecond (HM.toList hashMap)

-- | Finds a value or halts the program
lookupOrFail :: (Eq k, Hashable k) => k -> HM.HashMap k a -> a
lookupOrFail name =
    fromMaybe (error "Required value was not found in a map") . HM.lookup name

-- | Merges two maps and monoidically joins
deepMerge ::
       (Eq k, Hashable k, Semigroup a)
    => HM.HashMap k a
    -> HM.HashMap k a
    -> HM.HashMap k a
deepMerge = HM.unionWith (<>)

-- | Intersect keys of a map with a provided set
intersectKeys ::
       (Eq k, Hashable k) => HS.HashSet k -> HM.HashMap k a -> HM.HashMap k a
intersectKeys keys = HM.filterWithKey (\k _ -> k `HS.member` keys)
