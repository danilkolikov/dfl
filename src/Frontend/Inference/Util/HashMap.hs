{- |
Module      :  Frontend.Inference.Util.HashMap
Description :  Helper functions for hashmaps
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Helper functions for hashmaps
-}
module Frontend.Inference.Util.HashMap where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast (Ident)

-- | Monadically maps values of a map
mapHashMapM ::
       (Monad m) => (a -> m b) -> HM.HashMap Ident a -> m (HM.HashMap Ident b)
mapHashMapM f hashMap =
    let applySecond (p, s) = (\x -> (p, x)) <$> f s
     in HM.fromList <$> mapM applySecond (HM.toList hashMap)
