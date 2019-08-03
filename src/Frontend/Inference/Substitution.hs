{- |
Module      :  Frontend.Inference.Substitution
Description :  Class for objects which support substitution
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Class for objects which support substitution
-}
module Frontend.Inference.Substitution where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast (Ident)

-- | Mapping of variable names to objects
type Substitution a = HM.HashMap Ident a

-- | Class for types which support substitution of values in place of variables
class Substitutable a where
    -- | Substitute variables in the object
    substitute :: Substitution a -> a -> a

-- | Compose two substitution mapping. Resulting mapping contains keys
-- | from the first one, where values are substituted using the second mapping,
-- | and also includes all keys, missing in the first mapping
compose ::
       (Substitutable a) => Substitution a -> Substitution a -> Substitution a
compose s1 s2 =
    let substitutedS1 = HM.map (substitute s2) s1
        missingS1 = HM.difference s2 s1
     in substitutedS1 `HM.union` missingS1
