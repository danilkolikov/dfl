{- |
Module      :  Frontend.Desugaring.Util.IdentGenerator
Description :  Generator of new identifiers
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Generates new unique identifiers
-}
module Frontend.Desugaring.Util.IdentGenerator where

import Control.Monad.Trans.State as ST (State, get, put, evalState)

import Frontend.Syntax.Token (VarId(..))

-- | Type of generator of identifiers
type IdentGenerator a = ST.State Int a

-- | Run IdentGenerator
runIdentGenerator :: IdentGenerator a -> Int -> a
runIdentGenerator = evalState

-- | Function for generation of a next number
nextNumber :: IdentGenerator Int
nextNumber = do
    cur <- ST.get
    ST.put (cur + 1)
    return cur

-- | Prefix of generated identifiers
gENERATED_ID_PREFIX :: String
gENERATED_ID_PREFIX = "$gen$"

-- | Function for generation of new VarId
newVarId :: IdentGenerator VarId
newVarId = VarId . (gENERATED_ID_PREFIX ++) . show <$> nextNumber
