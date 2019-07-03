{- |
Module      :  Compiler.Prettify.Utils
Description :  Utility functions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for pretty printing
-}
module Compiler.Prettify.Utils where

import Data.Tuple (swap)

import Frontend.Syntax.Token
import qualified Data.HashMap.Lazy as HM

inverseMap :: HM.HashMap a b -> [(b, a)]
inverseMap = map swap . HM.toList

inversedSpecial :: [(Special, Char)]
inversedSpecial = inverseMap specialSymbols

inversedKeywords :: [(Keyword, String)]
inversedKeywords = inverseMap keywords

inversedOperators :: [(Operator, String)]
inversedOperators = inverseMap operators

prettifyHeader :: String -> String
prettifyHeader header = unlines [header, replicate (length header) '-']
