{- |
Module      :  Compiler.Output
Description :  Output of the compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Output of the DFL compiler
-}
module Compiler.Output where

import Frontend.Inference.Processor (Signatures)
import Frontend.Syntax.Processor (InfixOperators)

-- | Result of compilation of a source file
data Output = Output
    { getInfixOperators :: InfixOperators -- ^ Get map of infix operators
    , getInferredKinds :: Signatures -- ^ Get inferred kinds
    } deriving (Eq, Show)
