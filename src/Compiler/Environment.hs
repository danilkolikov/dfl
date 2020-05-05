{- |
Module      :  Compiler.Environment
Description :  Environment of the DFL compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Environment of the DFL compiler
-}
module Compiler.Environment where

-- | Environment of compilation
data Environment
 = Environment
    { getMainSourceFile :: String -- ^ Path to the source file
    , isDebugOutputEnabled :: Bool  -- ^ Should print debug output?
    }
