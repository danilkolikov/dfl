{- |
Module      :  Main
Description :  DFL compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module launches the DFL compiler.
-}
module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import System.Exit

import Compiler.Environment (Environment(..))
import Compiler.Monad (runCompiler)
import Compiler.Processor (compileSourceFile)

-- | Parse command line arguments
parseArguments :: IO Environment
parseArguments =
    let fileArg = argument str (metavar "FILE")
        debugArg =
            switch $ long "debug" <> short 'd' <> help "Generate debug output"
        argParser = liftA2 Environment fileArg debugArg
        description =
            fullDesc <> progDesc "Compile a source file" <>
            header "dfl - a compiler for the Differentiable Functional Language"
        options = info (argParser <**> helper) description
     in execParser options

-- | Launch the DFL compiler
main :: IO ()
main = do
    environment <- parseArguments
    result <- runCompiler compileSourceFile environment
    case result of
        Nothing -> exitFailure
        Just _ -> exitSuccess
