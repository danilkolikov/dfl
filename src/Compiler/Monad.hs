{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
Module      :  Compiler.Monad
Description :  Monad for execution of a compiler
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Definitions of a monad which executes the compiler
-}
module Compiler.Monad where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)

import Compiler.Base (Compiler(..), getSourceFileName)
import Compiler.DebugOutput (DebugOutput(..), DebugOutputType(..))
import Compiler.Environment (Environment)
import Compiler.Prettify.CompilationError (prettifyCompilationError)
import Compiler.Prettify.Output (prettifyOutput)

-- | Monad, encapsulation compilation of a single source file
newtype CompilerMonad a = CompilerMonad
    { runCompilerMonad :: MaybeT (ReaderT Environment IO) a -- ^ Extract value
    } deriving (Functor, Applicative, Monad)

-- | Run compiler
runCompiler :: CompilerMonad a -> Environment -> IO (Maybe a)
runCompiler (CompilerMonad m) = runReaderT (runMaybeT m)

instance MonadIO CompilerMonad where
    liftIO = CompilerMonad . lift . lift

instance Compiler CompilerMonad where
    getEnvironmentComponent = CompilerMonad . lift . asks
    readFileContent = liftIO . readFile
    handleResult res =
        case res of
            Left e -> do
                liftIO . putStrLn . prettifyCompilationError $ e
                fail "An error was encountered"
            Right r -> return r
    writeDebugOutput DebugOutput { getDebugOutputType = type'
                                 , getDebugOutputValue = value
                                 } = do
        fileName <- getOutputFileName (debugOutputTypeToSuffix type')
        liftIO $ writeFile fileName value
    writeOutput output = do
        fileName <- getOutputFileName "out"
        liftIO $ writeFile fileName (prettifyOutput output)

-- | Get name of an output file
getOutputFileName :: (Compiler m) => String -> m String
getOutputFileName suffix = (++ '.' : suffix) <$> getSourceFileName

-- | Get appropriate suffix for a debug output type
debugOutputTypeToSuffix :: DebugOutputType -> String
debugOutputTypeToSuffix type' =
    case type' of
        DebugOutputTypeLexems -> "lexems"
        DebugOutputTypeAst -> "ast"
        DebugOutputTypeDesugaredAst -> "desugared"
        DebugOutputTypeFixityResolution -> "fixity"
        DebugOutputTypeInference -> "inference"
