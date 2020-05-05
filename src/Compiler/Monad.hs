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
import System.Directory (doesFileExist)

import Compiler.Base
import Compiler.Environment
import Compiler.Prettify.Utils

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
    doesFileExist = liftIO . System.Directory.doesFileExist
    handleResult res =
        case res of
            Left e -> do
                liftIO . putStrLn . prettify $ e
                fail "An error was encountered"
            Right r -> return r
    writeToFile fileName output = do
        liftIO . writeFile fileName $ prettify output
