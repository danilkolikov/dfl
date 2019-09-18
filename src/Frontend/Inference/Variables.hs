{- |
Module      :  Frontend.Inference.Variables
Description :  Generation of variables
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with functions for generation of variables
-}
module Frontend.Inference.Variables
    ( VariableGenerator
    , evalVariableGenerator
    , runVariableGenerator
    , VariableGeneratorT
    , evalVariableGeneratorT
    , runVariableGeneratorT
    , VariableGeneratorState(..)
    , emptyVariableGeneratorState
    , Sort(..)
    , generateSortVariable
    , Kind(..)
    , generateKindVariable
    , Type(..)
    , generateTypeVariable
    , generateKSVariables
    , generateTKSVariables
    , Ident(..)
    , IdentEnvironment(..)
    , generateSortIdent
    , generateKindIdent
    , generateTypeIdent
    ) where

import Control.Monad.Trans.State.Lazy
    ( State
    , StateT
    , evalState
    , evalStateT
    , gets
    , modify
    , runState
    , runStateT
    )

import Frontend.Desugaring.Final.Ast (Ident(..), IdentEnvironment(..))
import Frontend.Inference.Kind
import Frontend.Inference.Sort
import Frontend.Inference.Type

-- | An object representing the state of the variable generator
data VariableGeneratorState = VariableGeneratorState
    { getVariableGeneratorStateSort, getVariableGeneratorStateKind, getVariableGeneratorStateType :: Int
    }

-- | An empty state of the variable generator
emptyVariableGeneratorState :: VariableGeneratorState
emptyVariableGeneratorState =
    VariableGeneratorState
        { getVariableGeneratorStateSort = 0
        , getVariableGeneratorStateKind = 0
        , getVariableGeneratorStateType = 0
        }

-- | A type of the variable generator
type VariableGenerator = State VariableGeneratorState

-- | Execute variable generator with an empty state
evalVariableGenerator :: VariableGenerator a -> a
evalVariableGenerator gen = evalState gen emptyVariableGeneratorState

-- | Runs the variable generator
runVariableGenerator ::
       VariableGenerator a
    -> VariableGeneratorState
    -> (a, VariableGeneratorState)
runVariableGenerator = runState

-- | A type of the variable generator with inner monad
type VariableGeneratorT m = StateT VariableGeneratorState m

-- | Execute variable generator with an empty state
evalVariableGeneratorT :: (Monad m) => VariableGeneratorT m a -> m a
evalVariableGeneratorT gen = evalStateT gen emptyVariableGeneratorState

-- | Runs the variable generator
runVariableGeneratorT ::
       (Monad m)
    => VariableGeneratorT m a
    -> VariableGeneratorState
    -> m (a, VariableGeneratorState)
runVariableGeneratorT = runStateT

-- | Generate abstract identifier
generateIdent ::
       (Monad m)
    => (VariableGeneratorState -> Int)
    -> (Int -> VariableGeneratorState -> VariableGeneratorState)
    -> IdentEnvironment
    -> VariableGeneratorT m Ident
generateIdent getCounter setCounter env = do
    counter <- gets getCounter
    modify $ setCounter (counter + 1)
    return $ IdentGenerated env counter

-- | Generate a sort variable
generateSortVariable :: (Monad m) => VariableGeneratorT m Sort
generateSortVariable = SortVar <$> generateSortIdent

-- | Generate a new sort identifier
generateSortIdent :: (Monad m) => VariableGeneratorT m Ident
generateSortIdent =
    generateIdent
        getVariableGeneratorStateSort
        (\n s -> s {getVariableGeneratorStateSort = n})
        IdentEnvironmentSortVariable

-- | Generate a kind variable
generateKindVariable :: (Monad m) => VariableGeneratorT m Kind
generateKindVariable = KindVar <$> generateKindIdent

-- | Generate a kind identifier
generateKindIdent :: (Monad m) => VariableGeneratorT m Ident
generateKindIdent =
    generateIdent
        getVariableGeneratorStateKind
        (\n s -> s {getVariableGeneratorStateKind = n})
        IdentEnvironmentKindVariable

-- | Generate a type variable
generateTypeVariable :: (Monad m) => VariableGeneratorT m Type
generateTypeVariable = TypeVar <$> generateTypeIdent

-- | Generate a type identifier
generateTypeIdent :: (Monad m) => VariableGeneratorT m Ident
generateTypeIdent =
    generateIdent
        getVariableGeneratorStateType
        (\n s -> s {getVariableGeneratorStateType = n})
        IdentEnvironmentTypeVariable

-- | Generates kind and sort variables
generateKSVariables :: (Monad m) => VariableGeneratorT m (Kind, Sort)
generateKSVariables = do
    sortVar <- generateSortVariable
    kindVar <- generateKindVariable
    return (kindVar, sortVar)

-- | Generates type, kind and sort variables
generateTKSVariables :: (Monad m) => VariableGeneratorT m (Type, Kind, Sort)
generateTKSVariables = do
    (kindVar, sortVar) <- generateKSVariables
    typeVar <- generateTypeVariable
    return (typeVar, kindVar, sortVar)
