{- |
Module      :  Frontend.Inference.Variables
Description :  Generation of variables
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with functions for generation of variables
-}
module Frontend.Inference.Variables
    ( VariableGenerator
    , VariableGeneratorState(..)
    , emptyVariableGeneratorState
    , evalVariableGenerator
    , runVariableGenerator
    , Sort(..)
    , generateSortVariable
    , Kind(..)
    , generateKindVariable
    , Type(..)
    , generateTypeVariable
    , Ident(..)
    , IdentEnvironment(..)
    , generateSortIdent
    , generateKindIdent
    , generateTypeIdent
    ) where

import Control.Monad.Trans.State.Lazy (State, evalState, gets, modify, runState)

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

-- | Generate abstract identifier
generateIdent ::
       (VariableGeneratorState -> Int)
    -> (Int -> VariableGeneratorState -> VariableGeneratorState)
    -> IdentEnvironment
    -> VariableGenerator Ident
generateIdent getCounter setCounter env = do
    counter <- gets getCounter
    modify $ setCounter (counter + 1)
    return $ IdentGenerated env counter

-- | Generate a sort variable
generateSortVariable :: VariableGenerator Sort
generateSortVariable = SortVar <$> generateSortIdent

-- | Generate a new sort identifier
generateSortIdent :: VariableGenerator Ident
generateSortIdent =
    generateIdent
        getVariableGeneratorStateSort
        (\n s -> s {getVariableGeneratorStateSort = n})
        IdentEnvironmentSortVariable

-- | Generate a kind variable
generateKindVariable :: VariableGenerator Kind
generateKindVariable = KindVar <$> generateKindIdent

-- | Generate a kind identifier
generateKindIdent :: VariableGenerator Ident
generateKindIdent =
    generateIdent
        getVariableGeneratorStateKind
        (\n s -> s {getVariableGeneratorStateKind = n})
        IdentEnvironmentKindVariable

-- | Generate a type variable
generateTypeVariable :: VariableGenerator Type
generateTypeVariable = TypeVar <$> generateTypeIdent

-- | Generate a type identifier
generateTypeIdent :: VariableGenerator Ident
generateTypeIdent =
    generateIdent
        getVariableGeneratorStateType
        (\n s -> s {getVariableGeneratorStateType = n})
        IdentEnvironmentTypeVariable
