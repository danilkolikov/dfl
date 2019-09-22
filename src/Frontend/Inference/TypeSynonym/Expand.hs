{- |
Module      :  Frontend.Inference.TypeSynonym.Expand
Description :  Functions for expanding of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for expanding of type synonyms
-}
module Frontend.Inference.TypeSynonym.Expand
    ( expandTypeSynonyms
    , TypeSynonymExpandingError(..)
    , TypeSynonymExpander
    , runTypeSynonymExpander
    , raiseError
    , expandType
    , expandConstraint
    ) where

import Control.Monad (liftM2, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Variables
import Frontend.Syntax.Position

-- | Errors which can be encountered during expansions of type synonyms
data TypeSynonymExpandingError
    = TypeSynonymExpandingErrorwWrongNumberOfArgs Ident
                                                   Int
                                                   Int -- ^ Wrong number of arguments
    | TypeSynonymExpandingErrorSynonymInInstance (WithLocation Ident)
                                                  (WithLocation Ident) -- ^ A type synonym is used in an instance
    deriving (Eq, Show)

-- | A type of objects which expand type synonyms
type TypeSynonymExpander
     = ReaderT (HM.HashMap Ident TypeSignature) (Except TypeSynonymExpandingError)

-- | Raises an error
raiseError :: TypeSynonymExpandingError -> TypeSynonymExpander a
raiseError = lift . throwE

-- | Run an expander of type synonyms
runTypeSynonymExpander ::
       HM.HashMap Ident TypeSignature
    -> TypeSynonymExpander a
    -> Either TypeSynonymExpandingError a
runTypeSynonymExpander signatures expander =
    runExcept $ runReaderT expander signatures

-- | Expands type synonyms in a type
expandTypeSynonyms ::
       HM.HashMap Ident TypeSignature
    -> Type
    -> Either TypeSynonymExpandingError Type
expandTypeSynonyms signatures type' =
    runTypeSynonymExpander signatures (expandType type')

-- | Expands type synonyms in a type
expandType :: Type -> TypeSynonymExpander Type
expandType type' =
    case type' of
        TypeVar name -> return $ TypeVar name
        TypeConstr name -> return $ TypeConstr name
        TypeFunction from to ->
            liftM2 TypeFunction (expandType from) (expandType to)
        TypeApplication func args ->
            case func of
                TypeConstr name -> expandApplication name args
                _ ->
                    liftM2
                        TypeApplication
                        (expandType func)
                        (mapM expandType args)

-- | Expands type synonyms in an application
expandApplication :: Ident -> NE.NonEmpty Type -> TypeSynonymExpander Type
expandApplication name args = do
    typeSynonyms <- ask
    processedArgs <- mapM expandType args
    case HM.lookup name typeSynonyms of
        Just TypeSignature { getTypeSignatureTypeParams = typeParams
                           , getTypeSignatureType = type'
                           } -> do
            when (length processedArgs < length typeParams) . raiseError $
                TypeSynonymExpandingErrorwWrongNumberOfArgs
                    name
                    (length typeParams)
                    (length processedArgs)
            let (toSubstitute, rest) =
                    splitAt (length typeParams) $ NE.toList processedArgs
                substitution =
                    HM.fromList $
                    zipWith
                        (\(paramName, _) argType -> (paramName, argType))
                        typeParams
                        toSubstitute
                substituted = substitute substitution type'
            return $
                case rest of
                    [] -> substituted
                    (f:rst) -> TypeApplication substituted (f NE.:| rst)
        Nothing -> return $ TypeApplication (TypeConstr name) processedArgs

-- | Expand types in a constraint
expandConstraint :: Constraint -> TypeSynonymExpander Constraint
expandConstraint constraint =
    case constraint of
        ConstraintVariable {} -> return constraint
        ConstraintAppliedVariable className type' typeArgs ->
            ConstraintAppliedVariable className type' <$>
            mapM expandType typeArgs
