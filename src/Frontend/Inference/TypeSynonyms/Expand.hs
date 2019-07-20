{- |
Module      :  Frontend.Inference.TypeSynonyms.Expand
Description :  Functions for expanding of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for expanding of type synonyms
-}
module Frontend.Inference.TypeSynonyms.Expand
    ( expandTypeSynonyms
    , TypeSynonymsExpandingError(..)
    ) where

import Control.Monad (liftM2, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, runExcept, throwE)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Variables
import Frontend.Syntax.Position

-- | Errors which can be encountered during expansions of type synonyms
data TypeSynonymsExpandingError =
    TypeSynonymsExpandingErrorwWrongNumberOfArgs (WithLocation Ident)
                                                 Int
                                                 Int -- ^ Wrong number of arguments
    deriving (Eq, Show)

-- | A type of objects which expand type synonyms
type TypeSynonymExpander
     = ReaderT (HM.HashMap Ident TypeSignature) (Except TypeSynonymsExpandingError)

-- | Expands type synonyms in a type
expandTypeSynonyms ::
       WithLocation F.Type
    -> HM.HashMap Ident TypeSignature
    -> Either TypeSynonymsExpandingError Type
expandTypeSynonyms type' = runExcept . runReaderT (expandType type')

-- | Expands type synonyms in a type
expandType :: WithLocation F.Type -> TypeSynonymExpander Type
expandType type' =
    case getValue type' of
        F.TypeVar name -> return $ TypeVar (getValue name)
        F.TypeConstr name -> return $ TypeConstr (getValue name)
        F.TypeFunction from to ->
            liftM2 TypeFunction (expandType from) (expandType to)
        F.TypeApplication func args ->
            case getValue func of
                F.TypeConstr name -> expandApplication name args
                _ ->
                    liftM2
                        TypeApplication
                        (expandType func)
                        (mapM expandType args)

-- | Expands type synonyms in an application
expandApplication ::
       WithLocation F.Ident
    -> NE.NonEmpty (WithLocation F.Type)
    -> TypeSynonymExpander Type
expandApplication ident@(WithLocation name _) args = do
    typeSynonyms <- ask
    processedArgs <- mapM expandType args
    case HM.lookup name typeSynonyms of
        Just TypeSignature { getTypeSignatureTypeParams = typeParams
                           , getTypeSignatureType = type'
                           } -> do
            when (length processedArgs < length typeParams) . lift . throwE $
                TypeSynonymsExpandingErrorwWrongNumberOfArgs
                    ident
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
