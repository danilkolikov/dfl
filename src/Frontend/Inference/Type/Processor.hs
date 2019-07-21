{- |
Module      :  Frontend.Inference.Type.Processor
Description :  Functions for type inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Processor of type inference
-}
module Frontend.Inference.Type.Processor where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (Except, except, runExcept)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State.Lazy (StateT, execStateT, modify)
import Data.Bifunctor (first)
import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Kind.ProcessorBase (Signatures)
import Frontend.Inference.Signature
import Frontend.Inference.Type.Classes
import Frontend.Inference.Type.DataTypes
import Frontend.Inference.Type.Signatures
import Frontend.Inference.TypeSynonyms.Processor (TypeSynonymSignatures)
import Frontend.Inference.Variables

-- | A type of errors which may be encountered during type inference
data TypeInferenceError =
    TypeInferenceErrorSignature TypeSignatureInferrenceError -- ^ An error of type signature inference
    deriving (Eq, Show)

-- | A state of the type inference processor
data TypeSignatures = TypeSignatures
    { getTypeSignaturesConstructors :: HM.HashMap Ident TypeSignature
    , getTypeSignaturesMethods :: HM.HashMap Ident TypeSignature
    , getTypeSignaturesFunctions :: HM.HashMap Ident TypeSignature
    } deriving (Eq, Show)

-- | An empty type signature
emptyTypeSignatures :: TypeSignatures
emptyTypeSignatures =
    TypeSignatures
        { getTypeSignaturesConstructors = HM.empty
        , getTypeSignaturesMethods = HM.empty
        , getTypeSignaturesFunctions = HM.empty
        }

-- | An environment of type inference
data LocalEnvironment = LocalEnvironment
    { getSignatures :: Signatures
    , getTypeSynonyms :: TypeSynonymSignatures
    }

-- | A type of the processor of type inference
type Processor
     = ReaderT LocalEnvironment (StateT TypeSignatures (Except TypeInferenceError))

-- | Infer types of functions in the module
inferTypes ::
       F.Module
    -> Signatures
    -> TypeSynonymSignatures
    -> TypeSignatures
    -> Either TypeInferenceError TypeSignatures
inferTypes module' signatures typeSynonymSignatures initialState =
    let environment =
            LocalEnvironment
                { getSignatures = signatures
                , getTypeSynonyms = typeSynonymSignatures
                }
        reader = inferTypes' module'
        state = runReaderT reader environment
        exc = execStateT state initialState
     in runExcept exc

-- | Infer types of functions in the module
inferTypes' :: F.Module -> Processor ()
inferTypes' F.Module { F.getModuleDataTypes = dataTypes
                     , F.getModuleClasses = classes
                     } = do
    LocalEnvironment { getSignatures = signatures
                     , getTypeSynonyms = typeSynonymSignatures
                     } <- ask
    let constructors =
            HM.unions . map createConstructorSignatures . HM.elems $ dataTypes
        methods = HM.unions . map createClassSignatures . HM.elems $ classes
        inferSignature (name, sig) = do
            res <-
                wrapError TypeInferenceErrorSignature $
                inferTypeSignature signatures typeSynonymSignatures sig
            return (name, res)
        inferSignatures = (HM.fromList <$>) . mapM inferSignature . HM.toList
    constructorSignatures <- inferSignatures constructors
    methodsSignatures <- inferSignatures methods
    lift . modify $ \s ->
        s
            { getTypeSignaturesConstructors = constructorSignatures
            , getTypeSignaturesMethods = methodsSignatures
            }

-- | Wrap an error, encountered during type inference
wrapError :: (a -> TypeInferenceError) -> Either a b -> Processor b
wrapError f = lift . lift . except . first f
