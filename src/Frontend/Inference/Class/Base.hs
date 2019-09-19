{- |
Module      :  Frontend.Inference.Class.Base
Description :  Base definitions for class processing.
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Base definitions for class processing.
-}
module Frontend.Inference.Class.Base where

import Control.Applicative ((<|>))
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Class.Ast
import Frontend.Inference.DependencyResolver
import qualified Frontend.Inference.Kind.Ast as K
import Frontend.Inference.Signature
import Frontend.Inference.Util.Debug
import Frontend.Syntax.Position

-- | A type of the class processor
type ClassProcessor = WithDebugOutput ClassProcessingError ClassDebugOutput

-- | Errors which can be encountered during processing of type classes
data ClassProcessingError
    = ClassProcessingErrorRecursive Ident -- ^ Class extends itself
    | ClassProcessingErrorMutuallyRecursive [Ident] -- ^ Classes mutually extend themselves
    | ClassProcessingErrorDependencyResolution DependencyResolverError -- ^ Dependency resolution error
    | ClassProcessingErrorUnknownClass (WithLocation Ident) -- ^ Unknown class
    | ClassProcessingErrorUnknownGeneratedDataType Ident -- ^ Unknown generated data type

-- | A type of debug output of class processing
data ClassDebugOutput = ClassDebugOutput
    { getClassDebugOutputDependencyGraph :: Maybe DependencyGraph -- ^ Dependency graph
    , getClassDebugOutputDependencyGroups :: Maybe [HS.HashSet Ident] -- ^ Dependency groups
    , getClassDebugOutputOutputs :: Maybe [ClassProcessorState] -- ^ Outputs of processing of each class
    }

instance Semigroup ClassDebugOutput where
    ClassDebugOutput g1 d1 o1 <> ClassDebugOutput g2 d2 o2 =
        ClassDebugOutput (g1 <|> g2) (d1 <|> d2) (o1 <> o2)

instance Monoid ClassDebugOutput where
    mempty = ClassDebugOutput mempty mempty mempty

-- | A type of output of class processsing
data ClassProcessorState = ClassProcessorState
    { getClassProcessorStateClasses :: HM.HashMap Ident Class -- ^ Defined classes
    , getClassProcessorStateDataTypes :: HM.HashMap Ident K.DataType -- ^ Generated data types
    , getClassProcessorStateSignatures :: HM.HashMap Ident TypeConstructorSignature -- ^ Signatures of generated data types
    , getClassProcessorStateDefaultInstances :: HM.HashMap Ident DefaultInstance -- ^ Default instances of classes
    , getClassProcessorStateMethods :: HM.HashMap Ident TypeSignature -- ^ Signatures of methods
    }

instance Semigroup ClassProcessorState where
    ClassProcessorState c1 d1 sig1 i1 m1 <> ClassProcessorState c2 d2 sig2 i2 m2 =
        ClassProcessorState
            (c1 <> c2)
            (d1 <> d2)
            (sig1 <> sig2)
            (i1 <> i2)
            (m1 <> m2)

instance Monoid ClassProcessorState where
    mempty = ClassProcessorState mempty mempty mempty mempty mempty
