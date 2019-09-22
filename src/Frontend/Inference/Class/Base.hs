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
import Frontend.Inference.Class
import Frontend.Inference.DependencyResolver
import qualified Frontend.Inference.Kind.Ast as K
import Frontend.Inference.Signature
import Frontend.Inference.Util.Debug
import Frontend.Syntax.Position

-- | A type of the class processor
type ClassProcessor
     = WithDebugOutput ClassProcessorError ClassProcessorDebugOutput

-- | Errors which can be encountered during processing of type classes
data ClassProcessorError
    = ClassProcessorErrorRecursive Ident -- ^ Class extends itself
    | ClassProcessorErrorMutuallyRecursive [Ident] -- ^ Classes mutually extend themselves
    | ClassProcessorErrorDependencyResolution DependencyResolverError -- ^ Dependency resolution error
    | ClassProcessorErrorUnknownClass (WithLocation Ident) -- ^ Unknown class
    | ClassProcessorErrorUnknownGeneratedDataType Ident -- ^ Unknown generated data type
    deriving (Eq, Show)

-- | A type of debug output of class processing
data ClassProcessorDebugOutput = ClassProcessorDebugOutput
    { getClassProcessorDebugOutputDependencyGraph :: Maybe DependencyGraph -- ^ Dependency graph
    , getClassProcessorDebugOutputDependencyGroups :: Maybe [HS.HashSet Ident] -- ^ Dependency groups
    , getClassProcessorDebugOutputOutputs :: Maybe [ClassProcessorOutput] -- ^ Outputs of processing of each class
    } deriving (Eq, Show)

instance Semigroup ClassProcessorDebugOutput where
    ClassProcessorDebugOutput g1 d1 o1 <> ClassProcessorDebugOutput g2 d2 o2 =
        ClassProcessorDebugOutput (g1 <|> g2) (d1 <|> d2) (o1 <> o2)

instance Monoid ClassProcessorDebugOutput where
    mempty = ClassProcessorDebugOutput mempty mempty mempty

-- | A type of output of class processsing
data ClassProcessorOutput = ClassProcessorOutput
    { getClassProcessorOutputClasses :: HM.HashMap Ident Class -- ^ Defined classes
    , getClassProcessorOutputDataTypes :: HM.HashMap Ident K.DataType -- ^ Generated data types
    , getClassProcessorOutputSignatures :: HM.HashMap Ident TypeConstructorSignature -- ^ Signatures of generated data types
    , getClassProcessorOutputDefaultInstances :: HM.HashMap Ident DefaultInstance -- ^ Default instances of classes
    , getClassProcessorOutputMethods :: HM.HashMap Ident TypeSignature -- ^ Signatures of methods
    , getClassProcessorOutputGetters :: HM.HashMap Ident K.Expression -- ^ Getters of class' components
    } deriving (Eq, Show)

instance Semigroup ClassProcessorOutput where
    ClassProcessorOutput c1 d1 sig1 i1 m1 g1 <> ClassProcessorOutput c2 d2 sig2 i2 m2 g2 =
        ClassProcessorOutput
            (c1 <> c2)
            (d1 <> d2)
            (sig1 <> sig2)
            (i1 <> i2)
            (m1 <> m2)
            (g1 <> g2)

instance Monoid ClassProcessorOutput where
    mempty = ClassProcessorOutput mempty mempty mempty mempty mempty mempty
