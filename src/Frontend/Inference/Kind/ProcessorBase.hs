{- |
Module      :  Frontend.Inference.Kind.ProcessorBase
Description :  Shared code for kind inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Shared code (functions, data types) for kind inference
-}
module Frontend.Inference.Kind.ProcessorBase where

import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Kind.Ast
import Frontend.Inference.Kind.Kind
import qualified Frontend.Syntax.Position as P

-- | Environment of kind inference
data Environment = Environment
    { getTypeSynonyms :: F.TypeSynonyms -- ^ Defined type synonyms
    , getDataTypes :: F.DataTypes -- ^ Defined data types
    , getClasses :: F.Classes -- ^ Defined classes
    }

-- | Mapping from variables to variables with kinds
type IdentToKindMapping = HM.HashMap Ident (WithKind Ident)

-- | Mapping of variables of a data type
data DataTypeKindMapping = DataTypeKindMapping
    { getDataTypeKindMappingName :: WithKind Ident
    , getDataTypeKindMappingParams :: IdentToKindMapping
    }

-- | Mapping of variables of a type synonym
data TypeSynonymKindMapping = TypeSynonymKindMapping
    { getTypeSynonymKindMappingName :: WithKind Ident
    , getTypeSynonymKindMappingParams :: IdentToKindMapping
    }

-- | Mapping of variables of a class
data ClassKindMapping = ClassKindMapping
    { getClassKindMappingParam :: WithKind Ident
    , getClassKindMappingMethods :: HM.HashMap Ident ( F.Method
                                                     , IdentToKindMapping)
    }

-- | Mapping of variables of a dependency group
data KindMappings = KindMappings
    { getDataTypeKindMappings :: HM.HashMap Ident ( F.DataType
                                                  , DataTypeKindMapping)
    , getTypeSynonymKindMappings :: HM.HashMap Ident ( F.TypeSynonym
                                                     , TypeSynonymKindMapping)
    , getClassKindMappings :: HM.HashMap Ident (F.Class, ClassKindMapping)
    }

instance Semigroup KindMappings where
    (KindMappings d1 t1 c1) <> (KindMappings d2 t2 c2) =
        KindMappings (d1 <> d2) (t1 <> t2) (c1 <> c2)

instance Monoid KindMappings where
    mempty = KindMappings mempty mempty mempty

-- | Structure with inferred kinds of a dependency group
data KindInferenceState = KindInferenceState
    { getResolvedTypeSynonyms :: TypeSynonyms
    , getResolvedDataTypes :: DataTypes
    , getResolvedClasses :: Classes
    } deriving (Eq, Show)

instance Semigroup KindInferenceState where
    (KindInferenceState t1 d1 c1) <> (KindInferenceState t2 d2 c2) =
        KindInferenceState (t1 <> t2) (d1 <> d2) (c1 <> c2)

instance Monoid KindInferenceState where
    mempty = KindInferenceState mempty mempty mempty
    mappend = (<>)

-- | Empty kind inference state
emptyKindInferenceState :: KindInferenceState
emptyKindInferenceState = mempty

instance KindSubstitutable KindInferenceState where
    substituteKind sub KindInferenceState { getResolvedTypeSynonyms = typeSynonyms
                                          , getResolvedDataTypes = dataTypes
                                          , getResolvedClasses = classes
                                          } =
        KindInferenceState
            { getResolvedTypeSynonyms = HM.map (substituteKind sub) typeSynonyms
            , getResolvedDataTypes = HM.map (substituteKind sub) dataTypes
            , getResolvedClasses = HM.map (substituteKind sub) classes
            }

-- | Set kind to an object
setKind :: P.WithLocation a -> Kind -> WithKind a
setKind (P.WithLocation x loc) = WithKind x loc

-- | Create mapping from idents to kinds
createMapping :: [WithKind Ident] -> IdentToKindMapping
createMapping params =
    let prepareParam withKind = (getValue withKind, withKind)
     in HM.fromList $ map prepareParam params
