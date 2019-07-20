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
import Frontend.Inference.Signature

-- | An environment of kind inference
data Environment = Environment
    { getTypeSynonyms :: F.TypeSynonyms -- ^ Defined type synonyms
    , getDataTypes :: F.DataTypes -- ^ Defined data types
    , getClasses :: F.Classes -- ^ Defined classes
    }

-- | A single item of a dependency group
data DependencyGroupItem a
    = DependencyGroupItemTypeSynonym F.TypeSynonym
                                     a -- ^ A type synonym
    | DependencyGroupItemDataType F.DataType
                                  a -- ^ A data type
    | DependencyGroupItemClass F.Class
                               a -- ^ A class
    deriving (Eq, Show)

-- | A single item of a dependency group without any additional information
type DependencyGroupItemEmpty = DependencyGroupItem ()

-- | A single item of a dependency group with a signature
type DependencyGroupItemWithSignature
     = DependencyGroupItem TypeConstructorSignature

-- | Signatures of entities in a dependency group
data Signatures = Signatures
    { getTypeSynonymSignatures :: HM.HashMap F.Ident TypeConstructorSignature -- ^ Signatures of type synonyms
    , getDataTypeSignatures :: HM.HashMap F.Ident TypeConstructorSignature -- ^ Signatures of data types
    , getClassSignatures :: HM.HashMap F.Ident TypeConstructorSignature -- ^ Signatures of classes
    } deriving (Eq, Show)

instance Semigroup Signatures where
    (Signatures d1 t1 c1) <> (Signatures d2 t2 c2) =
        Signatures (d1 <> d2) (t1 <> t2) (c1 <> c2)

instance Monoid Signatures where
    mempty = Signatures mempty mempty mempty

-- | Empty signatures
emptySignatures :: Signatures
emptySignatures = mempty

instance SortSubstitutable Signatures where
    substituteSort sub (Signatures t d c) =
        Signatures
            (HM.map (substituteSort sub) t)
            (HM.map (substituteSort sub) d)
            (HM.map (substituteSort sub) c)

instance KindSubstitutable Signatures where
    substituteKind sub sorts (Signatures t d c) =
        Signatures
            (HM.map (substituteKind sub sorts) t)
            (HM.map (substituteKind sub sorts) d)
            (HM.map (substituteKind sub sorts) c)

-- | Equalities between kinds and sorts
data Equalities = Equalities
    { getKindEqualities :: [(Kind, Kind)] -- ^ A list of equalities between kinds
    , getSortEqualities :: [(Sort, Sort)] -- ^ A list of equalities between sorts
    , getHasSortEqualities :: [(Kind, Sort)] -- ^ A list of statements that a kind has a sort
    } deriving (Eq, Show)

instance Semigroup Equalities where
    Equalities k1 s1 m1 <> Equalities k2 s2 m2 =
        Equalities (k1 <> k2) (s1 <> s2) (m1 <> m2)

instance Monoid Equalities where
    mempty = Equalities mempty mempty mempty
