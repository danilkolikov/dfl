{- |
Module      :  Frontend.Inference.Kind.Environment
Description :  Environment of kind inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Environment of kind inference
-}
module Frontend.Inference.Kind.Environment where

import Frontend.Desugaring.Final.Ast

-- | Environment of kind inference
data Environment = Environment
    { getTypeSynonyms :: TypeSynonyms -- ^ Defined type synonyms
    , getDataTypes :: DataTypes -- ^ Defined data types
    , getClasses :: Classes -- ^ Defined classes
    }
