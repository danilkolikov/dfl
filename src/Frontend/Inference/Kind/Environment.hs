{- |
Module      :  Frontend.Inference.Kind.Environment
Description :  Environment of kind inference
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Environment of kind inference
-}
module Frontend.Inference.Kind.Environment where

import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast

-- | A single item of the kind inference environment
data KindInferenceEnvironmentItem
    = KindInferenceEnvironmentItemTypeSynonym TypeSynonym
    | KindInferenceEnvironmentItemDataType DataType
    | KindInferenceEnvironmentItemClass (Class Exp)
    deriving (Eq, Show)

-- | Environment of kind inference
type KindInferenceEnvironment = HM.HashMap Ident KindInferenceEnvironmentItem

-- | Prepares a kind inferens environment
prepareEnvironment ::
       TypeSynonyms -> DataTypes -> Classes Exp -> KindInferenceEnvironment
prepareEnvironment typeSynonyms dataTypes classes =
    HM.unions
        [ HM.map KindInferenceEnvironmentItemTypeSynonym typeSynonyms
        , HM.map KindInferenceEnvironmentItemDataType dataTypes
        , HM.map KindInferenceEnvironmentItemClass classes
        ]
