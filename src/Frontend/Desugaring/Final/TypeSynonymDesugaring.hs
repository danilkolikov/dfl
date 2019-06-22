{- |
Module      :  Frontend.Desugaring.Final.TypeSynonymDesugaring
Description :  Final desugaring of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Final desugaring of type synonyms
-}
module Frontend.Desugaring.Final.TypeSynonymDesugaring
    ( desugarTypeSynonyms
    , desugarTypeSynonym
    ) where

import Data.Functor (($>))

import Frontend.Desugaring.Final.Ast
import Frontend.Desugaring.Final.Processor
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Find type synonyms among the list of top declarations and desugar them
desugarTypeSynonyms ::
       [WithLocation I.TopDecl] -> DesugaringProcessor TypeSynonyms
desugarTypeSynonyms = collectHashMap desugarTypeSynonym

-- | Desugar a single top declaration to a type synonym, or return Nothing
desugarTypeSynonym ::
       I.TopDecl -> DesugaringProcessor (Maybe (Ident, TypeSynonym))
desugarTypeSynonym (I.TopDeclType simpleType type') =
    let (I.SimpleType name args) = getValue simpleType
     in defineTypeName name $> Just (getValue name, TypeSynonym name args type')
desugarTypeSynonym _ = return Nothing
