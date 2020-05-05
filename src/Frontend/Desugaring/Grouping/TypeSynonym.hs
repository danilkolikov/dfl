{- |
Module      :  Frontend.Desugaring.Grouping.TypeSynonym
Description :  Grouping of type synonyms
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Grouping of type synonyms
-}
module Frontend.Desugaring.Grouping.TypeSynonym
    ( groupTypeSynonyms
    , groupTypeSynonym
    ) where

import Frontend.Desugaring.Grouping.Ast
import Frontend.Desugaring.Grouping.Base
import Frontend.Desugaring.Grouping.Util
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Find type synonyms among the list of top declarations and group them
groupTypeSynonyms :: [WithLocation I.TopDecl] -> GroupingProcessor TypeSynonyms
groupTypeSynonyms = collectHashMap groupTypeSynonym

-- | Desugar a single top declaration to a type synonym, or return Nothing
groupTypeSynonym :: I.TopDecl -> GroupingProcessor (Maybe (Ident, TypeSynonym))
groupTypeSynonym (I.TopDeclType simpleType type') = do
    let (I.SimpleType name args) = getValue simpleType
        wrappedName = wrapIdent name
        wrappedArgs = map wrapIdent args
        wrappedType = wrapType type'
    defineTypeName wrappedName
    return $
        Just
            ( getValue wrappedName
            , TypeSynonym wrappedName wrappedArgs wrappedType)
groupTypeSynonym _ = return Nothing
