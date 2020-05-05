{- |
Module      :  Frontend.Module.Import.Selecting
Description :  Selection of imports
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for selecting objects to import
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Frontend.Module.Import.Selecting
    ( selectExplicitImports
    , ExplicitProcessorError(..)
    ) where

import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Module.Base
import Frontend.Module.Explicit
import Frontend.Syntax.Position

-- | Selects explicit imports from the list of exports of a module
selectExplicitImports ::
       Explicit
    -> Bool
    -> I.ImpExpList (WithLocation I.Import)
    -> Either ExplicitProcessorError Explicit
selectExplicitImports exports isHiding list = do
    selected <- runExplicitProcessor (selectExplicit list) exports
    return $
        if isHiding
            then subtractExplicit exports selected
            else selected

instance SelectsExplicit I.Import where
    selectExplicit import' =
        case import' of
            I.ImportFunction name -> processFunction $ wrapIdent name
            I.ImportDataOrClass name components ->
                processDataOrClass (wrapIdent name) (fmap wrapIdent components)
      where
        wrapIdent = fmap IdentUserDefined

subtractExplicit :: Explicit -> Explicit -> Explicit
subtractExplicit allExports hiddenExports
    | Explicit { getExplicitTypeSynonyms = allTypeSynonyms
               , getExplicitDataTypes = allDataTypes
               , getExplicitClasses = allClasses
               , getExplicitExpressions = allExpressions
               } <- allExports
    , Explicit { getExplicitTypeSynonyms = hiddenSynonyms
               , getExplicitDataTypes = hiddenDataTypes
               , getExplicitClasses = hiddenClasses
               , getExplicitExpressions = hiddenExpressions
               } <- hiddenExports =
        Explicit
            { getExplicitTypeSynonyms =
                  HM.difference allTypeSynonyms hiddenSynonyms
            -- TODO: It's not an accurate subtraction
            , getExplicitDataTypes = HM.difference allDataTypes hiddenDataTypes
            , getExplicitClasses = HM.difference allClasses hiddenClasses
            , getExplicitExpressions =
                  HM.difference allExpressions hiddenExpressions
            }
