{- |
Module      :  Frontend.Desugaring.Final.Util
Description :  Utility functions for desugaring
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for desugaring
-}
module Frontend.Desugaring.Final.Util where

import Frontend.Desugaring.Final.Ast
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

-- | Desugar a constraint
desugarConstraint :: WithLocation I.Constraint -> WithLocation Constraint
desugarConstraint c
    | (I.Constraint name params paramArgs) <- getValue c =
        Constraint name params paramArgs <$ c
