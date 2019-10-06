{- |
Module      :  Frontend.Desugaring.Record.Util
Description :  Utility functions for desugaring
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for record desugaring
-}
module Frontend.Desugaring.Record.Util where

import Core.PredefinedIdents
import Frontend.Desugaring.Record.Ast
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Make arbitrary expression
makeExp :: UserDefinedIdent -> WithLocation Exp
makeExp = withDummyLocation . ExpVar . withDummyLocation . IdentUserDefined

-- | undefined
undefinedExp :: WithLocation Exp
undefinedExp = makeExp uNDEFINED

-- | Make an ident
makeIdent :: Int -> WithLocation Ident
makeIdent =
    withDummyLocation .
    IdentGenerated . GeneratedIdent GeneratedIdentEnvironmentRecordDesugaring

-- | Make a pattern
makePattern :: Int -> WithLocation Pattern
makePattern = withDummyLocation . (`PatternVar` Nothing) . makeIdent
