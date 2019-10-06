{- |
Module      :  Frontend.Desugaring.Grouping.Util
Description :  Utility functions for grouping
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Utility functions for the grouping
-}
module Frontend.Desugaring.Grouping.Util where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE

import Core.Ident
import Frontend.Desugaring.Grouping.Ast
import qualified Frontend.Desugaring.Initial.Ast as I
import Frontend.Syntax.Position (WithLocation(..))

wrapIdent :: WithLocation UserDefinedIdent -> WithLocation Ident
wrapIdent = (IdentUserDefined <$>)

wrapType :: WithLocation I.Type -> WithLocation Type
wrapType t =
    t $>
    case getValue t of
        I.TypeVar name -> TypeVar (wrapIdent name)
        I.TypeConstr name -> TypeConstr (wrapIdent name)
        I.TypeFunction from to -> TypeFunction (wrapType from) (wrapType to)
        I.TypeApplication func args ->
            TypeApplication (wrapType func) (fmap wrapType args)

-- | Desugar a constraint
wrapConstraint :: WithLocation I.Constraint -> WithLocation Constraint
wrapConstraint c
    | (I.Constraint name param paramArgs) <- getValue c =
        c $>
        case map wrapType paramArgs of
            [] -> ConstraintParam (wrapIdent name) (wrapIdent param)
            (s:rest) ->
                ConstraintAppliedParam
                    (wrapIdent name)
                    (wrapIdent param)
                    (s NE.:| rest)

-- | Desugar a simple class
wrapSimpleClass :: WithLocation I.SimpleClass -> WithLocation SimpleConstraint
wrapSimpleClass c
    | (I.SimpleClass name param) <- getValue c =
        c $> SimpleConstraint (wrapIdent name) (wrapIdent param)

wrapPattern :: WithLocation I.Pattern -> WithLocation Pattern
wrapPattern pat =
    pat $>
    case getValue pat of
        I.PatternInfix iPat -> PatternInfix (wrapInfixPattern iPat)
        I.PatternConstr name patterns ->
            PatternConstr (wrapIdent name) (map wrapPattern patterns)
        I.PatternRecord name bindings ->
            PatternRecord (wrapIdent name) (map wrapPatternBinding bindings)
        I.PatternVar name maybePat ->
            PatternVar (wrapIdent name) (wrapPattern <$> maybePat)
        I.PatternConst c -> PatternConst c
        I.PatternWildcard -> PatternWildcard

wrapInfixPattern :: WithLocation I.InfixPattern -> WithLocation InfixPattern
wrapInfixPattern iPat =
    iPat $>
    case getValue iPat of
        I.InfixPatternApplication l op r ->
            InfixPatternApplication
                (wrapInfixPattern l)
                (wrapIdent op)
                (wrapInfixPattern r)
        I.InfixPatternSimple inner -> InfixPatternSimple (wrapPattern inner)

wrapPatternBinding ::
       WithLocation I.PatternBinding -> WithLocation PatternBinding
wrapPatternBinding binding =
    binding $>
    case getValue binding of
        I.PatternBinding name pat ->
            PatternBinding (wrapIdent name) (wrapPattern pat)

wrapExport :: WithLocation I.Export -> WithLocation Export
wrapExport export =
    export $>
    case getValue export of
        I.ExportFunction name -> ExportFunction (wrapIdent name)
        I.ExportDataOrClass name list ->
            ExportDataOrClass (wrapIdent name) (fmap wrapIdent list)
        I.ExportModule name -> ExportModule (wrapIdent name)
