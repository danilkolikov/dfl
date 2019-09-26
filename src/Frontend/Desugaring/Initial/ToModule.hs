{- |
Module      :  Frontend.Desugaring.Initial.ToModule
Description :  Desugaring of AST nodes to Module
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Module-s.
-}
module Frontend.Desugaring.Initial.ToModule
    ( desugarToModule
    , desugarExport
    , desugarImpDecl
    , desugarImport
    , desugarToImpExp
    , desugarExports
    , desugarImpSpec
    ) where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE (NonEmpty(..))

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.ToTopDecl (desugarToTopDecl)
import Frontend.Desugaring.Initial.Utils
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..))

-- | Desugar object to a Module
desugarToModule :: WithLocation (Module Body) -> WithLocation D.Module
desugarToModule md =
    md $>
    case getValue md of
        ModuleExplicit name exports body ->
            bodyToDecls
                (D.Module (desugarToIdent name) (desugarExports exports))
                (getValue body)
        ModuleImplicit body ->
            bodyToDecls
                (D.Module (makeIdent dEFAULT_MODULE_NAME) D.ImpExpAll)
                (getValue body)

-- Helper functions
bodyToDecls ::
       ([WithLocation D.ImpDecl] -> [WithLocation D.TopDecl] -> a) -> Body -> a
bodyToDecls wrap (Body impDecls topDecls) =
    wrap (map desugarImpDecl impDecls) (concatMap desugarToTopDecl topDecls)

-- | Desugar Export
desugarExport :: WithLocation Export -> WithLocation D.Export
desugarExport export =
    export $>
    case getValue export of
        ExportFunction qVar -> D.ExportFunction (desugarToIdent qVar)
        ExportDataOrClass name funcs ->
            D.ExportDataOrClass (desugarToIdent name) (desugarToImpExp funcs)
        ExportModule name -> D.ExportModule (desugarToIdent name)

-- | Desugar exports of a module
desugarExports ::
       Maybe [WithLocation Export] -> D.ImpExpList (WithLocation D.Export)
desugarExports Nothing = D.ImpExpAll
desugarExports (Just []) = D.ImpExpNothing
desugarExports (Just (f:rest)) =
    D.ImpExpSome (fmap desugarExport (f NE.:| rest))

-- | Desugar ImpDecl
desugarImpDecl :: WithLocation ImpDecl -> WithLocation D.ImpDecl
desugarImpDecl impDecl =
    impDecl $>
    case getValue impDecl of
        ImpDecl qual name as impSpec ->
            let (hiding, imports) = desugarImpSpec impSpec
             in D.ImpDecl
                    qual
                    (desugarToIdent name)
                    (desugarToIdent <$> as)
                    hiding
                    imports

-- | Desugar ImpSpec
desugarImpSpec ::
       Maybe (WithLocation ImpSpec)
    -> (Bool, D.ImpExpList (WithLocation D.Import))
desugarImpSpec impSpec =
    case impSpec of
        Nothing -> (False, D.ImpExpAll)
        Just (WithLocation (ImpSpec flag imps) _) ->
            ( flag
            , case imps of
                  [] -> D.ImpExpNothing
                  f:rest -> D.ImpExpSome (fmap desugarImport (f NE.:| rest)))

-- | Desugar Import
desugarImport :: WithLocation Import -> WithLocation D.Import
desugarImport import' =
    import' $>
    case getValue import' of
        ImportFunction name -> D.ImportFunction (desugarToIdent name)
        ImportDataOrClass name meths ->
            D.ImportDataOrClass (desugarToIdent name) (desugarToImpExp meths)

-- | Desugar ImpExp list
desugarToImpExp ::
       WithLocation ImpExpList -> D.ImpExpList (WithLocation D.Ident)
desugarToImpExp list
    | ImpExpNothing <- getValue list = D.ImpExpNothing
    | ImpExpSome names <- getValue list =
        D.ImpExpSome (fmap desugarToIdent names)
    | otherwise = D.ImpExpAll
