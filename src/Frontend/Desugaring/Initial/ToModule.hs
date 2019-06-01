{- |
Module      :  Frontend.Desugaring.Initial.ToModule
Description :  Desugaring of AST nodes to Module
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Module-s.
-}
module Frontend.Desugaring.Initial.ToModule
    ( DesugarToModule(..)
    ) where

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE (NonEmpty(..))

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.ToIdent (desugarToIdent)
import Frontend.Desugaring.Initial.ToTopDecl (desugarToTopDecl)
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Class for types which can be desugared to Module
class DesugarToModule a where
    desugarToModule :: a -> WithLocation D.Module -- ^ Desugar object to a Module

instance (DesugarToModule a) => DesugarToModule (WithLocation a) where
    desugarToModule = (getValue . desugarToModule <$>)

instance DesugarToModule Module where
    desugarToModule (ModuleExplicit name exports body) =
        withDummyLocation $
        bodyToDecls
            (D.Module (desugarToIdent name) (desugarExports exports))
            (getValue body)
    desugarToModule (ModuleImplicit body) =
        withDummyLocation $
        bodyToDecls
            (D.Module
                 (withDummyLocation $ D.IdentNamed dEFAULT_MODULE_NAME)
                 D.ImpExpAll)
            (getValue body)

-- Helper functions
bodyToDecls ::
       ([WithLocation D.ImpDecl] -> [WithLocation D.TopDecl] -> a) -> Body -> a
bodyToDecls wrap (Body impDecls topDecls) =
    wrap (map desugarImpDecl impDecls) (concatMap desugarToTopDecl topDecls)

desugarExport :: WithLocation Export -> WithLocation D.Export
desugarExport export =
    (case getValue export of
         ExportFunction qVar -> D.ExportFunction (desugarToIdent qVar)
         ExportDataOrClass name funcs ->
             D.ExportDataOrClass
                 (desugarToIdent name)
                 (desugarToImpExp $ getValue funcs)
         ExportModule name -> D.ExportModule (desugarToIdent name)) <$
    export

desugarExports ::
       Maybe [WithLocation Export] -> D.ImpExpList (WithLocation D.Export)
desugarExports Nothing = D.ImpExpAll
desugarExports (Just []) = D.ImpExpNothing
desugarExports (Just (f:rest)) =
    D.ImpExpSome (fmap desugarExport (f NE.:| rest))

desugarImpDecl :: WithLocation ImpDecl -> WithLocation D.ImpDecl
desugarImpDecl impDecl =
    impDecl $>
    (case getValue impDecl of
         ImpDecl qual name as impSpec ->
             let (hiding, imports) =
                     case impSpec of
                         Nothing -> (False, D.ImpExpAll)
                         Just (WithLocation (ImpSpec flag imps) _) ->
                             ( flag
                             , case imps of
                                   [] -> D.ImpExpNothing
                                   f:rest ->
                                       D.ImpExpSome
                                           (fmap desugarImport (f NE.:| rest)))
              in D.ImpDecl
                     qual
                     (desugarToIdent name)
                     (desugarToIdent <$> as)
                     hiding
                     imports)

desugarImport :: WithLocation Import -> WithLocation D.Import
desugarImport import' =
    import' $>
    (case getValue import' of
         ImportFunction name -> D.ImportFunction (desugarToIdent name)
         ImportDataOrClass name meths ->
             D.ImportDataOrClass
                 (desugarToIdent name)
                 (desugarToImpExp $ getValue meths))

desugarToImpExp :: ImpExpList -> D.ImpExpList (WithLocation D.Ident)
desugarToImpExp ImpExpNothing = D.ImpExpNothing
desugarToImpExp (ImpExpSome names) = D.ImpExpSome (fmap desugarToIdent names)
desugarToImpExp ImpExpAll = D.ImpExpAll
