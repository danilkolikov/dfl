{- |
Module      :  Frontend.Desugaring.Initial.ToConst
Description :  Desugaring of AST nodes to Const
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Const-s.
-}
module Frontend.Desugaring.Initial.ToConst
    ( DesugarToConst(..)
    ) where

import Frontend.Desugaring.Initial.Ast
import Frontend.Syntax.Ast (Literal(..))
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)
import Frontend.Syntax.Token (CharT(..), FloatT(..), IntT(..), StringT(..))

-- | Class for types which can be desugared to Const
class DesugarToConst a where
    desugarToConst :: a -> WithLocation Const -- ^ Desugars object to Const

instance (DesugarToConst a) => DesugarToConst (WithLocation a) where
    desugarToConst = (getValue . desugarToConst <$>)

instance DesugarToConst IntT where
    desugarToConst (IntT x) = withDummyLocation $ ConstInt x

instance DesugarToConst FloatT where
    desugarToConst (FloatT x) = withDummyLocation $ ConstFloat x

instance DesugarToConst CharT where
    desugarToConst (CharT c) = withDummyLocation $ ConstChar c

instance DesugarToConst StringT where
    desugarToConst (StringT s) = withDummyLocation $ ConstString s

instance DesugarToConst Literal where
    desugarToConst (LiteralInteger i) = desugarToConst i
    desugarToConst (LiteralFloat f) = desugarToConst f
    desugarToConst (LiteralChar c) = desugarToConst c
    desugarToConst (LiteralString s) = desugarToConst s
