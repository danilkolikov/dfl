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

import Data.Functor (($>))

import Frontend.Desugaring.Initial.Ast
import Frontend.Syntax.Ast (Literal(..))
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token (CharT(..), FloatT(..), IntT(..), StringT(..))

-- | Class for types which can be desugared to Const
class DesugarToConst a where
    desugarToConst :: WithLocation a -> WithLocation Const -- ^ Desugars object to Const

instance DesugarToConst IntT where
    desugarToConst int
        | IntT x <- getValue int = int $> ConstInt x

instance DesugarToConst FloatT where
    desugarToConst float
        | FloatT x <- getValue float = float $> ConstFloat x

instance DesugarToConst CharT where
    desugarToConst char
        | CharT c <- getValue char = char $> ConstChar c

instance DesugarToConst StringT where
    desugarToConst string
        | StringT s <- getValue string = string $> ConstString s

instance DesugarToConst Literal where
    desugarToConst literal =
        case getValue literal of
            LiteralInteger i -> desugarToConst i
            LiteralFloat f -> desugarToConst f
            LiteralChar c -> desugarToConst c
            LiteralString s -> desugarToConst s
