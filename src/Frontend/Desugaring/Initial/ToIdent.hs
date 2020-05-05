{- |
Module      :  Frontend.Desugaring.Initial.ToIdent
Description :  Desugaring of AST nodes to Ident
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugaring of AST nodes to objects, representing Ident-s.
-}
module Frontend.Desugaring.Initial.ToIdent
    ( DesugarToIdent(..)
    , DesugarToSimpleIdent(..)
    ) where

import Data.Functor (($>))

import Core.Ident (SimpleIdent(..), UserDefinedIdent(..))
import Core.PredefinedIdents
import Frontend.Syntax.Ast
    ( DClass(..)
    , FuncLabel(..)
    , GCon(..)
    , GConSym(..)
    , GTyCon(..)
    , OpLabel(..)
    , Qualified(..)
    )
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token (ConId(..), ConSym(..), VarId(..), VarSym(..))

-- | Class for types which can be desugared to SimpleIdent
class DesugarToSimpleIdent a where
    desugarToSimpleIdent :: a -> SimpleIdent

instance DesugarToSimpleIdent ConId where
    desugarToSimpleIdent (ConId s) = IdentNamed s

instance DesugarToSimpleIdent ConSym where
    desugarToSimpleIdent (ConSym s) = IdentNamed s

instance DesugarToSimpleIdent VarId where
    desugarToSimpleIdent (VarId s) = IdentNamed s

instance DesugarToSimpleIdent VarSym where
    desugarToSimpleIdent (VarSym s) = IdentNamed s

-- | Class for types which can be desugared to Ident-s
class DesugarToIdent a where
    desugarToIdent :: WithLocation a -> WithLocation UserDefinedIdent -- ^ Desugar object to Ident

desugarSimpleIdentToIdent ::
       (DesugarToSimpleIdent a)
    => WithLocation a
    -> WithLocation UserDefinedIdent
desugarSimpleIdentToIdent c =
    c $> IdentSimple (desugarToSimpleIdent $ getValue c)

instance (DesugarToIdent a, DesugarToIdent b) =>
         DesugarToIdent (OpLabel a b) where
    desugarToIdent opLabel =
        case getValue opLabel of
            OpLabelId name -> desugarToIdent (opLabel $> name)
            OpLabelSym sym -> desugarToIdent (opLabel $> sym)

instance (DesugarToIdent a, DesugarToIdent b) =>
         DesugarToIdent (FuncLabel a b) where
    desugarToIdent funcLabel =
        case getValue funcLabel of
            FuncLabelId name -> desugarToIdent (funcLabel $> name)
            FuncLabelSym sym -> desugarToIdent (funcLabel $> sym)

instance (DesugarToIdent a, DesugarToIdent b) =>
         DesugarToIdent (Either a b) where
    desugarToIdent value =
        case getValue value of
            Left a -> desugarToIdent (value $> a)
            Right b -> desugarToIdent (value $> b)

instance (DesugarToIdent a) => DesugarToIdent (WithLocation a) where
    desugarToIdent (WithLocation a _) = desugarToIdent a

instance DesugarToIdent ConId where
    desugarToIdent = desugarSimpleIdentToIdent

instance DesugarToIdent ConSym where
    desugarToIdent = desugarSimpleIdentToIdent

instance DesugarToIdent VarId where
    desugarToIdent = desugarSimpleIdentToIdent

instance DesugarToIdent VarSym where
    desugarToIdent = desugarSimpleIdentToIdent

instance (DesugarToSimpleIdent a) => DesugarToIdent (Qualified a) where
    desugarToIdent c =
        c $>
        case getValue c of
            Qualified path a ->
                if null path
                    then IdentSimple (desugarToSimpleIdent a)
                    else IdentQualified
                             (map getPath path)
                             (desugarToSimpleIdent a)
      where
        getPath (ConId name) = name

instance DesugarToIdent GConSym where
    desugarToIdent gc =
        gc $>
        case getValue gc of
            GConSymColon -> cOLON
            GConSymOp s -> getValue $ desugarToIdent (gc $> s)

instance DesugarToIdent GCon where
    desugarToIdent gCon =
        gCon $>
        case getValue gCon of
            GConNamed name -> getValue $ desugarToIdent name
            GConUnit -> uNIT
            GConList -> lIST
            GConTuple n -> tUPLE n

instance DesugarToIdent GTyCon where
    desugarToIdent gTyCon =
        gTyCon $>
        case getValue gTyCon of
            GTyConNamed name -> getValue $ desugarToIdent name
            GTyConUnit -> uNIT
            GTyConList -> lIST
            GTyConTuple n -> tUPLE n
            GTyConFunction -> fUNCTION

instance DesugarToIdent DClass where
    desugarToIdent dClass
        | DClass name <- getValue dClass = desugarToIdent name
