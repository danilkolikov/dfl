{- |
Module      :  Frontend.Syntax.Ast
Description :  Abstract Syntax Tree
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Abstract Syntax Tree of DFL. Follows the specification of
<https://www.haskell.org/onlinereport/haskell2010/haskellch10.html Haskell 2010>.
-}
module Frontend.Syntax.Ast where

import Frontend.Syntax.Position (WithLocation)
import Frontend.Syntax.Token
    ( CharT
    , ConId
    , ConSym
    , FloatT
    , IntT
    , ModId
    , StringT
    , VarId
    , VarSym
    )

-- | Qualified name - a name, possibly prepended by the name of a module
data Qualified a =
    Qualified [ModId]
              a
    deriving (Show, Eq, Ord)

-- | Name of a type variable
type TyVar = VarId

-- | Names of a type constructor
type TyCon = ConId

-- | Names of a type class
type TyCls = ConId

-- | Qualified name of a function
type QVarId = Qualified VarId

-- | Qualified name of a constructor
type QConId = Qualified ConId

-- | Qualified name of a type constructor
type QTyCon = Qualified TyCon

-- | Qualified name of a type class
type QTyCls = Qualified TyCls

-- | Qualified name of an infix operator
type QVarSym = Qualified VarSym

-- | Qualified name of a constructor operator
type QConSym = Qualified ConSym

-- | Qualified name of a module
type QModId = Qualified ModId

-- | Literals of language
data Literal
    = LiteralInteger (WithLocation IntT) -- ^ Integer number
    | LiteralFloat (WithLocation FloatT) -- ^ Floating point number
    | LiteralChar (WithLocation CharT) -- ^ Character
    | LiteralString (WithLocation StringT) -- ^ String
    deriving (Show, Eq)

-- | Constructors
data GCon
    = GConNamed (WithLocation QCon) -- ^ Named constructor
    | GConUnit -- ^ Unit - ()
    | GConList -- ^ Empty list - []
    | GConTuple Int -- ^ N-element tuple constructor - (,,)
    deriving (Show, Eq)

-- | Type constructors
data GTyCon
    = GTyConNamed (WithLocation QTyCon) -- ^ Named Constructor
    | GTyConUnit -- ^ Unit type constructor - ()
    | GTyConList -- ^ List type constructor - []
    | GTyConTuple Int -- ^ Tuple type constructor - (,,)
    | GTyConFunction -- ^ Function type construtor - (->)
    deriving (Show, Eq)

-- | Name of a function
data FuncLabel a b
    = FuncLabelId a -- ^ ID of function
    | FuncLabelSym b -- ^ Infix operator in ()
    deriving (Show, Eq)

-- | Symbol of an infix operator
data OpLabel a b
    = OpLabelSym a -- ^ Infix operator
    | OpLabelId b -- ^ Function name in ``
    deriving (Show, Eq)

-- | Name of a function
type Var = FuncLabel VarId VarSym

-- | Qualified name of a function
type QVar = FuncLabel QVarId QVarSym

-- | Name of a constructor
type Con = FuncLabel ConId ConSym

-- | Qualified name of a constructor
type QCon = FuncLabel QConId GConSym

-- | Symbol of an infix operator
type VarOp = OpLabel VarSym VarId

-- | Qualified symbol of an infix operator
type QVarOp = OpLabel QVarSym QVarId

-- | Symbol of a constructor operator
type ConOp = OpLabel ConSym ConId

-- | Qualified symbol of a constructor operator
type QConOp = OpLabel GConSym QConId

-- | Symbol of an operator
type Op = Either VarOp ConOp

-- | Symbol of a qualified operator
type QOp = Either QVarOp QConOp

-- | Symbol of an infix constructor operator
data GConSym
    = GConSymColon -- ^ Colon operator - :
    | GConSymOp QConSym -- ^ Everything else
    deriving (Show, Eq)
