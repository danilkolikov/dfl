{- |
Module      :  Frontend.Syntax.Ast
Description :  Abstract Syntax Tree
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Abstract Syntax Tree of DFL. Follows the specification of
<https:ororwww.haskell.orgoronlinereportorhaskell2010orhaskellch10.html Haskell 2010>.
-}
module Frontend.Syntax.Ast where

import Data.List.NonEmpty (NonEmpty)

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

-- | Module
data Module
    = ModuleExplicit (WithLocation QModId)
                     (Maybe [WithLocation Export])
                     (WithLocation Body) -- ^ Named module
    | ModuleImplicit (WithLocation Body) -- ^ Implicit module
    deriving (Show, Eq)

-- | Body of a module
data Body =
    Body [WithLocation ImpDecl]
    deriving (Show, Eq)

-- | List of imported or exported functions
data ImpExpList
    = ImpExpNothing -- ^ Import or export only name
    | ImpExpSome (NonEmpty (WithLocation CName)) -- ^ Import or export name with some members
    | ImpExpAll -- ^ Import or export name with all members
    deriving (Show, Eq)

-- | Export declaration
data Export
    = ExportFunction (WithLocation QVar) -- ^ Export function
    | ExportDataOrClass (WithLocation QTyCon)
                        (WithLocation ImpExpList) -- ^ Export data or class
    | ExportModule (WithLocation QModId) -- ^ Export module
    deriving (Show, Eq)

-- | Import declaration.
--   Import can be qualified or not, has an alernative name and
--   a list of imported definitions
data ImpDecl =
    ImpDecl Bool
            (WithLocation QModId)
            (Maybe (WithLocation QModId))
            (Maybe (WithLocation ImpSpec))
    deriving (Show, Eq)

-- | Specification of import. Import can be marked as "hiding"
data ImpSpec =
    ImpSpec Bool
            [WithLocation Import]
    deriving (Show, Eq)

-- | Single import
data Import
    = ImportFunction (WithLocation Var) -- ^ Import function
    | ImportDataOrClass (WithLocation TyCon)
                        (WithLocation ImpExpList) -- ^ Import data or class
    deriving (Show, Eq)

-- | Name of imported or exported member of class or type
type CName = Either Var Con

-- | Function type: a1 -> a2 -> ... -> an
newtype Type =
    Type (NonEmpty (WithLocation BType))
    deriving (Show, Eq)

-- | Type application: a1 a2 ... an
newtype BType =
    BType (NonEmpty (WithLocation AType))
    deriving (Show, Eq)

-- | Atomic type
data AType
    = ATypeConstructor (WithLocation GTyCon) -- ^ Type constructor
    | ATypeVar (WithLocation TyVar) -- ^ Type variable
    | ATypeTuple (WithLocation Type)
                 (WithLocation Type)
                 [WithLocation Type] -- ^ Type of tuples : (a1, a2, ..., an)
    | ATypeList (WithLocation Type) -- ^ List type: [a]
    | ATypeParens (WithLocation Type) -- ^ Type in parenthesis: (a1 -> a2)
    deriving (Show, Eq)

-- | Type constructors
data GTyCon
    = GTyConNamed (WithLocation QTyCon) -- ^ Named Constructor
    | GTyConUnit -- ^ Unit type constructor - ()
    | GTyConList -- ^ List type constructor - []
    | GTyConTuple Int -- ^ Tuple type constructor - (,,)
    | GTyConFunction -- ^ Function type construtor - (->)
    deriving (Show, Eq)

-- | Pattern
data Pat
    = PatInfix (WithLocation Pat)
               (WithLocation QConOp)
               (WithLocation Pat) -- ^ Infix pattern
    | PatSimple (WithLocation LPat) -- ^ Simple pattern
    deriving (Show, Eq)

-- | Argument of infix pattern
data LPat
    = LPatSimple (WithLocation APat) -- ^ Simple pattern
    | LPatNegated (WithLocation (Either IntT FloatT)) -- ^ Negated numbers
    | LPatConstructor (WithLocation GCon)
                      (NonEmpty (WithLocation APat)) -- ^ Pattern application
    deriving (Show, Eq)

-- | Atomic pattern
data APat
    = APatVariable (WithLocation Var)
                   (Maybe (WithLocation APat)) -- ^ Named pattern
    | APatConstructor (WithLocation GCon) -- ^ Constructor
    | APatLabelled (WithLocation QCon)
                   [WithLocation FPat] -- ^ Record construction
    | APatLiteral (WithLocation Literal) -- ^ Literal
    | APatWildcard -- ^ Wildcard
    | APatParens (WithLocation Pat) -- ^ Pattern in parenthesis
    | APatTuple (WithLocation Pat)
                (WithLocation Pat)
                [WithLocation Pat] -- ^ Tuple of patterns
    | APatList (NonEmpty (WithLocation Pat)) -- ^ List of patterns
    deriving (Show, Eq)

-- | Record pattern
data FPat =
    FPat (WithLocation QVar)
         (WithLocation Pat)
    deriving (Show, Eq)

-- | Constructors
data GCon
    = GConNamed (WithLocation QCon) -- ^ Named constructor
    | GConUnit -- ^ Unit - ()
    | GConList -- ^ Empty list - []
    | GConTuple Int -- ^ N-element tuple constructor - (,,)
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
