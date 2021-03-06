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
    , VarSym(..)
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
data Module a
    = ModuleExplicit (WithLocation QModId)
                     (Maybe [WithLocation Export])
                     (WithLocation a) -- ^ Named module
    | ModuleImplicit (WithLocation a) -- ^ Implicit module
    deriving (Show, Eq)

-- | A list of imports of a module
newtype Header =
    Header [WithLocation ImpDecl] -- ^ Imports
    deriving (Show, Eq)

-- | Body of a module
data Body =
    Body [WithLocation ImpDecl]
         [WithLocation TopDecl]
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

-- | Top-level declaration
data TopDecl
    = TopDeclType (WithLocation SimpleType)
                  (WithLocation Type) -- ^ Declaration of type
    | TopDeclData [WithLocation Class]
                  (WithLocation SimpleType)
                  [WithLocation Constr]
                  [WithLocation DClass] -- ^ Declaration of a data class
    | TopDeclNewType [WithLocation Class]
                     (WithLocation SimpleType)
                     (WithLocation NewConstr)
                     [WithLocation DClass] -- ^ Declaration of a new type
    | TopDeclClass [WithLocation SimpleClass]
                   (WithLocation TyCls)
                   (WithLocation TyVar)
                   [WithLocation CDecl] -- ^ Declaration of a type class
    | TopDeclInstance [WithLocation SimpleClass]
                      (WithLocation QTyCls)
                      (WithLocation Inst)
                      [WithLocation IDecl] -- ^ Declaration of an instance
    | TopDeclDecl (WithLocation Decl) -- ^ Simple declaration
    deriving (Show, Eq)

-- | Simple declaration
data Decl
    = DeclGenDecl (WithLocation GenDecl) -- ^ General declaration
    | DeclFunction (WithLocation (Either FunLHS Pat))
                   (WithLocation RHS) -- ^ Function declaration
    deriving (Show, Eq)

-- | Declaration in type class
data CDecl
    = CDeclGenDecl (WithLocation GenDecl) -- ^ General declaration
    | CDeclFunction (WithLocation (Either FunLHS Var))
                    (WithLocation RHS) -- ^ Default implementation
    deriving (Show, Eq)

-- | Declaration in instance
data IDecl =
    IDeclFunction (WithLocation (Either FunLHS Var))
                  (WithLocation RHS) -- ^ Implementation
    deriving (Show, Eq)

-- | General declaration
data GenDecl
    = GenDeclTypeSig Vars
                     [WithLocation Class]
                     (WithLocation Type) -- ^ Type declaration for a function
    | GenDeclFixity (WithLocation Fixity)
                    (WithLocation IntT)
                    Ops -- ^ Fixity declaration
    deriving (Show, Eq)

-- | Fixity of an operator
data Fixity
    = InfixL -- ^ Left associativity
    | InfixR -- ^ Right associativity
    | Infix -- ^ Non-associative
    deriving (Show, Eq, Ord)

-- | Non-empty list of operators
type Ops = NonEmpty (WithLocation Op)

-- | Non-empty list of variables
type Vars = NonEmpty (WithLocation Var)

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

-- | Class in a type context
data Class
    = ClassSimple (WithLocation QTyCls)
                  (WithLocation TyVar) -- ^ Application "SomeClass a"
    | ClassApplied (WithLocation QTyCls)
                   (WithLocation TyVar)
                   (NonEmpty (WithLocation AType)) -- ^ Application "SomeClass (a Foo Bar)"
    deriving (Show, Eq)

-- | Class in definition of a type class
data SimpleClass =
    SimpleClass (WithLocation QTyCls)
                (WithLocation TyVar)
    deriving (Show, Eq)

-- | Type in definition of a type class
data SimpleType =
    SimpleType (WithLocation TyCon)
               [WithLocation TyVar]
    deriving (Show, Eq)

-- | Non-empty list of constructors
type Constrs = NonEmpty (WithLocation Constr)

-- | Definition of a constructor
data Constr
    = ConstrSimple (WithLocation Con)
                   [WithLocation AType] -- ^ Simple constructor
    | ConstrInfix (WithLocation BType)
                  (WithLocation ConOp)
                  (WithLocation BType) -- ^ Infix constructor
    | ConstrRecord (WithLocation Con)
                   [WithLocation FieldDecl] -- ^ Record
    deriving (Show, Eq)

-- | Definition of a new type
data NewConstr
    = NewConstrSimple (WithLocation Con)
                      (WithLocation AType) -- ^ Simple
    | NewConstrNamed (WithLocation Con)
                     (WithLocation Var)
                     (WithLocation Type) -- ^ With getters
    deriving (Show, Eq)

-- | Definition of fields in a record
data FieldDecl =
    FieldDecl Vars
              (WithLocation Type)
    deriving (Show, Eq)

-- | Class in deriving
newtype DClass =
    DClass (WithLocation QTyCls)
    deriving (Show, Eq)

-- | Instance definition
data Inst
    = InstNamed (WithLocation GTyCon)
                [WithLocation TyVar] -- ^ Named instance
    | InstTuple (WithLocation TyVar)
                (WithLocation TyVar)
                [WithLocation TyVar] -- ^ Tuple
    | InstList (WithLocation TyVar) -- ^ List
    | InstFunction (WithLocation TyVar)
                   (WithLocation TyVar) -- ^ Function
    deriving (Show, Eq)

-- | Left hand side of a function
data FunLHS
    = FunLHSSimple (WithLocation Var)
                   (NonEmpty (WithLocation APat)) -- ^ Simple
    | FunLHSInfix (WithLocation Pat)
                  (WithLocation VarOp)
                  (WithLocation Pat) -- ^ Infix operator
    | FunLHSNested (WithLocation FunLHS)
                   (NonEmpty (WithLocation APat)) -- ^ Nested
    deriving (Show, Eq)

-- | Right hand side of a function
data RHS
    = RHSSimple (WithLocation Exp)
                [WithLocation Decl] -- ^ Simple
    | RHSGuarded (NonEmpty (WithLocation GdRHS))
                 [WithLocation Decl] -- ^ With guards
    deriving (Show, Eq)

-- | Right hand side of a guard
data GdRHS =
    GdRHS Guards
          (WithLocation Exp)
    deriving (Show, Eq)

-- | Non-empty list of guards
type Guards = NonEmpty (WithLocation Guard)

-- | Guard
data Guard
    = GuardPattern (WithLocation Pat)
                   (WithLocation InfixExp) -- ^ Pattern
    | GuardLet [WithLocation Decl] -- ^ Let-guard
    | GuardExpr (WithLocation InfixExp) -- ^ Boolean guard
    deriving (Show, Eq)

-- | Expression
data Exp
    = ExpTyped (WithLocation InfixExp)
               [WithLocation Class]
               (WithLocation Type) -- ^ Expression with an explicit type declaration
    | ExpSimple (WithLocation InfixExp) -- ^ Simple expression
    deriving (Show, Eq)

-- | Infix expression
data InfixExp
    = InfixExpApplication (WithLocation InfixExp)
                          (WithLocation QOp)
                          (WithLocation InfixExp) -- ^ Binary expression
    | InfixExpNegated (WithLocation QOp)
                      (WithLocation InfixExp) -- ^ Negation
    | InfixExpLExp (WithLocation LExp) -- ^ Atomic expression
    deriving (Show, Eq)

-- | Lambda-expression
data LExp
    = LExpAbstraction (NonEmpty (WithLocation APat))
                      (WithLocation Exp) -- ^ Lambda-abstraction
    | LExpLet [WithLocation Decl]
              (WithLocation Exp) -- ^ Let-abstraction
    | LExpIf (WithLocation Exp)
             (WithLocation Exp)
             (WithLocation Exp) -- ^ If expression
    | LExpCase (WithLocation Exp)
               Alts -- ^ Case expression
    | LExpDo (NonEmpty (WithLocation Stmt)) -- ^ Do expression
    | LExpApplication (NonEmpty (WithLocation AExp)) -- ^ Application
    deriving (Show, Eq)

-- | Atomic expression
data AExp
    = AExpVariable (WithLocation QVar) -- ^ Variable
    | AExpConstructor (WithLocation GCon) -- ^ Constructor
    | AExpLiteral (WithLocation Literal) -- ^ Literal
    | AExpParens (WithLocation Exp) -- ^ Expression in parenthesis
    | AExpTuple (WithLocation Exp)
                (WithLocation Exp)
                [WithLocation Exp] -- ^ Tuple: (a, b, c)
    | AExpList (NonEmpty (WithLocation Exp)) -- ^ List: [a, b, c]
    | AExpSequence (WithLocation Exp)
                   (Maybe (WithLocation Exp))
                   (Maybe (WithLocation Exp)) -- ^ Sequence: [1, 2 .. 4]
    | AExpListCompr (WithLocation Exp)
                    (NonEmpty (WithLocation Qual)) -- ^ List comprehension
    | AExpLeftSection (WithLocation InfixExp)
                      (WithLocation QOp) -- ^ Left section: (e -)
    | AExpRightSection (WithLocation QOp)
                       (WithLocation InfixExp) -- ^ Right section: (+ e)
    | AExpRecordConstr (WithLocation QCon)
                       [WithLocation FBind] -- ^ Record constructor
    | AExpRecordUpdate (WithLocation AExp)
                       (NonEmpty (WithLocation FBind)) -- ^ Record update
    deriving (Show, Eq)

-- | Qualifier in a list comprehension expression
data Qual
    = QualGenerator (WithLocation Pat)
                    (WithLocation Exp) -- ^ Generator of values
    | QualLet [WithLocation Decl] -- ^ Declaration of values
    | QualGuard (WithLocation Exp) -- ^ Boolean guard
    deriving (Show, Eq)

-- | Non-empty list of alternatives
type Alts = NonEmpty (WithLocation Alt)

-- | Alternatives in the case expression
data Alt
    = AltSimple (WithLocation Pat)
                (WithLocation Exp)
                [WithLocation Decl] -- ^ Simple
    | AltGuarded (WithLocation Pat)
                 (NonEmpty (WithLocation GdPat))
                 [WithLocation Decl] -- ^ Guarded
    deriving (Show, Eq)

-- | Guard in a case expression
data GdPat =
    GdPat Guards
          (WithLocation Exp)
    deriving (Show, Eq)

-- | Statement in do expression
data Stmt
    = StmtExp (WithLocation Exp) -- ^ Simple expression
    | StmtPat (WithLocation Pat)
              (WithLocation Exp) -- ^ Pattern assignment
    | StmtLet [WithLocation Decl] -- ^ Let declaration
    deriving (Show, Eq)

-- | Record field binding
data FBind =
    FBind (WithLocation QVar)
          (WithLocation Exp)
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
    | APatRecord (WithLocation QCon)
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

-- | Minus is a special case of an operator
minus :: QOp
minus = Left (OpLabelSym (Qualified [] (VarSym "-")))
