{- |
Module      :  Frontend.Desugaring.Ast
Description :  Desugared version of AST of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugared version of AST of DFL. Contains significantly less kinds of nodes,
but still has nodes related to records.
-}
module Frontend.Desugaring.Ast where

import Data.List.NonEmpty (NonEmpty)

import Frontend.Syntax.EntityName (EntityName)
import Frontend.Syntax.Position (WithLocation)

-- | Identifiers in AST
data Ident
    = IdentNamed EntityName -- ^ Name
    | IdentParametrised EntityName
                        Int -- ^ Name with a parameter. E.G. - tuples with different number of arguments
    | IdentGenerated Int -- ^ Automatically generated name
    deriving (Show, Eq)

-- | Constant values
data Const
    = ConstInt Int -- ^ Integer value
    | ConstFloat Double -- ^ Floating point value
    | ConstChar Char -- ^ Character
    | ConstString String -- ^ String
    deriving (Show, Eq)

-- | Definition of a module
data Module =
    Module (WithLocation Ident) -- ^ Module name
           (ImpExpList (WithLocation Export)) -- ^ Exports
           [WithLocation ImpDecl] -- ^ Imports
           [WithLocation TopDecl] -- ^ Top-level declarations
    deriving (Show, Eq)

-- | Export of a module
data Export
    = ExportFunction (WithLocation Ident) -- ^ Export single function
    | ExportDataOrClass (WithLocation Ident)
                        (ImpExpList (WithLocation Ident)) -- ^ Export data or class
    | ExportModule (WithLocation Ident) -- ^ Export module
    deriving (Show, Eq)

-- | List of exports or imports
data ImpExpList a
    = ImpExpNothing -- ^ Import or export nothing
    | ImpExpSome (NonEmpty a) -- ^ Import or export some definitions
    | ImpExpAll -- ^ Import or export all definitions
    deriving (Show, Eq)

-- | Import declaration.
data ImpDecl =
    ImpDecl Bool -- ^ Is it qualified?
            (WithLocation Ident) -- ^ Module name
            (Maybe (WithLocation Ident)) -- ^ Alternative name
            Bool -- ^ Should we hide following imports?
            (ImpExpList (WithLocation Import)) -- ^ List of imports
    deriving (Show, Eq)

-- | Single import
data Import
    = ImportFunction (WithLocation Ident) -- ^ Import function
    | ImportDataOrClass (WithLocation Ident)
                        (ImpExpList (WithLocation Ident)) -- ^ Import data or class
    deriving (Show, Eq)

-- | Top-level declarations
data TopDecl
    = TopDeclType (WithLocation SimpleType)
                  (WithLocation Type) -- ^ Definition of a type synonym
    | TopDeclData [WithLocation Constraint]
                  (WithLocation SimpleType)
                  [WithLocation Constr]
                  [WithLocation Ident] -- ^ Definition of a data type
    | TopDeclNewType [WithLocation Constraint]
                     (WithLocation SimpleType)
                     (WithLocation NewConstr)
                     [WithLocation Ident] -- ^ Definition of a `newtype`
    | TopDeclClass [WithLocation SimpleClass]
                   (WithLocation Ident)
                   (WithLocation Ident)
                   [WithLocation ClassAssignment] -- ^ Definition of a class
    | TopDeclInstance [WithLocation SimpleClass]
                      (WithLocation Ident)
                      (WithLocation Inst)
                      [WithLocation InstAssignment] -- ^ Definition of an instance
    | TopDeclAssignment (WithLocation Assignment) -- ^ Assignment
    deriving (Show, Eq)

-- | Constructor of a data type
data Constr
    = ConstrSimple (WithLocation Ident)
                   [WithLocation Type] -- ^ Usual constructor
    | ConstrRecord (WithLocation Ident)
                   [WithLocation FieldDecl] -- ^ Record constructor
    deriving (Show, Eq)

-- | Definition of a field in a record
data FieldDecl =
    FieldDecl (WithLocation Ident) -- ^ Name of a field
              (WithLocation Type) -- ^ Type of a field
    deriving (Show, Eq)

-- | Instance
data Inst =
    Inst (WithLocation Ident) -- ^ Name of a type
         [WithLocation Ident] -- ^ Arguments of a type
    deriving (Show, Eq)

-- | Constructor of a `newtype`
data NewConstr
    = NewConstrSimple (WithLocation Ident)
                      (WithLocation Type) -- ^ Simple constructor
    | NewConstrNamed (WithLocation Ident)
                     (WithLocation Ident)
                     (WithLocation Type) -- ^ Constructor with a getter
    deriving (Show, Eq)

-- | Constraint on a type
data Constraint =
    Constraint (WithLocation Ident) -- ^ Name of a class
               (WithLocation Ident) -- ^ Argument
               [WithLocation Type] -- ^ Optional arguments of the argument
    deriving (Show, Eq)

-- | Simple type
data SimpleType =
    SimpleType (WithLocation Ident) -- ^ Type name
               [WithLocation Ident] -- ^ Type arguments
    deriving (Show, Eq)

-- | Simple class
data SimpleClass =
    SimpleClass (WithLocation Ident) -- ^ Class name
                (WithLocation Ident) -- ^ Class arguments
    deriving (Show, Eq)

-- | Type
data Type
    = TypeApplication (WithLocation Type)
                      (NonEmpty (WithLocation Type)) -- ^ Application of a type constructor
    | TypeVar (WithLocation Ident) -- ^ Type variable
    | TypeConstr (WithLocation Ident) -- ^ Type constructor
    deriving (Show, Eq)

-- | Assignment in top-level, `let` or `where` blocks
data Assignment
    = AssignmentName (WithLocation Ident)
                     (WithLocation Exp) -- ^ Assign some expression to a name
    | AssignmentPattern (WithLocation Pattern)
                        (WithLocation Exp) -- ^ Assign some expression to a pattern
    | AssignmentType (WithLocation Ident)
                     [WithLocation Constraint]
                     (WithLocation Type) -- ^ Define type of a name
    deriving (Show, Eq)

-- | Assignment in a class definition
data ClassAssignment
    = ClassAssignmentName (WithLocation Ident)
                          (WithLocation Exp) -- ^ Assign expression to a name
    | ClassAssignmentType (WithLocation Ident)
                          [WithLocation Constraint]
                          (WithLocation Type) -- ^ Define type of an expression
    deriving (Show, Eq)

-- | Assignemnt in an instance definition
data InstAssignment =
    InstAssignmentName (WithLocation Ident)
                       (WithLocation Exp) -- ^ Assign expression to a name
    deriving (Show, Eq)

-- | Pattern
data Pattern
    = PatternConstr (WithLocation Ident)
                    [WithLocation Pattern] -- ^ Application of a constructor
    | PatternRecord (WithLocation Ident)
                    [WithLocation PatternBinding] -- ^ Application of a record constructor
    | PatternVar (WithLocation Ident)
                 (Maybe (WithLocation Pattern)) -- ^ Variable with possible pattern
    | PatternConst (WithLocation Const) -- ^ Constant
    | PatternWildcard -- ^ Wildcard
    deriving (Show, Eq)

-- | Record pattern binding
data PatternBinding =
    PatternBinding (WithLocation Ident) -- ^ Field name
                   (WithLocation Pattern) -- ^ Pattern
    deriving (Show, Eq)

-- | Expression
data Exp
    = ExpAbstraction (NonEmpty (WithLocation Ident))
                     (WithLocation Exp) -- ^ Lambda-abstraction
    | ExpLet [WithLocation Assignment]
             (WithLocation Exp) -- ^ Let-abstraction
    | ExpCase (WithLocation Exp)
              (NonEmpty (WithLocation Alt)) -- ^ Case expression
    | ExpApplication (WithLocation Exp)
                     (NonEmpty (WithLocation Exp)) -- ^ Application of expressions
    | ExpVar (WithLocation Ident) -- ^ Variable
    | ExpConst (WithLocation Const) -- ^ Constant
    | ExpRecordConstr (WithLocation Ident)
                      [WithLocation Binding] -- ^ Construction of a record
    | ExpRecordUpdate (WithLocation Exp)
                      (NonEmpty (WithLocation Binding)) -- ^ Update of a record
    deriving (Show, Eq)

-- | Record field binding
data Binding =
    Binding (WithLocation Ident) -- ^ Field name
            (WithLocation Exp) -- ^ Expression
    deriving (Show, Eq)

-- | Alternative in `case` expressions
data Alt =
    Alt (WithLocation Pattern) -- ^ Pattern
        (WithLocation Exp) -- ^ Expression
    deriving (Show, Eq)
