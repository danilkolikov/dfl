{- |
Module      :  Frontend.Desugaring.Initial.Ast
Description :  Desugared version of AST of DFL
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Desugared version of AST of DFL. Contains significantly less kinds of nodes,
but still has nodes related to records.
-}
module Frontend.Desugaring.Initial.Ast where

import Data.List.NonEmpty (NonEmpty)

import Core.Ident (UserDefinedIdent)
import Frontend.Syntax.Position (WithLocation)

-- | Constant values
data Const
    = ConstInt Int -- ^ Integer value
    | ConstFloat Double -- ^ Floating point value
    | ConstChar Char -- ^ Character
    | ConstString String -- ^ String
    deriving (Show, Eq)

-- | Definition of a module
data Module =
    Module (WithLocation UserDefinedIdent) -- ^ Module name
           (ImpExpList (WithLocation Export)) -- ^ Exports
           [WithLocation ImpDecl] -- ^ Imports
           [WithLocation TopDecl] -- ^ Top-level declarations
    deriving (Show, Eq)

-- | Header of a module
data Header =
    Header (WithLocation UserDefinedIdent) -- ^ Module name
           (ImpExpList (WithLocation Export)) -- ^ Exports
           [WithLocation ImpDecl] -- ^ Imports
    deriving (Show, Eq)

-- | Export of a module
data Export
    = ExportFunction (WithLocation UserDefinedIdent) -- ^ Export single function
    | ExportDataOrClass (WithLocation UserDefinedIdent)
                        (ImpExpList (WithLocation UserDefinedIdent)) -- ^ Export data or class
    | ExportModule (WithLocation UserDefinedIdent) -- ^ Export module
    deriving (Show, Eq)

-- | List of exports or imports
data ImpExpList a
    = ImpExpNothing -- ^ Import or export nothing
    | ImpExpSome (NonEmpty a) -- ^ Import or export some definitions
    | ImpExpAll -- ^ Import or export all definitions
    deriving (Show, Eq)

instance Functor ImpExpList where
    fmap f list =
        case list of
            ImpExpNothing -> ImpExpNothing
            ImpExpAll -> ImpExpAll
            ImpExpSome ne -> ImpExpSome (fmap f ne)

instance Foldable ImpExpList where
    foldr f b list =
        case list of
            ImpExpNothing -> b
            ImpExpAll -> b
            ImpExpSome ne -> foldr f b ne

instance Traversable ImpExpList where
    traverse f list = case list of
      ImpExpNothing -> pure ImpExpNothing
      ImpExpAll -> pure ImpExpAll
      ImpExpSome ne -> ImpExpSome <$> traverse f ne

-- | Import declaration.
data ImpDecl =
    ImpDecl Bool -- ^ Is it qualified?
            (WithLocation UserDefinedIdent) -- ^ Module name
            (Maybe (WithLocation UserDefinedIdent)) -- ^ Alternative name
            Bool -- ^ Should we hide following imports?
            (ImpExpList (WithLocation Import)) -- ^ List of imports
    deriving (Show, Eq)

-- | Single import
data Import
    = ImportFunction (WithLocation UserDefinedIdent) -- ^ Import function
    | ImportDataOrClass (WithLocation UserDefinedIdent)
                        (ImpExpList (WithLocation UserDefinedIdent)) -- ^ Import data or class
    deriving (Show, Eq)

-- | Top-level declarations
data TopDecl
    = TopDeclType (WithLocation SimpleType)
                  (WithLocation Type) -- ^ Definition of a type synonym
    | TopDeclData [WithLocation Constraint]
                  (WithLocation SimpleType)
                  [WithLocation Constr]
                  [WithLocation UserDefinedIdent] -- ^ Definition of a data type
    | TopDeclNewType [WithLocation Constraint]
                     (WithLocation SimpleType)
                     (WithLocation NewConstr)
                     [WithLocation UserDefinedIdent] -- ^ Definition of a `newtype`
    | TopDeclClass [WithLocation SimpleClass]
                   (WithLocation UserDefinedIdent)
                   (WithLocation UserDefinedIdent)
                   [WithLocation ClassAssignment] -- ^ Definition of a class
    | TopDeclInstance [WithLocation SimpleClass]
                      (WithLocation UserDefinedIdent)
                      (WithLocation Inst)
                      [WithLocation InstAssignment] -- ^ Definition of an instance
    | TopDeclAssignment (WithLocation Assignment) -- ^ Assignment
    deriving (Show, Eq)

-- | Constructor of a data type
data Constr
    = ConstrSimple (WithLocation UserDefinedIdent)
                   [WithLocation Type] -- ^ Usual constructor
    | ConstrRecord (WithLocation UserDefinedIdent)
                   [WithLocation FieldDecl] -- ^ Record constructor
    deriving (Show, Eq)

-- | Definition of a field in a record
data FieldDecl =
    FieldDecl (WithLocation UserDefinedIdent) -- ^ Name of a field
              (WithLocation Type) -- ^ Type of a field
    deriving (Show, Eq)

-- | Instance
data Inst =
    Inst (WithLocation UserDefinedIdent) -- ^ Name of a type
         [WithLocation UserDefinedIdent] -- ^ Arguments of a type
    deriving (Show, Eq)

-- | Constructor of a `newtype`
data NewConstr
    = NewConstrSimple (WithLocation UserDefinedIdent)
                      (WithLocation Type) -- ^ Simple constructor
    | NewConstrRecord (WithLocation UserDefinedIdent)
                      (WithLocation UserDefinedIdent)
                      (WithLocation Type) -- ^ Record-style constructor
    deriving (Show, Eq)

-- | Constraint on a type
data Constraint =
    Constraint (WithLocation UserDefinedIdent) -- ^ Name of a class
               (WithLocation UserDefinedIdent) -- ^ Argument
               [WithLocation Type] -- ^ Optional arguments of the argument
    deriving (Show, Eq)

-- | Simple type
data SimpleType =
    SimpleType (WithLocation UserDefinedIdent) -- ^ Type name
               [WithLocation UserDefinedIdent] -- ^ Type arguments
    deriving (Show, Eq)

-- | Simple class
data SimpleClass =
    SimpleClass (WithLocation UserDefinedIdent) -- ^ Class name
                (WithLocation UserDefinedIdent) -- ^ Class arguments
    deriving (Show, Eq)

-- | Type
data Type
    = TypeApplication (WithLocation Type)
                      (NonEmpty (WithLocation Type)) -- ^ Application of a type constructor
    | TypeFunction (WithLocation Type)
                   (WithLocation Type) -- ^ Function type
    | TypeVar (WithLocation UserDefinedIdent) -- ^ Type variable
    | TypeConstr (WithLocation UserDefinedIdent) -- ^ Type constructor
    deriving (Show, Eq)

-- | Fixity of an operator
data Fixity
    = InfixL -- ^ Left associativity
    | InfixR -- ^ Right associativity
    | Infix -- ^ Non-associative
    deriving (Show, Eq, Ord)

-- | Assignment in top-level, `let` or `where` blocks
data Assignment
    = AssignmentName (WithLocation UserDefinedIdent)
                     (NonEmpty (WithLocation Pattern))
                     (WithLocation Exp) -- ^ Assign some expression to a name
    | AssignmentPattern (WithLocation Pattern)
                        (WithLocation Exp) -- ^ Assign some expression to a pattern
    | AssignmentType (WithLocation UserDefinedIdent)
                     [WithLocation Constraint]
                     (WithLocation Type) -- ^ Define type of a name
    | AssignmentFixity (WithLocation UserDefinedIdent)
                       Fixity
                       Int -- ^ Assigns fixity to an operator
    deriving (Show, Eq)

-- | Assignment in a class definition
data ClassAssignment
    = ClassAssignmentName (WithLocation UserDefinedIdent)
                          [WithLocation Pattern]
                          (WithLocation Exp) -- ^ Assign expression to a name
    | ClassAssignmentType (WithLocation UserDefinedIdent)
                          [WithLocation Constraint]
                          (WithLocation Type) -- ^ Define type of an expression
    | ClassAssignmentFixity (WithLocation UserDefinedIdent)
                            Fixity
                            Int -- ^ Assigns fixity to an operator
    deriving (Show, Eq)

-- | Assignemnt in an instance definition
data InstAssignment =
    InstAssignmentName (WithLocation UserDefinedIdent)
                       [WithLocation Pattern]
                       (WithLocation Exp) -- ^ Assign expression to a name
    deriving (Show, Eq)

-- | Infix pattern
data InfixPattern
    = InfixPatternApplication (WithLocation InfixPattern)
                              (WithLocation UserDefinedIdent)
                              (WithLocation InfixPattern)
    | InfixPatternSimple (WithLocation Pattern)
    deriving (Eq, Show)

-- | Pattern
data Pattern
    = PatternInfix (WithLocation InfixPattern) -- ^ Infix pattern
    | PatternConstr (WithLocation UserDefinedIdent)
                    [WithLocation Pattern] -- ^ Application of a constructor
    | PatternRecord (WithLocation UserDefinedIdent)
                    [WithLocation PatternBinding] -- ^ Application of a record constructor
    | PatternVar (WithLocation UserDefinedIdent)
                 (Maybe (WithLocation Pattern)) -- ^ Variable with possible pattern
    | PatternConst (WithLocation Const) -- ^ Constant
    | PatternWildcard -- ^ Wildcard
    deriving (Show, Eq)

-- | Record pattern binding
data PatternBinding =
    PatternBinding (WithLocation UserDefinedIdent) -- ^ Field name
                   (WithLocation Pattern) -- ^ Pattern
    deriving (Show, Eq)

-- | Infix expression
data InfixExp
    = InfixExpApplication (WithLocation InfixExp)
                          (WithLocation UserDefinedIdent)
                          (WithLocation InfixExp)
    | InfixExpNegated (WithLocation UserDefinedIdent)
                      (WithLocation InfixExp)
    | InfixExpSimple (WithLocation Exp)
    deriving (Eq, Show)

-- | Expression
data Exp
    = ExpInfix (WithLocation InfixExp) -- ^ Nested InfixExpression
    | ExpTyped (WithLocation Exp)
               [WithLocation Constraint]
               (WithLocation Type) -- ^ Expression with an explicitly specified type
    | ExpAbstraction (NonEmpty (WithLocation Pattern))
                     (WithLocation Exp) -- ^ Lambda-abstraction
    | ExpLet [WithLocation Assignment]
             (WithLocation Exp) -- ^ Let-abstraction
    | ExpCase (WithLocation Exp)
              (NonEmpty (WithLocation Alt)) -- ^ Case expression
    | ExpDo [WithLocation Stmt]
            (WithLocation Exp) -- ^ Do statement
    | ExpApplication (WithLocation Exp)
                     (NonEmpty (WithLocation Exp)) -- ^ Application of expressions
    | ExpVar (WithLocation UserDefinedIdent) -- ^ Variable
    | ExpConstr (WithLocation UserDefinedIdent) -- ^ Constructor
    | ExpConst (WithLocation Const) -- ^ Constant
    | ExpListCompr (WithLocation Exp)
                   (NonEmpty (WithLocation Stmt)) -- ^ List comprehension
    | ExpLeftSection (WithLocation Exp)
                     (WithLocation Exp) -- ^ Left section
    | ExpRightSection (WithLocation Exp)
                      (WithLocation Exp) -- ^ Right section
    | ExpRecordConstr (WithLocation UserDefinedIdent)
                      [WithLocation Binding] -- ^ Construction of a record
    | ExpRecordUpdate (WithLocation Exp)
                      (NonEmpty (WithLocation Binding)) -- ^ Update of a record
    deriving (Show, Eq)

-- | Statements in `do` blocks or in list comprehension
data Stmt
    = StmtPattern (WithLocation Pattern)
                  (WithLocation Exp)
    | StmtLet [WithLocation Assignment]
    | StmtExp (WithLocation Exp)
    deriving (Show, Eq)

-- | Record field binding
data Binding =
    Binding (WithLocation UserDefinedIdent) -- ^ Field name
            (WithLocation Exp) -- ^ Expression
    deriving (Show, Eq)

-- | Alternative in `case` expressions
data Alt
    = AltSimple (WithLocation Pattern) -- ^ Pattern
                (WithLocation Exp) -- ^ Expression
    | AltGuarded (WithLocation Pattern) -- ^ Pattern
                 (NonEmpty (WithLocation GuardedExp)) -- ^ Guarded expression
                 [WithLocation Assignment] -- ^ Where block
    deriving (Show, Eq)

-- | Expression with a guard
data GuardedExp =
    GuardedExp (NonEmpty (WithLocation Stmt)) -- ^ List of guards
               (WithLocation Exp) -- ^ Expression
    deriving (Show, Eq)
