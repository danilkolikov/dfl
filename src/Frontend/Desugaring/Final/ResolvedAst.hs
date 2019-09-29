{- |
Module      :  Frontend.Desugaring.Final.ResolvedAst
Description :  AST with resolved records
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Version of AST with resolved record patterns, construction and update
-}
module Frontend.Desugaring.Final.ResolvedAst
    ( Ident(..)
    , Const(..)
    , ClassAssignment(..)
    , Assignment(..)
    , InstAssignment(..)
    , Type(..)
    , Exp(..)
    , Pattern(..)
    , Alt(..)
    , GuardedExp(..)
    , Stmt(..)
    ) where

import Data.List.NonEmpty (NonEmpty)

import Frontend.Desugaring.Initial.Ast
    ( Const(..)
    , Constraint(..)
    , Fixity(..)
    , Ident(..)
    , Type(..)
    )
import Frontend.Syntax.Position (WithLocation)

-- | Assignment in top-level, `let` or `where` blocks
data Assignment
    = AssignmentName (WithLocation Ident)
                     (NonEmpty (WithLocation Pattern))
                     (WithLocation Exp) -- ^ Assign some expression to a name
    | AssignmentPattern (WithLocation Pattern)
                        (WithLocation Exp) -- ^ Assign some expression to a pattern
    | AssignmentType (WithLocation Ident)
                     [WithLocation Constraint]
                     (WithLocation Type) -- ^ Define type of a name
    | AssignmentFixity (WithLocation Ident)
                       Fixity
                       Int -- ^ Defines fixity of an operator
    deriving (Show, Eq)

-- | Assignment in a class definition
data ClassAssignment
    = ClassAssignmentName (WithLocation Ident)
                          [WithLocation Pattern]
                          (WithLocation Exp) -- ^ Assign expression to a name
    | ClassAssignmentType (WithLocation Ident)
                          [WithLocation Constraint]
                          (WithLocation Type) -- ^ Define type of an expression
    | ClassAssignmentFixity (WithLocation Ident)
                            Fixity
                            Int -- ^ Defines fixity of an operator
    deriving (Show, Eq)

-- | Assignemnt in an instance definition
data InstAssignment =
    InstAssignmentName (WithLocation Ident)
                       [WithLocation Pattern]
                       (WithLocation Exp) -- ^ Assign expression to a name
    deriving (Show, Eq)

-- | Pattern
data Pattern
    = PatternConstr (WithLocation Ident)
                    [WithLocation Pattern] -- ^ Application of a constructor
    | PatternVar (WithLocation Ident)
                 (Maybe (WithLocation Pattern)) -- ^ Variable with possible pattern
    | PatternConst (WithLocation Const) -- ^ Constant
    | PatternWildcard -- ^ Wildcard
    deriving (Show, Eq)

-- | Expression
data Exp
    = ExpTyped (WithLocation Exp)
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
    | ExpVar (WithLocation Ident) -- ^ Variable
    | ExpConstr (WithLocation Ident) -- ^ Constructor
    | ExpConst (WithLocation Const) -- ^ Constant
    | ExpListCompr (WithLocation Exp)
                   (NonEmpty (WithLocation Stmt)) -- ^ List comprehension
    | ExpLeftSection (WithLocation Exp)
                     (WithLocation Exp) -- ^ Left section
    | ExpRightSection (WithLocation Exp)
                      (WithLocation Exp) -- ^ Right section
    deriving (Show, Eq)

-- | Statements in `do` blocks or in list comprehension
data Stmt
    = StmtPattern (WithLocation Pattern)
                  (WithLocation Exp)
    | StmtLet [WithLocation Assignment]
    | StmtExp (WithLocation Exp)
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
