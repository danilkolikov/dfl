{- |
Module      :  Frontend.Desugaring.Fixity.Ast
Description :  Version of AST with resolved fixity operators
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Version of AST with resolved fixity operators
-}
module Frontend.Desugaring.Fixity.Ast
    ( module Frontend.Desugaring.Fixity.Ast
    , module Frontend.Desugaring.Ast
    ) where

import Data.List.NonEmpty (NonEmpty)

import Core.Ident (Ident)
import Frontend.Desugaring.Ast
import Frontend.Syntax.Position (WithLocation)

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
    = ExpTyped (WithLocation Exp)
               [WithLocation Constraint]
               (WithLocation Type) -- ^ Expression with an explicitly specified type
    | ExpAbstraction (NonEmpty (WithLocation Pattern))
                     (WithLocation Exp) -- ^ Lambda-abstraction
    | ExpLet (Expressions Exp)
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
    | ExpRecordConstr (WithLocation Ident)
                      [WithLocation Binding] -- ^ Construction of a record
    | ExpRecordUpdate (WithLocation Exp)
                      (NonEmpty (WithLocation Binding)) -- ^ Update of a record
    deriving (Show, Eq)

-- | Statements in `do` blocks or in list comprehension
data Stmt
    = StmtPattern (WithLocation Pattern)
                  (WithLocation Exp)
    | StmtLet (Expressions Exp)
    | StmtExp (WithLocation Exp)
    deriving (Show, Eq)

-- | Record field binding
data Binding =
    Binding (WithLocation Ident) -- ^ Field name
            (WithLocation Exp) -- ^ Expression
    deriving (Show, Eq)

-- | Alternative in `case` expressions
data Alt
    = AltSimple (WithLocation Pattern) -- ^ Pattern
                (WithLocation Exp) -- ^ Expression
    | AltGuarded (WithLocation Pattern) -- ^ Pattern
                 (NonEmpty (WithLocation GuardedExp)) -- ^ Guarded expression
                 (Expressions Exp) -- ^ Where block
    deriving (Show, Eq)

-- | Expression with a guard
data GuardedExp =
    GuardedExp (NonEmpty (WithLocation Stmt)) -- ^ List of guards
               (WithLocation Exp) -- ^ Expression
    deriving (Show, Eq)
