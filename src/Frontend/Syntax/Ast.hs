{- |
Module      :  Frontend.Syntax.Ast
Description :  Abstract Syntax Tree
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Abstract Syntax Tree of DFL. Follows the specification of
<https://www.haskell.org/onlinereport/haskell2010/haskellch10.html Haskell 2010>.
-}
module Frontend.Syntax.Ast where

import Frontend.Syntax.Token (ConId, ConSym, ModId, VarId, VarSym)

-- | Qualified name - a name, possibly prepended by the name of a module
data Qualified a =
    Qualified [ModId]
              a
    deriving (Show, Eq, Ord)

-- | Type representing names of type variables
type TyVar = VarId

-- | Type representing names of type constructors
type TyCon = ConId

-- | Type representing names of type classes
type TyCls = ConId

-- | Type representing qualified IDs of functions
type QVarId = Qualified VarId

-- | Type representing qualified IDs of constructors
type QConId = Qualified ConId

-- | Type representing qualified IDs of type constructors
type QTyCon = Qualified TyCon

-- | Type representing qualified IDs of type classes
type QTyCls = Qualified TyCls

-- | Type representing qualified user-defined operators
type QVarSym = Qualified VarSym

-- | Type representing qualified constructor operators
type QConSym = Qualified ConSym

-- | Type representing qualified IDs of modules
type QModId = Qualified ModId
