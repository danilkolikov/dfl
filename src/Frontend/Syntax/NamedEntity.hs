{- |
Module      :  Frontend.Syntax.NamedEntity
Description :  Getter of names of entities
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions which get names of entities, either functions or operators.
-}
module Frontend.Syntax.NamedEntity
    ( EntityName
    , NamedEntity(..)
    , cOLON_NAME
    ) where

import Frontend.Syntax.Ast
    ( FuncLabel(..)
    , GConSym(..)
    , OpLabel(..)
    , Qualified(..)
    )
import Frontend.Syntax.EntityName (EntityName, cOLON_NAME)
import Frontend.Syntax.Token (ConId(..), ConSym(..), VarId(..), VarSym(..))

-- | Class for types which represent named entities
class NamedEntity a where
    getEntityName :: a -> EntityName -- ^ Get name of an entity

instance (NamedEntity a, NamedEntity b) => NamedEntity (Either a b) where
    getEntityName = either getEntityName getEntityName

instance NamedEntity ConId where
    getEntityName (ConId s) = [s]

instance NamedEntity ConSym where
    getEntityName (ConSym s) = [s]

instance NamedEntity VarId where
    getEntityName (VarId s) = [s]

instance NamedEntity VarSym where
    getEntityName (VarSym s) = [s]

instance (NamedEntity a) => NamedEntity (Qualified a) where
    getEntityName (Qualified path s) =
        concatMap getEntityName path ++ getEntityName s

instance NamedEntity GConSym where
    getEntityName GConSymColon = cOLON_NAME
    getEntityName (GConSymOp s) = getEntityName s

instance (NamedEntity a, NamedEntity b) => NamedEntity (OpLabel a b) where
    getEntityName (OpLabelSym s) = getEntityName s
    getEntityName (OpLabelId s) = getEntityName s

instance (NamedEntity a, NamedEntity b) => NamedEntity (FuncLabel a b) where
    getEntityName (FuncLabelId s) = getEntityName s
    getEntityName (FuncLabelSym s) = getEntityName s
