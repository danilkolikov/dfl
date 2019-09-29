{- |
Module      :  Frontend.Syntax.EntityName
Description :  Predefined names of entities
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Set of predefined names of entities
-}
module Frontend.Syntax.EntityName where

import Frontend.Syntax.Token (TokenT)

-- | Name of an entity - function or operator
type EntityName = [TokenT]

-- Module
-- | Default name of a module
dEFAULT_MODULE_NAME :: EntityName
dEFAULT_MODULE_NAME = ["Main"]

-- Type constructors
-- | Constructor of the unit type
uNIT_NAME :: EntityName
uNIT_NAME = ["()"]

-- | Constructor of the list type
lIST_NAME :: EntityName
lIST_NAME = ["[]"]

-- | Constructor of the function type
fUNCTION_NAME :: EntityName
fUNCTION_NAME = ["->"]

-- | Constructor of the tuple type
tUPLE_NAME :: EntityName
tUPLE_NAME = ["(,)"]

-- | Int type
iNT_NAME :: EntityName
iNT_NAME = ["Int"]

-- | Float type
fLOAT_NAME :: EntityName
fLOAT_NAME = ["Float"]

-- | Char type
cHAR_NAME :: EntityName
cHAR_NAME = ["Char"]

-- | String type
sTRING_NAME :: EntityName
sTRING_NAME = ["String"]

-- | Bool type
bOOL_NAME :: EntityName
bOOL_NAME = ["Bool"]

-- Classes
-- | Monad class
mONAD_NAME :: EntityName
mONAD_NAME = ["Monad"]

-- | Eq class
eQ_NAME :: EntityName
eQ_NAME = ["Eq"]

-- | Num class
nUM_NAME :: EntityName
nUM_NAME = ["Num"]

-- | Enum class
eNUM_NAME :: EntityName
eNUM_NAME = ["Enum"]

-- | Kind symbol
sTAR_NAME :: EntityName
sTAR_NAME = ["*"]

-- Operators
-- | Operator ":"
cOLON_NAME :: EntityName
cOLON_NAME = [":"]

-- | Operator ">>"
iGNORING_BIND_NAME :: EntityName
iGNORING_BIND_NAME = [">>"]

-- | Operator ">>="
bIND_NAME :: EntityName
bIND_NAME = [">>="]

-- | Operator "=="
eQUAL_NAME :: EntityName
eQUAL_NAME = ["=="]

-- | Operator "$"
aPPLICATION_NAME :: EntityName
aPPLICATION_NAME = ["$"]

-- Functions
-- | Function "main"
mAIN_NAME :: EntityName
mAIN_NAME = ["main"]

-- | Function "negate"
nEGATE_NAME :: EntityName
nEGATE_NAME = ["negate"]

-- | Function "undefined"
uNDEFINED_NAME :: EntityName
uNDEFINED_NAME = ["undefined"]

-- | Function "fail"
fAIL_NAME :: EntityName
fAIL_NAME = ["fail"]

-- | Function "enumFrom"
eNUM_FROM_NAME :: EntityName
eNUM_FROM_NAME = ["enumFrom"]

-- | Function "enumFromThen"
eNUM_FROM_THEN_NAME :: EntityName
eNUM_FROM_THEN_NAME = ["enumFromThen"]

-- | Function "enumFromTo"
eNUM_FROM_TO_NAME :: EntityName
eNUM_FROM_TO_NAME = ["enumFromTo"]

-- | Function "enumFromThenTo"
eNUM_FROM_THEN_TO_NAME :: EntityName
eNUM_FROM_THEN_TO_NAME = ["enumFromThenTo"]

-- | Function "concatMap"
cONCAT_MAP_NAME :: EntityName
cONCAT_MAP_NAME = ["concatMap"]

-- Constructors
-- | Constructor "True"
tRUE_NAME :: EntityName
tRUE_NAME = ["True"]

-- | Constructor "False"
fALSE_NAME :: EntityName
fALSE_NAME = ["False"]
