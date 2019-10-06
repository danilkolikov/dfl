{- |
Module      :  Core.PredefinedIdents
Description :  Predefined identifiers
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Set of predefined identifiers
-}
module Core.PredefinedIdents
    ( UserDefinedIdent
    , dEFAULT_MODULE
    , pRELUDE_MODULE
    , uNIT
    , lIST
    , fUNCTION
    , tUPLE
    , iNT
    , fLOAT
    , cHAR
    , sTRING
    , bOOL
    , mONAD
    , eQ
    , nUM
    , eNUM
    , sTAR
    , cOLON
    , mINUS
    , iGNORING_BIND
    , bIND
    , eQUAL
    , aPPLICATION
    , mAIN
    , nEGATE
    , uNDEFINED
    , fAIL
    , eNUM_FROM
    , eNUM_FROM_THEN
    , eNUM_FROM_TO
    , eNUM_FROM_THEN_TO
    , cONCAT_MAP
    , tRUE
    , fALSE
    ) where

import Core.Ident

-- | Makes a simple ident
makeSimpleIdent :: IdentName -> UserDefinedIdent
makeSimpleIdent = IdentSimple . IdentNamed

-- | Makes a qualified ident
makeModuleIdent :: IdentName -> IdentName -> UserDefinedIdent
makeModuleIdent moduleName = IdentQualified [moduleName] . IdentNamed

-- Modules
-- | Name of the defautl module
dEFAULT_MODULE_NAME :: IdentName
dEFAULT_MODULE_NAME = "Main"

-- | Default name of a module
dEFAULT_MODULE :: UserDefinedIdent
dEFAULT_MODULE = makeSimpleIdent dEFAULT_MODULE_NAME

-- | Name of the Prelude
pRELUDE_MODULE_NAME :: IdentName
pRELUDE_MODULE_NAME = "Prelude"

-- | Prelude module
pRELUDE_MODULE :: UserDefinedIdent
pRELUDE_MODULE = makeSimpleIdent pRELUDE_MODULE_NAME

-- | Makes a Prelude ident
makePreludeIdent :: IdentName -> UserDefinedIdent
makePreludeIdent = makeModuleIdent pRELUDE_MODULE_NAME

-- Type constructors
-- | Constructor of the unit type
uNIT :: UserDefinedIdent
uNIT = makePreludeIdent "()"

-- | Constructor of the list type
lIST :: UserDefinedIdent
lIST = makePreludeIdent "[]"

-- | Constructor of the function type
fUNCTION :: UserDefinedIdent
fUNCTION = makePreludeIdent "->"

-- | Constructor of the tuple type
tUPLE :: Int -> UserDefinedIdent
tUPLE = IdentQualified [pRELUDE_MODULE_NAME] . IdentParametrised "(,)"

-- | Int type
iNT :: UserDefinedIdent
iNT = makePreludeIdent "Int"

-- | Float type
fLOAT :: UserDefinedIdent
fLOAT = makePreludeIdent "Float"

-- | Char type
cHAR :: UserDefinedIdent
cHAR = makePreludeIdent "Char"

-- | String type
sTRING :: UserDefinedIdent
sTRING = makePreludeIdent "String"

-- | Bool type
bOOL :: UserDefinedIdent
bOOL = makePreludeIdent "Bool"

-- Classes
-- | Monad class
mONAD :: UserDefinedIdent
mONAD = makePreludeIdent "Monad"

-- | Eq class
eQ :: UserDefinedIdent
eQ = makePreludeIdent "Eq"

-- | Num class
nUM :: UserDefinedIdent
nUM = makePreludeIdent "Num"

-- | Enum class
eNUM :: UserDefinedIdent
eNUM = makePreludeIdent "Enum"

-- | Kind symbol
sTAR :: UserDefinedIdent
sTAR = makeSimpleIdent "*"

-- Operators
-- | Operator ":"
cOLON :: UserDefinedIdent
cOLON = makePreludeIdent ":"

-- | Operator "-"
mINUS :: UserDefinedIdent
mINUS = makeSimpleIdent "-"

-- | Operator ">>"
iGNORING_BIND :: UserDefinedIdent
iGNORING_BIND = makePreludeIdent ">>"

-- | Operator ">>="
bIND :: UserDefinedIdent
bIND = makePreludeIdent ">>="

-- | Operator "=="
eQUAL :: UserDefinedIdent
eQUAL = makePreludeIdent "=="

-- | Operator "$"
aPPLICATION :: UserDefinedIdent
aPPLICATION = makePreludeIdent "$"

-- Functions
-- | Function "main"
mAIN :: UserDefinedIdent
mAIN = makeModuleIdent dEFAULT_MODULE_NAME "main"

-- | Function "negate"
nEGATE :: UserDefinedIdent
nEGATE = makePreludeIdent "negate"

-- | Function "undefined"
uNDEFINED :: UserDefinedIdent
uNDEFINED = makePreludeIdent "undefined"

-- | Function "fail"
fAIL :: UserDefinedIdent
fAIL = makePreludeIdent "fail"

-- | Function "enumFrom"
eNUM_FROM :: UserDefinedIdent
eNUM_FROM = makePreludeIdent "enumFrom"

-- | Function "enumFromThen"
eNUM_FROM_THEN :: UserDefinedIdent
eNUM_FROM_THEN = makePreludeIdent "enumFromThen"

-- | Function "enumFromTo"
eNUM_FROM_TO :: UserDefinedIdent
eNUM_FROM_TO = makePreludeIdent "enumFromTo"

-- | Function "enumFromThenTo"
eNUM_FROM_THEN_TO :: UserDefinedIdent
eNUM_FROM_THEN_TO = makePreludeIdent "enumFromThenTo"

-- | Function "concatMap"
cONCAT_MAP :: UserDefinedIdent
cONCAT_MAP = makePreludeIdent "concatMap"

-- Constructors
-- | Constructor "True"
tRUE :: UserDefinedIdent
tRUE = makePreludeIdent "True"

-- | Constructor "False"
fALSE :: UserDefinedIdent
fALSE = makePreludeIdent "False"
