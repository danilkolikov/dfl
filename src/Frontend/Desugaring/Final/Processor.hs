{- |
Module      :  Frontend.Desugaring.Final.Processor
Description :  Type for processor of desugaring
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Type for objects which do final desugaring.
-}
module Frontend.Desugaring.Final.Processor where

import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as E (Except, runExcept, throwE)
import qualified Control.Monad.Trans.State as S (StateT, get, modify, runStateT)
import qualified Data.HashMap.Lazy as HM

import Frontend.Desugaring.Final.Ast hiding (getDataTypeConstructors)
import Frontend.Syntax.Position (WithLocation(..), withDummyLocation)

-- | Errors which may happen during desugaring
data DesugaringError =
    DesugaringErrorNameConflict (WithLocation Ident)
                                (WithLocation Ident) -- ^ Collision between two defined names
    deriving (Show, Eq)

-- | Type for a context of defined names
type DefinedNames = HM.HashMap Ident (WithLocation Ident)

-- | State of the desugaring processor
data DesugaringState = DesugaringState
    { getDefinedTypeNames :: DefinedNames -- ^ Defined type names (data types, new types, type synonyms)
    , getDefinedClassNames :: DefinedNames -- ^ Defined class names
    , getDefinedFunctionNames :: DefinedNames -- ^ Defined functions
    , getDataTypeFields :: HM.HashMap Ident DataType -- ^ Map from the name of a field to a corresponding data type
    , getDataTypeConstructors :: HM.HashMap Ident DataType -- ^ Map from the name of a constructor to a corresponding data type
    , getCurrentIdentCounter :: Int -- ^ Counter of generated names
    } deriving (Show, Eq)

-- | Empty state of desugaring processor
emptyDesugaringState :: DesugaringState
emptyDesugaringState =
    DesugaringState
        { getDefinedTypeNames = HM.empty
        , getDefinedClassNames = HM.empty
        , getDefinedFunctionNames = HM.empty
        , getDataTypeFields = HM.empty
        , getDataTypeConstructors = HM.empty
        , getCurrentIdentCounter = 0
        }

-- | Type for objects which do desugaring
type DesugaringProcessor a
     = S.StateT DesugaringState (E.Except DesugaringError) a

-- | Run desugaring
runDesugaringProcessor ::
       DesugaringProcessor a
    -> DesugaringState
    -> Either DesugaringError (a, DesugaringState)
runDesugaringProcessor dp st = E.runExcept (S.runStateT dp st)

-- | Define a name
defineName ::
       (DesugaringState -> DefinedNames) -- ^ Getter of a context of defined names
    -> (DefinedNames -> DesugaringState -> DesugaringState) -- ^ Setter of a new context of defined names
    -> WithLocation Ident -- ^ New name
    -> DesugaringProcessor ()
defineName getNames setNames name = do
    state <- S.get
    let definedNames = getNames state
    case HM.lookup (getValue name) definedNames of
        Just found -> lift . E.throwE $ DesugaringErrorNameConflict name found
        Nothing ->
            S.modify . setNames $ HM.insert (getValue name) name definedNames

-- | Define a type name
defineTypeName :: WithLocation Ident -> DesugaringProcessor ()
defineTypeName =
    defineName getDefinedTypeNames (\names s -> s {getDefinedTypeNames = names})

-- | Define a class name
defineClassName :: WithLocation Ident -> DesugaringProcessor ()
defineClassName =
    defineName
        getDefinedClassNames
        (\names s -> s {getDefinedClassNames = names})

-- | Define a function name
defineFunctionName :: WithLocation Ident -> DesugaringProcessor ()
defineFunctionName =
    defineName
        getDefinedFunctionNames
        (\names s -> s {getDefinedFunctionNames = names})

-- | Define a field of a data type
defineDataTypeField :: WithLocation Ident -> DataType -> DesugaringProcessor ()
defineDataTypeField name dataType = do
    defineFunctionName name
    state <- S.get
    let fields = getDataTypeFields state
        newFields = HM.insert (getValue name) dataType fields
    S.modify $ \s -> s {getDataTypeFields = newFields}

-- | Define a constructor of a data type
defineDataTypeConstructor ::
       WithLocation Ident -> DataType -> DesugaringProcessor ()
defineDataTypeConstructor name dataType = do
    state <- S.get
    let constructors = getDataTypeConstructors state
        newConstructors = HM.insert (getValue name) dataType constructors
    S.modify $ \s -> s {getDataTypeConstructors = newConstructors}

-- | Generate new identifier
generateNewIdent :: DesugaringProcessor Ident
generateNewIdent = do
    state <- S.get
    let counter = getCurrentIdentCounter state
    S.modify $ \s -> s {getCurrentIdentCounter = counter + 1}
    return . IdentGenerated $ counter

-- | Generate new identifier with dummy location
generateNewIdent' :: DesugaringProcessor (WithLocation Ident)
generateNewIdent' = withDummyLocation <$> generateNewIdent

-- | Collect a hash map of objects, returned by processors
collectHashMap ::
       (a -> DesugaringProcessor (Maybe (Ident, b)))
    -> [WithLocation a]
    -> DesugaringProcessor (HM.HashMap Ident b)
collectHashMap _ [] = return HM.empty
collectHashMap get (f:rest) = do
    cur <- get . getValue $ f
    hashMap <- collectHashMap get rest
    return $
        case cur of
            Just (key, val) -> HM.insert key val hashMap
            Nothing -> hashMap
