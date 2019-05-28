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

import Frontend.Desugaring.Final.Ast
import Frontend.Syntax.Position (WithLocation(..))

-- | Errors which may happen during desugaring
data DesugaringError =
    DesugaringErrorNameConflict (WithLocation Ident)
                                (WithLocation Ident) -- ^ Collision between two defined names

-- | Type for a context of defined names
type DefinedNames = HM.HashMap Ident (WithLocation Ident)

-- | State of the desugaring processor
data DesugaringState = DesugaringState
    { getDefinedTypeNames :: DefinedNames -- ^ Defined type names (data types, new types, type synonyms)
    , getDefinedClassNames :: DefinedNames -- ^ Defined class names
    , getDefinedFunctionNames :: DefinedNames -- ^ Defined functions
    , getDataTypeFields :: HM.HashMap Ident DataType -- ^ Map from the name of a field to a corresponding data type
    , getNewTypeFields :: HM.HashMap Ident NewType -- ^ Map from the name of a field to a corresponding new type
    , getCurrentIdentCounter :: Int -- ^ Counter of generated names
    }

-- | Create an empty state, based on a provided counter of generated names
createEmptyDesugaringState :: Int -> DesugaringState
createEmptyDesugaringState pos =
    DesugaringState
        { getDefinedTypeNames = HM.empty
        , getDefinedClassNames = HM.empty
        , getDefinedFunctionNames = HM.empty
        , getDataTypeFields = HM.empty
        , getNewTypeFields = HM.empty
        , getCurrentIdentCounter = pos
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

-- | Define a field
defineField ::
       (DesugaringState -> HM.HashMap Ident a) -- ^ Getter of a field context
    -> (HM.HashMap Ident a -> DesugaringState -> DesugaringState) -- ^ Setter of a field context
    -> WithLocation Ident -- ^ Name of a field
    -> a -- ^ Corresponding constructor
    -> DesugaringProcessor ()
defineField getFields setFields name field = do
    defineFunctionName name
    state <- S.get
    let fields = getFields state
    S.modify . setFields $ HM.insert (getValue name) field fields

-- | Set field of a data type
defineDataTypeField :: WithLocation Ident -> DataType -> DesugaringProcessor ()
defineDataTypeField =
    defineField getDataTypeFields (\fields s -> s {getDataTypeFields = fields})

-- | Set field of a new type
defineNewTypeField :: WithLocation Ident -> NewType -> DesugaringProcessor ()
defineNewTypeField =
    defineField getNewTypeFields (\fields s -> s {getNewTypeFields = fields})

-- | Generate new identifier
generateNewIdent :: DesugaringProcessor Ident
generateNewIdent = do
    state <- S.get
    let counter = getCurrentIdentCounter state
    S.modify $ \s -> s {getCurrentIdentCounter = counter + 1}
    return . IdentGenerated $ counter + 1
