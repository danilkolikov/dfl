{- |
Module      :  Frontend.Desugaring.Grouping.Base
Description :  Base functions for expression grouping
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Type for objects which group expressions.
-}
module Frontend.Desugaring.Grouping.Base where

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans.State (StateT, get, modify, runStateT)
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust, isNothing)

import Frontend.Desugaring.Grouping.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Util.Debug

-- | Errors which may happen during desugaring
data GroupingProcessorError
    = GroupingProcessorErrorNameConflict (WithLocation Ident)
                                         (WithLocation Ident) -- ^ Collision between two defined names
    | GroupingProcessorErrorDuplicatedTypeDeclaration (WithLocation Ident) -- ^ Expression has multiple type declarations
    | GroupingProcessorErrorDuplicatedFixityDeclaration (WithLocation Ident) -- ^ Expression has multiple fixity declarations
    | GroupingProcessorErrorMissingExpressionDefinition (WithLocation Ident) -- ^ Definition of an expression is missing
    | GroupingProcessorErrorMissingMethodType (WithLocation Ident) -- ^ Definition of a method is missing a tupe signature
    | GroupingProcessorErrorDifferentNumberOfArguments (WithLocation Ident) -- ^ Declarations of a function have differnt number of arguments
    | GroupingProcessorErrorUnexpectedFixitySignature (WithLocation Ident) -- ^ Fixity signature without an actual declaration
    deriving (Show, Eq)

-- | Type for a context of defined names
type DefinedNames = HM.HashMap Ident (WithLocation Ident)

-- | State of the desugaring processor
data GroupingProcessorState = GroupingProcessorState
    { getGroupingProcessorStateTypes :: DefinedNames -- ^ Defined type names (data types, new types, type synonyms)
    , getGroupingProcessorStateExpressions :: DefinedNames -- ^ Defined expressions (fields, methods, functions)
    } deriving (Show, Eq)

instance Semigroup GroupingProcessorState where
    GroupingProcessorState t1 e1 <> GroupingProcessorState t2 e2 =
        GroupingProcessorState (t1 <> t2) (e1 <> e2)

instance Monoid GroupingProcessorState where
    mempty = GroupingProcessorState mempty mempty

-- | An environment of the grouping processor
type GroupingProcessorEnvironment = Ident

-- | A debug output of grouping
data GroupingDebugOutput = GroupingDebugOutput
    { getGroupingDebugOutputState :: Maybe GroupingProcessorState
    , getGroupingDebugOutputTypeSynonyms :: Maybe TypeSynonyms
    , getGroupingDebugOutputDataTypes :: Maybe DataTypes
    , getGroupingDebugOutputClassses :: Maybe (Classes Exp)
    , getGroupingDebugOutputInstances :: Maybe (Instances Exp)
    , getGroupingDebugOutputFunctions :: Maybe (Expressions Exp)
    } deriving (Eq, Show)

instance Semigroup GroupingDebugOutput where
    a <> b
        | GroupingDebugOutput { getGroupingDebugOutputState = st1
                              , getGroupingDebugOutputTypeSynonyms = t1
                              , getGroupingDebugOutputDataTypes = d1
                              , getGroupingDebugOutputClassses = c1
                              , getGroupingDebugOutputInstances = i1
                              , getGroupingDebugOutputFunctions = f1
                              } <- a
        , GroupingDebugOutput { getGroupingDebugOutputState = st2
                              , getGroupingDebugOutputTypeSynonyms = t2
                              , getGroupingDebugOutputDataTypes = d2
                              , getGroupingDebugOutputClassses = c2
                              , getGroupingDebugOutputInstances = i2
                              , getGroupingDebugOutputFunctions = f2
                              } <- b =
            GroupingDebugOutput
                { getGroupingDebugOutputState = st1 <> st2
                , getGroupingDebugOutputTypeSynonyms = t1 <> t2
                , getGroupingDebugOutputDataTypes = d1 <> d2
                , getGroupingDebugOutputClassses = c1 <> c2
                , getGroupingDebugOutputInstances = i1 <> i2
                , getGroupingDebugOutputFunctions = f1 <> f2
                }

instance Monoid GroupingDebugOutput where
    mempty =
        GroupingDebugOutput
            { getGroupingDebugOutputState = mempty
            , getGroupingDebugOutputTypeSynonyms = mempty
            , getGroupingDebugOutputDataTypes = mempty
            , getGroupingDebugOutputClassses = mempty
            , getGroupingDebugOutputInstances = mempty
            , getGroupingDebugOutputFunctions = mempty
            }

-- | Type for objects which do desugaring
type GroupingProcessor
     = ReaderT GroupingProcessorEnvironment (StateT GroupingProcessorState (WithDebugOutput GroupingProcessorError GroupingDebugOutput))

-- | Run desugaring
runGroupingProcessor ::
       GroupingProcessorEnvironment
    -> GroupingProcessor a
    -> ( Either GroupingProcessorError (a, GroupingProcessorState)
       , GroupingDebugOutput)
runGroupingProcessor env gp =
    runWithDebugOutput $ runStateT (runReaderT gp env) mempty

-- | Lifts WithDebugOutput to GroupingProcessor
liftWDO ::
       WithDebugOutput GroupingProcessorError GroupingDebugOutput a
    -> GroupingProcessor a
liftWDO = lift . lift

-- | Raises a grouping error
raiseGroupingError :: GroupingProcessorError -> GroupingProcessor a
raiseGroupingError = liftWDO . raiseError

-- | Writes a debug output
writeGroupingDebugOutput :: GroupingDebugOutput -> GroupingProcessor ()
writeGroupingDebugOutput = liftWDO . writeDebugOutput

-- | Defines a name
defineName ::
       (GroupingProcessorState -> DefinedNames) -- ^ Getter of a context of defined names
    -> (DefinedNames -> GroupingProcessorState) -- ^ Setter of a new context of defined names
    -> WithLocation Ident -- ^ New name
    -> GroupingProcessor ()
defineName getNames setNames name = do
    groupingState <- lift get
    let definedName = HM.lookup (getValue name) $ getNames groupingState
    unless (isNothing definedName) . raiseGroupingError $
        GroupingProcessorErrorNameConflict name (fromJust definedName)
    -- Save both qualified and non-qualified names
    moduleName <- ask
    let qualifiedName = makeQualifiedName moduleName (getValue name)
        newNames = HM.fromList [(getValue name, name), (qualifiedName, name)]
        newState = setNames newNames
     in do lift . modify $ \state -> state <> newState
           liftWDO $
               writeDebugOutput
                   mempty {getGroupingDebugOutputState = Just newState}

-- | Define a type name
defineTypeName :: WithLocation Ident -> GroupingProcessor ()
defineTypeName =
    defineName
        getGroupingProcessorStateTypes
        (\names -> mempty {getGroupingProcessorStateTypes = names})

-- | Define an expression name
defineExpressionName :: WithLocation Ident -> GroupingProcessor ()
defineExpressionName =
    defineName
        getGroupingProcessorStateExpressions
        (\names -> mempty {getGroupingProcessorStateExpressions = names})

-- | Collect a hash map of objects, returned by processors
collectHashMap ::
       (a -> GroupingProcessor (Maybe (Ident, b)))
    -> [WithLocation a]
    -> GroupingProcessor (HM.HashMap Ident b)
collectHashMap _ [] = return HM.empty
collectHashMap getter (f:rest) = do
    cur <- getter . getValue $ f
    hashMap <- collectHashMap getter rest
    return $
        case cur of
            Just (key, val) -> HM.insert key val hashMap
            Nothing -> hashMap

-- | Collects a list of objects, returnred by processors
collectList ::
       (a -> GroupingProcessor (Maybe b))
    -> [WithLocation a]
    -> GroupingProcessor [b]
collectList _ [] = return []
collectList getter (f:rest) = do
    cur <- getter . getValue $ f
    list <- collectList getter rest
    return $
        case cur of
            Just val -> val : list
            Nothing -> list

-- | Adds a qualifier to an ident
makeQualifiedName :: Ident -> Ident -> Ident
makeQualifiedName moduleName ident
    | IdentGenerated {} <- ident = ident -- Generated names can't be qualified
    | IdentGenerated {} <- moduleName =
        error "Generated modules are not supported"
    | IdentUserDefined moduleName' <- moduleName
    , IdentUserDefined ident' <- ident =
        IdentUserDefined $
        case ident' of
            IdentQualified {} -> ident' -- Already qualified
            IdentSimple simpleIdent ->
                let getModulePath innerIdent =
                        case innerIdent of
                            IdentNamed name -> [name]
                            IdentParametrised {} ->
                                error "Parametrised modules are not supported"
                    qualifier =
                        case moduleName' of
                            IdentQualified path innerIdent ->
                                path ++ getModulePath innerIdent
                            IdentSimple innerIdent -> getModulePath innerIdent
                 in IdentQualified qualifier simpleIdent

-- | Group of assignments
data AssignmentsGroup =
    AssignmentsGroup (WithLocation Ident)
                     (NE.NonEmpty ( NE.NonEmpty (WithLocation Pattern)
                                  , WithLocation Exp))

-- | Output of the function "groupAssignments"
data GroupedAssignments = GroupedAssignments
    { getGroupedAssignmentsGroups :: HM.HashMap Ident AssignmentsGroup
    , getGroupedAssignmentsPatterns :: [(WithLocation Pattern, WithLocation Exp)]
    , getGroupedAssignmentsTypes :: [(WithLocation Ident, TypeSignature)]
    , getGroupedAssignmentsFixities :: [(WithLocation Ident, FixitySignature)]
    }

instance Semigroup GroupedAssignments where
    GroupedAssignments g1 p1 t1 f1 <> GroupedAssignments g2 p2 t2 f2 =
        GroupedAssignments (g1 <> g2) (p1 <> p2) (t1 <> t2) (f1 <> f2)

instance Monoid GroupedAssignments where
    mempty = GroupedAssignments mempty mempty mempty mempty
