{- |
Module      :  Frontend.Inference.Kind.WithDependencies
Description :  Functions for dependency resolutions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for resolution of dependencies of types
-}
module Frontend.Inference.Kind.WithDependencies where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast
import Frontend.Inference.DependencyResolver (Dependencies, DependencyGraph)
import Frontend.Inference.Kind.Environment
import Frontend.Syntax.Position (WithLocation(..))

-- | Class for types which have dependencies
class WithDependencies a where
    getName :: a -> Ident -- ^ Get name of an object
    getDependencies :: a -> Dependencies -- ^ Get its dependencies
    getDependencyGraph :: [a] -> DependencyGraph -- ^ Construct a dependency graph
    getDependencyGraph =
        HM.fromList . map (\x -> (getName x, getDependencies x))

-- | Get dependency graph of a module
getModuleDependencyGraph :: Environment -> DependencyGraph
getModuleDependencyGraph Environment { getTypeSynonyms = typeSynonyms
                                     , getDataTypes = dataTypes
                                     , getClasses = classes
                                     } =
    let synonymGraph = getDependencyGraph $ HM.elems typeSynonyms
        dataTypeGraph = getDependencyGraph $ HM.elems dataTypes
        classGraph = getDependencyGraph $ HM.elems classes
     in HM.unions [synonymGraph, dataTypeGraph, classGraph]

instance WithDependencies TypeSynonym where
    getName = getValue . getTypeSynonymName
    getDependencies = getTypeDependencies . getValue . getTypeSynonymType

instance WithDependencies DataType where
    getName = getValue . getDataTypeName
    getDependencies DataType { getDataTypeContext = constraints
                             , getDataTypeConstructors = constructors
                             } =
        let dependencies =
                map (getConstraintDependencies . getValue) constraints ++
                map (getConstructorDependencies . snd) constructors
         in HS.unions dependencies

instance WithDependencies Class where
    getName = getValue . getClassName
    getDependencies Class { getClassContext = constraints
                          , getClassMethods = methods
                          } =
        let dependencies =
                map (getSimpleConstraintDependencies . getValue) constraints ++
                map
                    (getTypeSignatureDependencies . getMethodType)
                    (HM.elems methods)
         in HS.unions dependencies

-- | Get dependencies of a type
getTypeDependencies :: Type -> Dependencies
getTypeDependencies TypeVar {} = HS.empty
getTypeDependencies (TypeConstr name) = HS.singleton (getValue name)
getTypeDependencies (TypeFunction from to) =
    getTypeDependencies (getValue from) `HS.union`
    getTypeDependencies (getValue to)
getTypeDependencies (TypeApplication func args) =
    let types = func : NE.toList args
        dependencies = map (getTypeDependencies . getValue) types
     in HS.unions dependencies

-- | Get dependencies of a constructor
getConstructorDependencies :: Constructor -> Dependencies
getConstructorDependencies c =
    let types = getConstructorArgs c
        dependencies = map (getTypeDependencies . getValue) types
     in HS.unions dependencies

-- | Get dependencies of a constraint
getConstraintDependencies :: Constraint -> Dependencies
getConstraintDependencies (ConstraintParam className _) =
    HS.singleton (getValue className)
getConstraintDependencies (ConstraintAppliedParam className _ _) =
    HS.fromList $ map getValue [className]

-- | Get dependencies of a simple constraint
getSimpleConstraintDependencies :: SimpleConstraint -> Dependencies
getSimpleConstraintDependencies (SimpleConstraint className _) =
    HS.singleton $ getValue className

-- | Get dependencies of a type signature
getTypeSignatureDependencies :: TypeSignature -> Dependencies
getTypeSignatureDependencies (TypeSignature context type') =
    let contextDependencies = map (getConstraintDependencies . getValue) context
        typeDependencies = getTypeDependencies $ getValue type'
     in HS.unions $ typeDependencies : contextDependencies
