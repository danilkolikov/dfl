{- |
Module      :  Frontend.Inference.Base.Variables
Description :  Functions for handling bound variables
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for handling bound variables
-}
module Frontend.Inference.Base.Variables where

import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust)

import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Inference.Equalities hiding
    ( getTypeVariables
    )
import Frontend.Inference.Signature
import Frontend.Inference.Solver
import Frontend.Inference.Substitution

-- | A system of equalities between types, kinds and sorts of a bound variable
data TypeVariableEqualities = TypeVariableEqualities
    { getTypeVariableEqualitiesTypes :: [Type] -- ^ Equal types
    , getTypeVariableEqualitiesKinds :: [Kind] -- ^ Equal kinds
    , getTypeVariableEqualitiesSorts :: [Sort] -- ^ Equal kinds
    }

-- | Systems of equalities of bound variables
type TypeVariableEqualitiesMap = HM.HashMap Ident TypeVariableEqualities

instance Semigroup TypeVariableEqualities where
    TypeVariableEqualities t1 k1 s1 <> TypeVariableEqualities t2 k2 s2 =
        TypeVariableEqualities (t1 <> t2) (k1 <> k2) (s1 <> s2)

-- | Creates a system of equalities
createTypeVariableEqualities :: (Type, Kind, Sort) -> TypeVariableEqualities
createTypeVariableEqualities (type', kind, sort) =
    TypeVariableEqualities
        { getTypeVariableEqualitiesTypes = [type']
        , getTypeVariableEqualitiesKinds = [kind]
        , getTypeVariableEqualitiesSorts = [sort]
        }

-- | Creates a map of systems of equalities
createTypeVariableEqualitiesMap :: TypeVariables -> TypeVariableEqualitiesMap
createTypeVariableEqualitiesMap = HM.map createTypeVariableEqualities

-- | Applies solution of a system of equalites to type variables
applySolutionToTypeVariables :: Solution -> TypeVariables -> TypeVariables
applySolutionToTypeVariables Solution { getSolutionTypeSubstitution = typeSub
                                      , getSolutionKindSubstitution = kindSub
                                      , getSolutionSortSubstitution = sortSub
                                      } =
    let processSingle (type', kind, sort) =
            ( substitute typeSub type'
            , substitute kindSub kind
            , substitute sortSub sort)
     in HM.map processSingle

-- | Appends type variables to the system of equalities
appendTypeVariableEqualities ::
       TypeVariableEqualitiesMap -> TypeVariables -> TypeVariableEqualitiesMap
appendTypeVariableEqualities eqMap typeVars =
    let processSingle (name, var) equalities =
            let foundEqualities = fromJust $ HM.lookup name equalities
                newEqualities = createTypeVariableEqualities var
             in HM.insert name (foundEqualities <> newEqualities) equalities
     in foldr processSingle eqMap (HM.toList typeVars)

-- | Collects systems of equalities using provided solutions
collectTypeVariableEqualities ::
       TypeVariables -> [Solution] -> TypeVariableEqualitiesMap
collectTypeVariableEqualities typeVars solutions =
    let initial = createTypeVariableEqualitiesMap typeVars
        applied = map (`applySolutionToTypeVariables` typeVars) solutions
     in foldl appendTypeVariableEqualities initial applied
