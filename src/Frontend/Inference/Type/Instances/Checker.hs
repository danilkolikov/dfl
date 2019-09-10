{- |
Module      :  Frontend.Inference.Instances.Checker
Description :  Functions for checking of instances
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Function for checking of instances
-}
module Frontend.Inference.Type.Instances.Checker where

import Control.Monad (foldM, unless)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromMaybe)

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Instance
import Frontend.Inference.Signature

-- | A type of errors which can be raised during instance checking
data InstanceCheckerError
    = InstanceCheckerErrorSynonym F.Ident -- ^ An instance uses a type synonym
    | InstanceCheckerErrorDefined F.Ident -- ^ An instance is defined twice

-- | A type of a map of defined instances
type DefinedInstances = HM.HashMap F.Ident (HM.HashMap F.Ident Instance)

-- | Collects instances into the map
collectInstances ::
       Signatures TypeSignature
    -> [Instance]
    -> Either InstanceCheckerError DefinedInstances
collectInstances typeSynonyms = foldM processInstance HM.empty
  where
    processInstance ::
           DefinedInstances
        -> Instance
        -> Either InstanceCheckerError DefinedInstances
    processInstance defined DefaultInstance {} = return defined
    processInstance defined inst@Instance { getInstanceType = typeName
                                          , getInstanceClass = className
                                          } = do
        unless (not $ HM.member typeName typeSynonyms) $
            Left $ InstanceCheckerErrorSynonym typeName
        let classInstances = fromMaybe HM.empty $ HM.lookup className defined
        case HM.lookup typeName classInstances of
            Just _ -> Left $ InstanceCheckerErrorDefined typeName
            Nothing ->
                let newClassInstances = HM.insert typeName inst classInstances
                 in return $ HM.insert className newClassInstances defined
