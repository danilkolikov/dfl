{- |
Module      :  Frontend.Inference.BuiltIns
Description :  Definition of built-in constructions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module with signatures of built-in functions and types
-}
module Frontend.Inference.BuiltIns where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast (Ident(..))
import Frontend.Inference.Base.Common
import Frontend.Inference.Signature
import Frontend.Syntax.EntityName

-- | Makes a pair of ident and object
makePair :: EntityName -> a -> (Ident, a)
makePair name x = (IdentNamed name, x)

-- | The minimal size of a tuple
mIN_TUPLE_SIZE :: Int
mIN_TUPLE_SIZE = 2

-- | The maximal size of a tuple
mAX_TUPLE_SIZE :: Int
mAX_TUPLE_SIZE = 10

-- | Default signatures of data types
defaultKindSignatures :: Signatures TypeConstructorSignature
defaultKindSignatures =
    HM.fromList $
    [ makeTerminal uNIT_NAME
    , makePair lIST_NAME $
      TypeConstructorSignature
          SortSquare
          []
          KindStar
          [(IdentNamed ["a"], KindStar)]
    , makeTerminal iNT_NAME
    , makeTerminal fLOAT_NAME
    , makeTerminal cHAR_NAME
    , makeTerminal sTRING_NAME
    , makeTerminal bOOL_NAME
    ] ++
    [makeTuple i | i <- [mIN_TUPLE_SIZE .. mAX_TUPLE_SIZE]]
  where
    makeTerminal :: EntityName -> (Ident, TypeConstructorSignature)
    makeTerminal name =
        makePair name $ TypeConstructorSignature SortSquare [] KindStar []
    makeTuple :: Int -> (Ident, TypeConstructorSignature)
    makeTuple n =
        let params = [(IdentNamed ["a" ++ show i], KindStar) | i <- [1 .. n]]
         in ( IdentParametrised tUPLE_NAME n
            , TypeConstructorSignature SortSquare [] KindStar params)

-- | Default signatures for type synonyms
defaultTypeSynonyms :: Signatures TypeSignature
defaultTypeSynonyms = HM.empty

-- | Default constructors
defaultConstructors :: Signatures TypeSignature
defaultConstructors =
    HM.fromList $
    [ makePair uNIT_NAME $
      TypeSignature
          SortSquare
          []
          KindStar
          []
          (TypeConstr (IdentNamed uNIT_NAME))
          []
    ] ++
    (let var = IdentNamed ["a"]
         listType =
             TypeApplication
                 (TypeConstr (IdentNamed lIST_NAME))
                 (TypeVar var NE.:| [])
         makeConstr name type' =
             makePair name $
             TypeSignature SortSquare [] KindStar [(var, KindStar)] type' []
      in [ makeConstr lIST_NAME listType
         , makeConstr
               cOLON_NAME
               (TypeFunction (TypeVar var) (TypeFunction listType listType))
         ]) ++
    (let boolType = TypeConstr (IdentNamed bOOL_NAME)
         makeConstr name =
             makePair name $ TypeSignature SortSquare [] KindStar [] boolType []
      in [makeConstr tRUE_NAME, makeConstr fALSE_NAME]) ++
    (let makeIdent = IdentParametrised tUPLE_NAME
         tupleType n =
             case tupleVars n of
                 [] -> error "Tuple of 0 params is unsupported"
                 (f:rest) ->
                     TypeApplication (TypeConstr (makeIdent n)) (f NE.:| rest)
         tupleParams n = map (\i -> IdentNamed ["a" ++ show i]) [1 .. n]
         tupleVars = map TypeVar . tupleParams
         makeKindParams = map (\x -> (x, KindStar)) . tupleParams
         makeType n = foldr (TypeFunction) (tupleType n) (tupleVars n)
         makeConstr n =
             ( makeIdent n
             , TypeSignature
                   SortSquare
                   []
                   KindStar
                   (makeKindParams n)
                   (makeType n)
                   [])
      in [makeConstr n | n <- [mIN_TUPLE_SIZE .. mAX_TUPLE_SIZE]])

-- | Default expressions
defaultExpressions :: Signatures TypeSignature
defaultExpressions =
    HM.fromList $
    (let var = IdentNamed ["a"]
         makeExp name type' =
             makePair name $
             TypeSignature SortSquare [] KindStar [(var, KindStar)] type' []
      in [ makeExp uNDEFINED_NAME (TypeVar var)
         , makeExp
               fAIL_NAME
               (TypeFunction (TypeConstr (IdentNamed sTRING_NAME)) (TypeVar var))
         ]) ++
    (let aVar = IdentNamed ["a"]
         aKind = KindStar
         bVar = IdentNamed ["b"]
         bKind = KindStar
         mVar = IdentNamed ["m"]
         mKind = KindFunction KindStar KindStar
         mA = TypeApplication (TypeVar mVar) (TypeVar aVar NE.:| [])
         mB = TypeApplication (TypeVar mVar) (TypeVar bVar NE.:| [])
         constraints =
             [ConstraintVariable (IdentNamed mONAD_NAME) (TypeVar mVar)]
         makeExp name type' =
             makePair name $
             TypeSignature
                 SortSquare
                 []
                 KindStar
                 [(mVar, mKind), (aVar, aKind), (bVar, bKind)]
                 type'
                 constraints
      in [ makeExp iGNORING_BIND_NAME (TypeFunction mA (TypeFunction mB mB))
         , makeExp
               bIND_NAME
               (TypeFunction
                    mA
                    (TypeFunction (TypeFunction (TypeVar aVar) mB) mB))
         ]) ++
    (let var = IdentNamed ["a"]
         aVar = TypeVar var
         constraints = [ConstraintVariable (IdentNamed eQ_NAME) aVar]
         makeExp name type' =
             makePair name $
             TypeSignature
                 SortSquare
                 []
                 KindStar
                 [(var, KindStar)]
                 type'
                 constraints
      in [ makeExp
               eQUAL_NAME
               (TypeFunction
                    aVar
                    (TypeFunction aVar (TypeConstr (IdentNamed bOOL_NAME))))
         ]) ++
    (let var = IdentNamed ["a"]
         aVar = TypeVar var
         constraints = [ConstraintVariable (IdentNamed nUM_NAME) aVar]
         makeExp name type' =
             makePair name $
             TypeSignature
                 SortSquare
                 []
                 KindStar
                 [(var, KindStar)]
                 type'
                 constraints
      in [makeExp eQUAL_NAME (TypeFunction aVar aVar)]) ++
    (let var = IdentNamed ["a"]
         aVar = TypeVar var
         list =
             TypeApplication (TypeConstr $ IdentNamed lIST_NAME) (aVar NE.:| [])
         constraints = [ConstraintVariable (IdentNamed eNUM_NAME) aVar]
         makeExp name type' =
             makePair name $
             TypeSignature
                 SortSquare
                 []
                 KindStar
                 [(var, KindStar)]
                 type'
                 constraints
      in [ makeExp eNUM_FROM_NAME (TypeFunction aVar list)
         , makeExp
               eNUM_FROM_TO_NAME
               (TypeFunction aVar (TypeFunction aVar list))
         , makeExp
               eNUM_FROM_THEN_NAME
               (TypeFunction aVar (TypeFunction aVar list))
         , makeExp
               eNUM_FROM_THEN_TO_NAME
               (TypeFunction aVar (TypeFunction aVar (TypeFunction aVar list)))
         ]) ++
    (let aIdent = IdentNamed ["a"]
         aVar = TypeVar aIdent
         bIdent = IdentNamed ["b"]
         bVar = TypeVar bIdent
         listA =
             TypeApplication (TypeConstr $ IdentNamed lIST_NAME) (aVar NE.:| [])
         listB =
             TypeApplication (TypeConstr $ IdentNamed lIST_NAME) (bVar NE.:| [])
         makeExp name type' =
             makePair name $
             TypeSignature
                 SortSquare
                 []
                 KindStar
                 [(aIdent, KindStar), (bIdent, KindStar)]
                 type'
                 []
      in [ makeExp cONCAT_MAP_NAME $
           TypeFunction (TypeFunction aVar listB) (TypeFunction listA listB)
         ])
