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

import Core.Ident
import Core.PredefinedIdents
import Frontend.Inference.Signature

-- | Makes a pair of ident and object
makePair :: UserDefinedIdent -> a -> (Ident, a)
makePair name x = (IdentUserDefined name, x)

-- | The minimal size of a tuple
mIN_TUPLE_SIZE :: Int
mIN_TUPLE_SIZE = 2

-- | The maximal size of a tuple
mAX_TUPLE_SIZE :: Int
mAX_TUPLE_SIZE = 10

makeNamedIdent :: String -> Ident
makeNamedIdent = IdentUserDefined . IdentSimple . IdentNamed

-- | Default signatures of data types
defaultKindSignatures :: Signatures TypeConstructorSignature
defaultKindSignatures =
    HM.fromList $
    [ makeTerminal uNIT
    , makePair lIST $
      TypeConstructorSignature
          SortSquare
          []
          KindStar
          [(makeNamedIdent "a", KindStar)]
    , makeTerminal iNT
    , makeTerminal fLOAT
    , makeTerminal cHAR
    , makeTerminal sTRING
    , makeTerminal bOOL
    ] ++
    [makeTuple i | i <- [mIN_TUPLE_SIZE .. mAX_TUPLE_SIZE]]
  where
    makeTerminal :: UserDefinedIdent -> (Ident, TypeConstructorSignature)
    makeTerminal name =
        makePair name $ TypeConstructorSignature SortSquare [] KindStar []
    makeTuple :: Int -> (Ident, TypeConstructorSignature)
    makeTuple n =
        let params =
                [(makeNamedIdent ("a" ++ show i), KindStar) | i <- [1 .. n]]
         in ( IdentUserDefined $ tUPLE n
            , TypeConstructorSignature SortSquare [] KindStar params)

-- | Default signatures for type synonyms
defaultTypeSynonyms :: Signatures TypeSignature
defaultTypeSynonyms = HM.empty

-- | Default constructors
defaultConstructors :: Signatures TypeSignature
defaultConstructors =
    HM.fromList $
    [ makePair uNIT $
      TypeSignature
          SortSquare
          []
          KindStar
          []
          (TypeConstr (IdentUserDefined uNIT))
          []
    ] ++
    (let var = makeNamedIdent "a"
         listType =
             TypeApplication
                 (TypeConstr (IdentUserDefined lIST))
                 (TypeVar var NE.:| [])
         makeConstr name type' =
             makePair name $
             TypeSignature SortSquare [] KindStar [(var, KindStar)] type' []
      in [ makeConstr lIST listType
         , makeConstr
               cOLON
               (TypeFunction (TypeVar var) (TypeFunction listType listType))
         ]) ++
    (let boolType = TypeConstr (IdentUserDefined bOOL)
         makeConstr name =
             makePair name $ TypeSignature SortSquare [] KindStar [] boolType []
      in [makeConstr tRUE, makeConstr fALSE]) ++
    (let makeIdent = IdentUserDefined . tUPLE
         tupleType n =
             case tupleVars n of
                 [] -> error "Tuple of 0 params is unsupported"
                 (f:rest) ->
                     TypeApplication (TypeConstr (makeIdent n)) (f NE.:| rest)
         tupleParams n = map (\i -> makeNamedIdent ("a" ++ show i)) [1 .. n]
         tupleVars = map TypeVar . tupleParams
         makeKindParams = map (\x -> (x, KindStar)) . tupleParams
         makeType n = foldr TypeFunction (tupleType n) (tupleVars n)
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
    (let var = makeNamedIdent "a"
         makeExp name type' =
             makePair name $
             TypeSignature SortSquare [] KindStar [(var, KindStar)] type' []
      in [ makeExp uNDEFINED (TypeVar var)
         , makeExp
               fAIL
               (TypeFunction
                    (TypeConstr (IdentUserDefined sTRING))
                    (TypeVar var))
         ]) ++
    (let aVar = makeNamedIdent "a"
         aKind = KindStar
         bVar = makeNamedIdent "b"
         bKind = KindStar
         mVar = makeNamedIdent "m"
         mKind = KindFunction KindStar KindStar
         mA = TypeApplication (TypeVar mVar) (TypeVar aVar NE.:| [])
         mB = TypeApplication (TypeVar mVar) (TypeVar bVar NE.:| [])
         constraints = [ConstraintVariable (IdentUserDefined mONAD) mVar]
         makeExp name type' =
             makePair name $
             TypeSignature
                 SortSquare
                 []
                 KindStar
                 [(mVar, mKind), (aVar, aKind), (bVar, bKind)]
                 type'
                 constraints
      in [ makeExp iGNORING_BIND (TypeFunction mA (TypeFunction mB mB))
         , makeExp
               bIND
               (TypeFunction
                    mA
                    (TypeFunction (TypeFunction (TypeVar aVar) mB) mB))
         ]) ++
    (let var = makeNamedIdent "a"
         aVar = TypeVar var
         constraints = [ConstraintVariable (IdentUserDefined eQ) var]
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
               eQUAL
               (TypeFunction
                    aVar
                    (TypeFunction aVar (TypeConstr (IdentUserDefined bOOL))))
         ]) ++
    (let var = makeNamedIdent "a"
         aVar = TypeVar var
         constraints = [ConstraintVariable (IdentUserDefined nUM) var]
         makeExp name type' =
             makePair name $
             TypeSignature
                 SortSquare
                 []
                 KindStar
                 [(var, KindStar)]
                 type'
                 constraints
      in [makeExp eQUAL (TypeFunction aVar aVar)]) ++
    (let var = makeNamedIdent "a"
         aVar = TypeVar var
         list =
             TypeApplication
                 (TypeConstr $ IdentUserDefined lIST)
                 (aVar NE.:| [])
         constraints = [ConstraintVariable (IdentUserDefined eNUM) var]
         makeExp name type' =
             makePair name $
             TypeSignature
                 SortSquare
                 []
                 KindStar
                 [(var, KindStar)]
                 type'
                 constraints
      in [ makeExp eNUM_FROM (TypeFunction aVar list)
         , makeExp eNUM_FROM_TO (TypeFunction aVar (TypeFunction aVar list))
         , makeExp eNUM_FROM_THEN (TypeFunction aVar (TypeFunction aVar list))
         , makeExp
               eNUM_FROM_THEN_TO
               (TypeFunction aVar (TypeFunction aVar (TypeFunction aVar list)))
         ]) ++
    (let aIdent = makeNamedIdent "a"
         aVar = TypeVar aIdent
         bIdent = makeNamedIdent "b"
         bVar = TypeVar bIdent
         listA =
             TypeApplication
                 (TypeConstr $ IdentUserDefined lIST)
                 (aVar NE.:| [])
         listB =
             TypeApplication
                 (TypeConstr $ IdentUserDefined lIST)
                 (bVar NE.:| [])
         makeExp name type' =
             makePair name $
             TypeSignature
                 SortSquare
                 []
                 KindStar
                 [(aIdent, KindStar), (bIdent, KindStar)]
                 type'
                 []
      in [ makeExp cONCAT_MAP $
           TypeFunction (TypeFunction aVar listB) (TypeFunction listA listB)
         ])
