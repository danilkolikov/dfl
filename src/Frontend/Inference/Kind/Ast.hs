{- |
Module      :  Frontend.Inference.Kind.Ast
Description :  Modified version of AST with kind information
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Modified version of AST, produced by kind inference with included kind information
-}
module Frontend.Inference.Kind.Ast where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import qualified Data.List.NonEmpty as NE

import Frontend.Desugaring.Final.Ast (Const, Ident, SimpleConstraint)
import Frontend.Inference.Signature
import Frontend.Inference.WithVariables
import Frontend.Syntax.Position

-- | A slice of a module with kind information
data AstWithKinds = AstWithKinds
    { getAstWithKindsClasses :: HM.HashMap Ident Class
    , getAstWithKindsDataTypes :: HM.HashMap Ident DataType
    , getAstWithKindsInstances :: [Instance]
    , getAstWithKindsExpressions :: HM.HashMap Ident Expression
    } deriving (Eq, Show)

-- | Definition of a data type
data DataType = DataType
    { getDataTypeContext :: [Constraint] -- ^ Context of a data type
    , getDataTypeName :: WithLocation Ident -- ^ Name of a data type
    , getDataTypeParams :: [WithLocation Ident] -- ^ Parameters of a data type
    , getDataTypeDeriving :: [WithLocation Ident] -- ^ List of instances to derive
    , getDataTypeConstructors :: [(Ident, Constructor)] -- ^ List of constructors
    , isNewType :: Bool -- ^ Is this type a newtype?
    } deriving (Show, Eq)

-- | Constructor of a data type
data Constructor = Constructor
    { getConstructorName :: WithLocation Ident -- ^ Name of a constructor
    , getConstructorArgs :: [Type] -- ^ Arguments of a constructor
    , getConstructorFields :: HM.HashMap Ident Int -- ^ Map of fields of a constructor.
    } deriving (Show, Eq)

-- | Definition of a type class
data Class = Class
    { getClassContext :: [WithLocation SimpleConstraint] -- ^ Context of a type class
    , getClassName :: WithLocation Ident -- ^ Name of a type class
    , getClassParam :: WithLocation Ident -- ^ Parameter of a type class
    , getClassMethods :: HM.HashMap Ident Method -- ^ Methods of a type class
    } deriving (Show, Eq)

-- | Definition of an instance of a type class
data Instance = Instance
    { getInstanceContext :: [WithLocation SimpleConstraint] -- ^ Context of an instance
    , getInstanceClass :: WithLocation Ident -- ^ Name of a type class
    , getInstanceType :: WithLocation Ident -- ^ Name of a type
    , getInstanceTypeArgs :: [WithLocation Ident] -- ^ Arguments of a type
    , getInstanceMethods :: HM.HashMap Ident (WithLocation Exp) -- ^ Methods of an instance
    } deriving (Show, Eq)

-- | Definition of an expression
data Expression = Expression
    { getExpressionName :: WithLocation Ident -- ^ Name of an expression
    , getExpressionBody :: WithLocation Exp -- ^ Body of an expression
    , getExpressionType :: Maybe TypeSignature -- ^ Optional type signature
    } deriving (Show, Eq)

-- | Definition of a class method
data Method = Method
    { getMethodName :: WithLocation Ident -- ^ Name of a method
    , getMethodType :: TypeSignature -- ^ Type signature
    , getMethodBody :: Maybe (WithLocation Exp) -- ^ Optional default implementation
    } deriving (Show, Eq)

-- | Expression
data Exp
    = ExpAbstraction (WithLocation Ident)
                     (WithLocation Exp) -- ^ Lambda-abstraction
    | ExpLet (HM.HashMap Ident Expression)
             (WithLocation Exp) -- ^ Let-abstraction
    | ExpCase (WithLocation Ident)
              (WithLocation Ident)
              [WithLocation Ident]
              (WithLocation Exp)
              (WithLocation Ident) -- ^ Case expression
    | ExpApplication (WithLocation Exp)
                     (NE.NonEmpty (WithLocation Exp)) -- ^ Application of expressions
    | ExpVar (WithLocation Ident) -- ^ Variable
    | ExpConstr (WithLocation Ident) -- ^ Constructor
    | ExpConst (WithLocation Const) -- ^ Constant
    deriving (Show, Eq)

instance WithVariables Exp where
    getVariableName (ExpVar name) = Just $ getValue name
    getVariableName _ = Nothing
    getFreeVariables exp' =
        case exp' of
            ExpVar name -> HS.singleton (getValue name)
            ExpConst _ -> HS.empty
            ExpConstr _ -> HS.empty
            ExpApplication func args ->
                HS.unions . map (getFreeVariables . getValue) $
                func : NE.toList args
            ExpAbstraction var inner ->
                HS.delete (getValue var) $ getFreeVariables (getValue inner)
            ExpCase _ _ params ifTrue _ ->
                foldr
                    (HS.delete . getValue)
                    (getFreeVariables $ getValue ifTrue)
                    params
            ExpLet exprs inner ->
                let exprsFree =
                        HS.unions . map (getFreeVariables . getValue) $
                        inner : map getExpressionBody (HM.elems exprs)
                    bound = HM.keysSet exprs
                 in exprsFree `HS.difference` bound
