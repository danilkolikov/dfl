{- |
Module      :  Frontend.Inference.Translation.Processor
Description :  Functions for translation of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for translation of expressions
-}
module Frontend.Inference.Translation.Processor
    ( TranslationProcessorError(..)
    , TranslationProcessorDebugOutput(..)
    , translateExpressions
    ) where

import Control.Monad (liftM2, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import Data.Foldable (asum)
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe, mapMaybe)

import qualified Frontend.Desugaring.Final.Ast as F
import qualified Frontend.Inference.Class.Ast as C
import qualified Frontend.Inference.Kind.Ast as K
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Type.Ast
import Frontend.Inference.Util.Debug
import Frontend.Inference.Util.HashMap
import Frontend.Inference.Variables
import Frontend.Syntax.EntityName (fUNCTION_NAME)
import Frontend.Syntax.Position

-- | Errors which can be encountered during translation of expressions
data TranslationProcessorError
    = TranslationProcessorErrorUnknownExpression Ident -- ^ Unknown expression
    | TranslationProcessorErrorUnknownClass Ident -- ^ Unknown class
    | TranslationProcessorErrorUnsatisfiedConstraint Ident
                                                     Ident -- ^ An instance's constraint is unsatisfied
    | TranslationProcessorErrorMissingInstance Ident
                                               Ident -- ^ An instance is missing
    | TranslationProcessorErrorArgsLengthMismatch Ident
                                                  Int
                                                  Int -- ^ Mismatch of the number of arguments of instance and constraint
    deriving (Eq, Show)

-- | Debug output of expression translation process
newtype TranslationProcessorDebugOutput = TranslationProcessorDebugOutput
    { getTranslationProcessorDebugOutputExpressions :: Maybe (HM.HashMap Ident ExpWithSignature)
    } deriving (Eq, Show)

instance Semigroup TranslationProcessorDebugOutput where
    TranslationProcessorDebugOutput e1 <> TranslationProcessorDebugOutput e2 =
        TranslationProcessorDebugOutput (e1 <> e2)

instance Monoid TranslationProcessorDebugOutput where
    mempty = TranslationProcessorDebugOutput mempty

data TranslationProcessorEnvironment = TranslationProcessorEnvironment
    { getTranslationProcessorEnvironmentTypeSignatures :: HM.HashMap Ident TypeSignature
    , getTranslationProcessorEnvironmentClasses :: HM.HashMap Ident C.Class
    , getTranslationProcessorEnvironmentInstances :: HM.HashMap Ident K.Instance
    , getTranslationProcessorEnvironmentConstraints :: [(Constraint, Ident)]
    } deriving (Eq, Show)

type TranslationProcessor
     = WithDebugOutput TranslationProcessorError TranslationProcessorDebugOutput

-- | Translates expressions with context into expressions without context
translateExpressions ::
       HM.HashMap Ident C.Class
    -> HM.HashMap Ident K.Instance
    -> HM.HashMap Ident TypeSignature
    -> HM.HashMap Ident ExpWithSignature
    -> ( Either TranslationProcessorError (HM.HashMap Ident ExpWithSignature)
       , TranslationProcessorDebugOutput)
translateExpressions classes instances typeSignatures expressions =
    runWithDebugOutput $
    mapHashMapWithKeyM
        (translateExpression
             classes
             instances
             (typeSignatures <> HM.map snd expressions))
        expressions

translateExpression ::
       HM.HashMap Ident C.Class
    -> HM.HashMap Ident K.Instance
    -> HM.HashMap Ident TypeSignature
    -> Ident
    -> ExpWithSignature
    -> TranslationProcessor ExpWithSignature
translateExpression classes instances typeSignatures expName (exp', signature)
    | TypeSignature {getTypeSignatureContext = context} <- signature = do
        let constraintIdents =
                zip
                    context
                    [IdentGenerated IdentEnvironmentTranslation i | i <- [0 ..]]
            environment =
                TranslationProcessorEnvironment
                    { getTranslationProcessorEnvironmentClasses = classes
                    , getTranslationProcessorEnvironmentTypeSignatures =
                          typeSignatures
                    , getTranslationProcessorEnvironmentInstances = instances
                    , getTranslationProcessorEnvironmentConstraints =
                          constraintIdents
                    }
        translatedExp <- runReaderT (translateExp exp') environment
        let resultExp =
                foldr (ExpAbstraction . snd) translatedExp constraintIdents
            result = (resultExp, signature)
        writeDebugOutput
            mempty
                { getTranslationProcessorDebugOutputExpressions =
                      Just $ HM.singleton expName result
                }
        return result

type SingleExpressionProcessor
     = ReaderT TranslationProcessorEnvironment TranslationProcessor

translateExp :: Exp -> SingleExpressionProcessor Exp
translateExp exp' =
    case exp' of
        ExpVar {} -> return exp'
        ExpExternal external -> translateExternal external
        ExpConstr external -> translateExternal external >> return exp' -- Just check constraints
        ExpConst {} -> return exp'
        ExpAbstraction name inner -> ExpAbstraction name <$> translateExp inner
        ExpCase var typeName args ifTrue ifFalse ->
            (\res -> ExpCase var typeName args res ifFalse) <$>
            translateExp ifTrue
        ExpApplication func args ->
            liftM2 ExpApplication (translateExp func) (mapM translateExp args)

translateExternal :: External -> SingleExpressionProcessor Exp
translateExternal external
    | External {getExternalName = name, getExternalTypeArgs = typeArgs} <-
         external = do
        signatures <- asks getTranslationProcessorEnvironmentTypeSignatures
        TypeSignature {getTypeSignatureContext = context} <-
            lift $
            lookupMapValue
                (TranslationProcessorErrorUnknownExpression name)
                name
                signatures
        args <- mapM (processConstraint typeArgs) context
        let resultExp = ExpExternal external
        return $
            case args of
                [] -> resultExp
                f:rest -> ExpApplication resultExp (f NE.:| rest)

processConstraint ::
       Substitution Type -> Constraint -> SingleExpressionProcessor Exp
processConstraint sub constraint =
    let findVar var = fromMaybe (TypeVar var) $ HM.lookup var sub
     in case constraint of
            ConstraintVariable cls var ->
                processSimpleConstraint cls $ findVar var
            ConstraintAppliedVariable cls var args ->
                processAppliedConstraint cls (findVar var) args

processSimpleConstraint :: Ident -> Type -> SingleExpressionProcessor Exp
processSimpleConstraint cls type' =
    case type' of
        TypeVar name -> checkVariableConstraint cls name []
        TypeConstr name -> checkTypeConstraint cls name []
        TypeFunction from to ->
            processSimpleConstraint cls (makeFunctionType from to)
        TypeApplication func args -> processAppliedConstraint cls func args

processAppliedConstraint ::
       Ident -> Type -> NE.NonEmpty Type -> SingleExpressionProcessor Exp
processAppliedConstraint cls type' args =
    case type' of
        TypeVar name -> checkVariableConstraint cls name (NE.toList args)
        TypeConstr name -> checkTypeConstraint cls name (NE.toList args)
        TypeFunction from to ->
            processAppliedConstraint cls (makeFunctionType from to) args
        TypeApplication func (f NE.:| rest) ->
            processAppliedConstraint cls func (f NE.:| rest ++ NE.toList args)

checkVariableConstraint ::
       Ident -> Ident -> [Type] -> SingleExpressionProcessor Exp
checkVariableConstraint cls var args = do
    constraints <- asks getTranslationProcessorEnvironmentConstraints
    let constraining = mapMaybe (getConstrainingClass var args) constraints
        makeExp (constrCls, ident) =
            let arg = ExpVar ident
                makeGetter getter =
                    ExpApplication (ExpVar getter) (arg NE.:| [])
             in if constrCls == cls
                    then return $ Just arg
                    else do
                        classes <-
                            asks getTranslationProcessorEnvironmentClasses
                        C.Class {C.getClassGetters = getters} <-
                            lift $
                            lookupMapValue
                                (TranslationProcessorErrorUnknownClass constrCls)
                                constrCls
                                classes
                        return $ makeGetter <$> HM.lookup cls getters
    exps <- mapM makeExp constraining
    case asum exps of
        Just exp' -> return exp'
        Nothing ->
            lift . raiseError $
            TranslationProcessorErrorUnsatisfiedConstraint cls var

checkTypeConstraint :: Ident -> Ident -> [Type] -> SingleExpressionProcessor Exp
checkTypeConstraint cls type' args = do
    instances <- asks getTranslationProcessorEnvironmentInstances
    let instanceName = IdentInstance cls type'
    case HM.lookup instanceName instances of
        Just inst -> checkInstance instanceName inst args
        Nothing ->
            lift . raiseError $
            TranslationProcessorErrorMissingInstance cls type'

checkInstance :: Ident -> K.Instance -> [Type] -> SingleExpressionProcessor Exp
checkInstance instName inst args
    | K.Instance { K.getInstanceContext = instanceContext
                 , K.getInstanceTypeArgs = instanceArgs
                 } <- inst = do
        unless (length instanceArgs == length args) . lift . raiseError $
            TranslationProcessorErrorArgsLengthMismatch
                instName
                (length instanceArgs)
                (length args)
        let typeSubstitution =
                HM.fromList $ zip (map getValue instanceArgs) args
            makeConstraint (F.SimpleConstraint cls var) =
                ConstraintVariable (getValue cls) (getValue var)
            constraints = map (makeConstraint . getValue) instanceContext
        resultArgs <- mapM (processConstraint typeSubstitution) constraints
        let external =
                External
                    { getExternalName = instName
                    , getExternalKindArgs = HM.empty -- TODO: understand, what should be here
                    , getExternalTypeArgs = typeSubstitution
                    }
            resultExp = ExpExternal external
        return $
            case resultArgs of
                [] -> resultExp
                f:rest -> ExpApplication resultExp (f NE.:| rest)

getConstrainingClass ::
       Ident -> [Type] -> (Constraint, Ident) -> Maybe (Ident, Ident)
getConstrainingClass var args (constraint, ident) =
    let (constrCls, constrVar, constrArgs) =
            case constraint of
                ConstraintVariable cls' var' -> (cls', var', [])
                ConstraintAppliedVariable cls' var' args' ->
                    (cls', var', NE.toList args')
     in if constrVar == var && constrArgs == args
            then Just (constrCls, ident)
            else Nothing

makeFunctionType :: Type -> Type -> Type
makeFunctionType from to =
    let functionConstructor = TypeVar $ IdentNamed fUNCTION_NAME
     in TypeApplication functionConstructor (from NE.:| [to])
