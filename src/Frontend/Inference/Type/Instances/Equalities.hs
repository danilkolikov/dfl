{- |
Module      :  Frontend.Inference.Type.Instances.Equalities
Description :  Function for generation of kind equalities for instances
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for generation of equalities between kinds and sorts of instance
definitions
-}
module Frontend.Inference.Type.Instances.Equalities where

import qualified Data.HashMap.Lazy as HM

import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Inference.Base.Common
import Frontend.Inference.Base.Descriptor
import Frontend.Inference.Equalities
import Frontend.Inference.Signature
import Frontend.Inference.Substitution
import Frontend.Inference.Variables hiding (Type(..))
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position

-- | Collects kind equalities for a type of an instance
generateEqualitiesForInstances ::
       Signatures TypeConstructorSignature
    -> EqualitiesBuilder F.Instances () ()
generateEqualitiesForInstances tyConSignatures _ _ instances _ =
    let localEnvironment =
            emptyEqualitiesGeneratorEnvironment
                {getTypeConstructorSignatures = tyConSignatures}
     in runEqualitiesGenerator
            (generateEqualitiesForInstances' instances)
            localEnvironment

-- | Collects kind equalities for a type of an instance
generateEqualitiesForInstances' ::
       F.Instances
    -> EqualitiesGenerator  (Signatures (((), ()), [Ident]))
generateEqualitiesForInstances' instances =
    mapM_ generateEqualitiesForInstance instances >> return HM.empty

-- | Generates equalities for an instance
generateEqualitiesForInstance ::
       F.Instance -> EqualitiesGenerator  ((), ())
generateEqualitiesForInstance F.Instance { F.getInstanceContext = context
                                         , F.getInstanceClass = className
                                         , F.getInstanceType = typeName
                                         , F.getInstanceTypeArgs = typeArgs
                                         } = do
    params <- createNewKindVariables $ map getValue typeArgs
    withKindVariables params $ writeClassEqualities className typeName typeArgs
    withKindVariables params $ mapM_ writeSimpleConstraintEqualities context
    return ((), ())

-- | Writes equalities about the class of an instance
writeClassEqualities ::
       WithLocation Ident
    -> WithLocation Ident
    -> [WithLocation Ident]
    -> EqualitiesGenerator  ()
writeClassEqualities className typeName typeArgs = do
    ((typeKind, typeSort), _) <- findKindOfType typeName
    args <- mapM lookupKindVariable typeArgs
    (resultKind, resultSort) <- createNewKindVariable
    let expectedTypeKind = foldr (KindFunction . fst) resultKind args
    writeKindEqualities [(typeKind, expectedTypeKind)]
    writeSortSquare $ typeSort : map snd args
    ((classKind, classSort), _) <- lookupKindOfType className
    let expectedClassKind = KindFunction resultKind KindStar
    writeKindEqualities [(classKind, expectedClassKind)]
    writeSortSquare [resultSort, classSort]

-- | Writes kind equalities of a simple constraint
writeSimpleConstraintEqualities ::
       WithLocation F.SimpleConstraint -> EqualitiesGenerator  ()
writeSimpleConstraintEqualities sc
    | F.SimpleConstraint className param <- getValue sc = do
        ((classKind, classSort), _) <- lookupKindOfType className
        (paramKind, paramSort) <- lookupKindVariable param
        let expectedKind = KindFunction paramKind KindStar
        writeKindEqualities [(classKind, expectedKind)]
        writeSortSquare [classSort, paramSort]

-- | Finds a kind of a type
findKindOfType ::
       WithLocation Ident
    -> EqualitiesGenerator  ((Kind, Sort), Substitution Kind)
findKindOfType typeName
    | getValue typeName == IdentNamed fUNCTION_NAME =
        return
            ( ( KindFunction KindStar (KindFunction KindStar KindStar)
              , SortSquare)
            , HM.empty)
    | otherwise = lookupKindOfType typeName
