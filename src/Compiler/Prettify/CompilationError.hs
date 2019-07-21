{- |
Module      :  Compiler.Prettify.CompilationError
Description :  Prettifying of CompilationError
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of CompilationError
-}
module Compiler.Prettify.CompilationError where

import Data.List (intercalate)

import Compiler.Error
import Compiler.Prettify.TokenStream (prettifyToken)
import Compiler.Prettify.Utils
import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Desugaring.Processor
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Kind.Equalities
import Frontend.Inference.Processor
import Frontend.Inference.Type.Signatures
import Frontend.Inference.TypeSynonyms.Expand (TypeSynonymsExpandingError(..))
import Frontend.Inference.TypeSynonyms.Processor
    ( TypeSynonymsProcessingError(..)
    )
import Frontend.Inference.Unification
import Frontend.Syntax.Position (WithLocation(WithLocation))
import Frontend.Syntax.Processor
import Frontend.Syntax.Token (VarId(..))

prettifyCompilationError :: CompilationError -> String
prettifyCompilationError err =
    "Compilation error: " ++ 
    case err of
        CompilerErrorLexer l -> prettifyLexerError l
        CompilerErrorParser p -> prettifyParserError p
        CompilerErrorFixity f -> prettifyFixityError f
        CompilerErrorDesugaring d -> prettifyDesugaringError d
        CompilerErrorInference i -> prettifyInferenceError i

prettifyLexerError :: LexicalError -> String
prettifyLexerError err =
    "Lexer error: " ++
    case err of
        LexicalErrorLexer pos tok expected ->
            "Unexpected character " ++
            maybe "" (\t -> show t ++ " ") tok ++
            "at position " ++
            prettifyPosition pos ++ ", expected " ++ intercalate ", " expected
        LexicalErrorLayout layoutError ->
            "Layout error: " ++ prettifyLayoutError layoutError
        LexicalErrorUnexpected pos ->
            "Unexpected lexer error at position " ++ prettifyPosition pos

prettifyLayoutError :: LayoutError -> String
prettifyLayoutError layoutError =
    case layoutError of
        LayoutErrorRedundantClosingBracket loc ->
            "Redundant closing bracket at location " ++ prettifyLocation loc
        LayoutErrorMissingClosingBracket ->
            "Missing closing bracket at the end of the file"

prettifyParserError :: ParserError -> String
prettifyParserError err =
    "Parser error: " ++
    case err of
        ParserErrorParser loc tok expected ->
            "Unexpected token " ++
            maybe "" (\t -> prettifyToken t ++ " ") tok ++
            "at location " ++
            prettifyLocation loc ++ ", expected " ++ intercalate ", " expected
        ParserErrorChecker checkerError ->
            "Ast checker error: " ++ prettifyAstCheckerError checkerError
        ParserErrorUnexpected pos ->
            "Unexpected parser error at position " ++ prettifyPosition pos

prettifyAstCheckerError :: AstCheckerError -> String
prettifyAstCheckerError checkerError =
    case checkerError of
        AstCheckerErrorSameTyVar first second ->
            let prettifyTyVar (WithLocation (VarId ident) loc) =
                    ident ++ "(" ++ prettifyLocation loc ++ ")"
             in "Type variables should be different: " ++
                prettifyTyVar first ++ " and " ++ prettifyTyVar second
        AstCheckerErrorLastStatement (WithLocation _ loc) ->
            "Last statement in do block shouldn't be let or pattern assignment (" ++
            prettifyLocation loc ++ ")"

prettifyFixityError :: FixityResolutionError -> String
prettifyFixityError err =
    let prettifyOperator (InfixOperator op _ _) = prettifyEntityName op
     in "Fixity resolution error: " ++
        case err of
            FixityResolutionErrorRedefinedOperator op _ loc ->
                "Redefinition of operator " ++
                prettifyOperator op ++ " at location " ++ prettifyLocation loc
            FixityResolutionErrorMissingOperand op loc ->
                "Missing operand of operator " ++
                prettifyOperator op ++ " at location " ++ prettifyLocation loc
            FixityResolutionErrorMissingOperator loc ->
                "Missing operator at location " ++ prettifyLocation loc
            FixityResolutionErrorUnexpectedOperator op loc ->
                "Unexpected operator " ++
                prettifyOperator op ++ " at location " ++ prettifyLocation loc
            FixityResolutionErrorFixityConflict op1 op2 loc ->
                "Fixity conflict between operators " ++
                prettifyOperator op1 ++
                " and " ++
                prettifyOperator op2 ++ " at location " ++ prettifyLocation loc
            FixityResolutionErrorCannotResolve loc ->
                "Can't resolve expression at location " ++ prettifyLocation loc

prettifyName :: WithLocation Ident -> String
prettifyName (WithLocation ident loc) =
    prettifyIdent ident ++ " (" ++ prettifyLocation loc ++ ")"

prettifyDesugaringError :: DesugaringError -> String
prettifyDesugaringError err =
    "Desugaring error: " ++
    case err of
        DesugaringErrorNameConflict name1 name2 ->
            "Conflicting names: " ++
            prettifyName name1 ++ " and " ++ prettifyName name2
        DesugaringErrorRecord recordError ->
            prettifyRecordDesugaringError recordError
        DesugaringErrorExpression expressionError ->
            prettifyExpressionDesugaringError expressionError

prettifyRecordDesugaringError :: RecordDesugaringError -> String
prettifyRecordDesugaringError recordError =
    "Record desugaring error: " ++
    case recordError of
        RecordDesugaringErrorUnknownField name ->
            "Unknown field " ++ prettifyName name
        RecordDesugaringErrorUnknownConstructor name ->
            "Unknown constructor " ++ prettifyName name
        RecordDesugaringErrorDuplicateField name1 name2 ->
            "Duplicating fields: " ++
            prettifyName name1 ++ " and " ++ prettifyName name2
        RecordDesugaringErrorMissingConstructor fields ->
            "There are no constructors including fields " ++
            intercalate ", " (map prettifyName fields)

prettifyExpressionDesugaringError :: ExpressionDesugaringError -> String
prettifyExpressionDesugaringError expressionError =
    "Expression desugaring error: " ++
    case expressionError of
        ExpressionDesugaringErrorDuplicatedTypeDeclaration name ->
            "Multiple type declarations for the expression " ++
            prettifyName name
        ExpressionDesugaringErrorMissingExpressionDefinition name ->
            "Mising definition for the expression " ++ prettifyName name
        ExpressionDesugaringErrorMissingMethodType name ->
            "Missing type declaration for the expression " ++ prettifyName name
        ExpressionDesugaringErrorDifferentNumberOfArguments name ->
            "Declarations of " ++
            prettifyName name ++ " have different numbers of arguments"
        ExpressionDesugaringErrorIdentifierIsAlreadyDefined name1 name2 ->
            "Expressions share the same name: " ++
            prettifyName name1 ++ " and " ++ prettifyName name2

prettifyInferenceError :: InferenceError -> String
prettifyInferenceError err =
    "Inference error: " ++
    case err of
        InferenceErrorKind kindErr -> prettifyKindInferenceError kindErr
        InferenceErrorTypeSynonyms typeSynonymsErr ->
            prettifyTypeSynonymsProcessingError typeSynonymsErr
        InferenceErrorType typeError -> prettifyTypeInferenceError typeError

prettifyKindInferenceError :: KindInferenceError -> String
prettifyKindInferenceError err =
    "Kind inference error: " ++
    case err of
        KindInferenceErrorDependencyResolution dependencyError ->
            prettifyDependencyError dependencyError
        KindInferenceErrorUnification unificationError ->
            prettifyUnificationError unificationError
        KindInferenceErrorEqualityGeneration groupError ->
            prettifyEqualityGenerationError groupError

prettifyDependencyError :: DependencyResolverError -> String
prettifyDependencyError dependencyError =
    "Dependency resolution error: " ++
    case dependencyError of
        DependencyResolverErrorUnknownNode node ->
            "Unknown identifier " ++ prettifyIdent node

prettifyUnificationError :: UnificationError -> String
prettifyUnificationError unificationError =
    "Unification error: " ++
    case unificationError of
        UnificationErrorFunctionNameMismatch name1 name2 ->
            "Can't unify functions " ++
            prettifyIdent name1 ++ " and " ++ prettifyIdent name2
        UnificationErrorRecursive name _ ->
            "Can't unify recursive expression " ++ prettifyIdent name
        UnificationErrorDifferentNumberOfArgs ->
            "Can't unify functions with different number of arguments"

prettifyEqualityGenerationError :: EqualityGenerationError -> String
prettifyEqualityGenerationError groupError =
    "Equality generation error: " ++
    case groupError of
        EqualityGenerationErrorUnknownVariable name ->
            "Unknown variable " ++ prettifyName name
        EqualityGenerationErrorUnknownType name ->
            "Unknown type " ++ prettifyName name
        EqualityGenerationErrorUnknownClass name ->
            "Unkonwn class " ++ prettifyName name
        EqualityGenerationErrorUnusedTypeVariable name _ ->
            "Class parameter " ++
            prettifyIdent name ++ " is not used in the type signature"

prettifyTypeSynonymsProcessingError :: TypeSynonymsProcessingError -> String
prettifyTypeSynonymsProcessingError typeSynonymsErr =
    "Type synonyms processing error: " ++
    case typeSynonymsErr of
        TypeSynonymsProcessingErrorRecursive name ->
            "Type synonym " ++
            prettifyIdent name ++ " is recursive, which is not supported"
        TypeSynonymsProcessingErrorMutuallyRecursive names ->
            "Type synonyms " ++
            intercalate ", " (map prettifyIdent names) ++
            " are mutually recursive, which is not supported"
        TypeSynonymsProcessingErrorDependencyResolution dependencyError ->
            prettifyDependencyError dependencyError
        TypeSynonymsProcessingErrorExpanding expandingError ->
            prettifyTypeSynonymsExpandingError expandingError

prettifyTypeSynonymsExpandingError :: TypeSynonymsExpandingError -> String
prettifyTypeSynonymsExpandingError (TypeSynonymsExpandingErrorwWrongNumberOfArgs name expected got) =
    "Type synonym expanding error: " ++
    "Wrong number of arguments of a type synonym " ++
    prettifyName name ++
    ", expected " ++ show expected ++ ", but got " ++ show got

prettifyTypeInferenceError :: TypeInferenceError -> String
prettifyTypeInferenceError typeError =
    "Type inference error: " ++
    case typeError of
        TypeInferenceErrorSignature signatureError ->
            prettifySignatureInferenceError signatureError

prettifySignatureInferenceError :: TypeSignatureInferrenceError -> String
prettifySignatureInferenceError signatureError =
    "Signature inference error: " ++
    case signatureError of
        TypeSignatureInferrenceErrorUnification unificationError ->
            prettifyUnificationError unificationError
        TypeSignatureInferrenceErrorEqualityGeneration eqError ->
            prettifyEqualityGenerationError eqError
        TypeSignatureInferrenceErrorTypeSynonyms typeSynonymsErr ->
            prettifyTypeSynonymsExpandingError typeSynonymsErr
