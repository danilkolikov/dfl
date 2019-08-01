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
import Frontend.Inference.Base.Common
import Frontend.Inference.DependencyResolver
import Frontend.Inference.Equalities
import Frontend.Inference.Processor
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
        CompilerErrorInference i -> prettifyCombinedInferenceError i

prettifyLexerError :: LexicalError -> String
prettifyLexerError err =
    "Lexer error: " ++
    case err of
        LexicalErrorLexer pos tok expected ->
            "Unexpected character " ++
            maybe "" (\t -> show t ++ " ") tok ++
            "at position " ++
            prettify pos ++ ", expected " ++ intercalate ", " expected
        LexicalErrorLayout layoutError ->
            "Layout error: " ++ prettifyLayoutError layoutError
        LexicalErrorUnexpected pos ->
            "Unexpected lexer error at position " ++ prettify pos

prettifyLayoutError :: LayoutError -> String
prettifyLayoutError layoutError =
    case layoutError of
        LayoutErrorRedundantClosingBracket loc ->
            "Redundant closing bracket at location " ++ prettify loc
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
            prettify loc ++ ", expected " ++ intercalate ", " expected
        ParserErrorChecker checkerError ->
            "Ast checker error: " ++ prettifyAstCheckerError checkerError
        ParserErrorUnexpected pos ->
            "Unexpected parser error at position " ++ prettify pos

prettifyAstCheckerError :: AstCheckerError -> String
prettifyAstCheckerError checkerError =
    case checkerError of
        AstCheckerErrorSameTyVar first second ->
            let prettifyTyVar (WithLocation (VarId ident) loc) =
                    ident ++ "(" ++ prettify loc ++ ")"
             in "Type variables should be different: " ++
                prettifyTyVar first ++ " and " ++ prettifyTyVar second
        AstCheckerErrorLastStatement (WithLocation _ loc) ->
            "Last statement in do block shouldn't be let or pattern assignment (" ++
            prettify loc ++ ")"

prettifyFixityError :: FixityResolutionError -> String
prettifyFixityError err =
    let prettifyOperator (InfixOperator op _ _) = prettifyEntityName op
     in "Fixity resolution error: " ++
        case err of
            FixityResolutionErrorRedefinedOperator op _ loc ->
                "Redefinition of operator " ++
                prettifyOperator op ++ " at location " ++ prettify loc
            FixityResolutionErrorMissingOperand op loc ->
                "Missing operand of operator " ++
                prettifyOperator op ++ " at location " ++ prettify loc
            FixityResolutionErrorMissingOperator loc ->
                "Missing operator at location " ++ prettify loc
            FixityResolutionErrorUnexpectedOperator op loc ->
                "Unexpected operator " ++
                prettifyOperator op ++ " at location " ++ prettify loc
            FixityResolutionErrorFixityConflict op1 op2 loc ->
                "Fixity conflict between operators " ++
                prettifyOperator op1 ++
                " and " ++
                prettifyOperator op2 ++ " at location " ++ prettify loc
            FixityResolutionErrorCannotResolve loc ->
                "Can't resolve expression at location " ++ prettify loc

prettifyName :: WithLocation Ident -> String
prettifyName (WithLocation ident loc) =
    prettify ident ++ " (" ++ prettify loc ++ ")"

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

prettifyCombinedInferenceError :: CombinedInferenceError -> String
prettifyCombinedInferenceError err =
    case err of
        CombinedInferenceErrorKindInference infErr ->
            prettifyInferenceError "Kind inference error: " infErr
        CombinedInferenceErrorTypeSynonyms typeSynonymsErr ->
            prettifyTypeSynonymsProcessingError typeSynonymsErr
        CombinedInferenceErrorTypeInference infErr ->
            prettifyInferenceError "Type inference error: " infErr

prettifyInferenceError :: String -> InferenceError -> String
prettifyInferenceError label err =
    label ++
    case err of
        InferenceErrorSynonyms expErr ->
            prettifyTypeSynonymsExpandingError expErr
        InferenceErrorDependencyResolution dependencyError ->
            prettifyDependencyError dependencyError
        InferenceErrorUnification unificationError ->
            prettifyUnificationError unificationError
        InferenceErrorEqualityGeneration groupError ->
            prettifyEqualitiesGenerationError groupError

prettifyDependencyError :: DependencyResolverError -> String
prettifyDependencyError dependencyError =
    "Dependency resolution error: " ++
    case dependencyError of
        DependencyResolverErrorUnknownNode node ->
            "Unknown identifier " ++ prettify node

prettifyUnificationError :: UnificationError -> String
prettifyUnificationError unificationError =
    "Unification error: " ++
    case unificationError of
        UnificationErrorFunctionNameMismatch name1 name2 ->
            "Can't unify functions " ++
            prettify name1 ++ " and " ++ prettify name2
        UnificationErrorRecursive name _ ->
            "Can't unify recursive expression " ++ prettify name
        UnificationErrorDifferentNumberOfArgs ->
            "Can't unify functions with different number of arguments"

prettifyEqualitiesGenerationError ::
       InferenceEqualitiesGenerationError -> String
prettifyEqualitiesGenerationError groupError =
    "Equality generation error: " ++
    case groupError of
        EqualitiesGenerationErrorUnknownName name ->
            "Unknown name " ++ prettifyName name
        EqualitiesGenerationErrorUnknownType name ->
            "Unknown type " ++ prettifyName name
        EqualitiesGenerationErrorNested err ->
            "Nested error: " ++ prettifyInferenceError "" err

prettifyTypeSynonymsProcessingError :: TypeSynonymsProcessingError -> String
prettifyTypeSynonymsProcessingError typeSynonymsErr =
    "Type synonyms processing error: " ++
    case typeSynonymsErr of
        TypeSynonymsProcessingErrorRecursive name ->
            "Type synonym " ++
            prettify name ++ " is recursive, which is not supported"
        TypeSynonymsProcessingErrorMutuallyRecursive names ->
            "Type synonyms " ++
            intercalate ", " (map prettify names) ++
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
