{- |
Module      :  Compiler.Prettify.CompilationError
Description :  Prettifying of CompilationError
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty printing of CompilationError
-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Compiler.Prettify.CompilationError where

import Data.List (intercalate)

import Compiler.Module.Base
import Compiler.Prettify.TokenStream (prettifyToken)
import Compiler.Prettify.Utils
import Frontend.Desugaring.Checking.Base
import Frontend.Desugaring.Final.Ast (Ident)
import Frontend.Desugaring.Fixity.Base
import Frontend.Desugaring.Grouping.Base
import Frontend.Desugaring.Processor
import Frontend.Desugaring.Record.Base
import Frontend.HeaderProcessor
import Frontend.Inference.Equalities
import Frontend.Inference.InferenceProcessor (VariableBinding)
import Frontend.Inference.Processor
import Frontend.Inference.TypeSynonym.Expand (TypeSynonymExpandingError(..))
import Frontend.Inference.Unification
import Frontend.Module.Import.Processor
import Frontend.Processor
import Frontend.Syntax.Lexer
import Frontend.Syntax.Position
import Frontend.Syntax.Processor
import Frontend.Syntax.Token (VarId(..))
import Util.DependencyResolver

instance Prettifiable FrontendProcessorError where
    prettify err =
        "Frontend processor error: " ++
        case err of
            FrontendProcessorErrorSyntax syntaxErr -> prettify syntaxErr
            FrontendProcessorErrorDesugaring desugaringError ->
                prettify desugaringError
            FrontendProcessorErrorInference infErr -> prettify infErr

instance Prettifiable SyntaxProcessorError where
    prettify err =
        "Syntax processor error: " ++
        case err of
            SyntaxProcessorErrorParser parserError -> prettify parserError
            SyntaxProcessorErrorChecker checkerError -> prettify checkerError

instance Prettifiable HeaderProcessorError where
    prettify err =
        "Header processor error: " ++
        case err of
            HeaderProcessorErrorLexer lexerError -> prettify lexerError
            HeaderProcessorErrorParser parserError -> prettify parserError

instance Prettifiable LexerError where
    prettify err =
        "Lexer error: " ++
        case err of
            LexerErrorLexer pos tok expected ->
                "Unexpected character " ++
                maybe "" (\t -> show t ++ " ") tok ++
                "at position " ++
                prettify pos ++ ", expected " ++ intercalate ", " expected
            LexerErrorLayout layoutError -> prettify layoutError
            LexerErrorUnexpected pos ->
                "Unexpected lexer error at position " ++ prettify pos

instance Prettifiable LayoutError where
    prettify layoutError =
        "Layout error: " ++
        case layoutError of
            LayoutErrorRedundantClosingBracket loc ->
                "Redundant closing bracket at location " ++ prettify loc
            LayoutErrorMissingClosingBracket ->
                "Missing closing bracket at the end of the file"

instance Prettifiable ParserError where
    prettify err =
        "Parser error: " ++
        case err of
            ParserErrorParser loc tok expected ->
                "Unexpected token " ++
                maybe "" (\t -> prettifyToken (getValue t) ++ " ") tok ++
                "at location " ++
                prettify loc ++ ", expected " ++ intercalate ", " expected
            ParserErrorUnexpected pos ->
                "Unexpected parser error at position " ++ prettify pos

instance Prettifiable AstCheckerError where
    prettify checkerError =
        "Ast checker error: " ++
        case checkerError of
            AstCheckerErrorSameTyVar first second ->
                let prettifyTyVar (WithLocation (VarId ident) loc) =
                        ident ++ "(" ++ prettify loc ++ ")"
                 in "Type variables should be different: " ++
                    prettifyTyVar first ++ " and " ++ prettifyTyVar second
            AstCheckerErrorLastStatement (WithLocation _ loc) ->
                "Last statement in do block shouldn't be let or pattern assignment (" ++
                prettify loc ++ ")"

instance Prettifiable FixityResolutionError where
    prettify err =
        "Fixity resolution error: " ++
        case err of
            FixityResolutionErrorMissingOperand loc ->
                "Missing operand at location" ++ prettify loc
            FixityResolutionErrorMissingOperator loc ->
                "Missing operator at location " ++ prettify loc
            FixityResolutionErrorUnexpectedOperator op ->
                "Unexpected operator " ++ prettifyName op
            FixityResolutionErrorFixityConflict op2 ->
                "Fixity conflict between operator " ++
                prettifyName op2 ++ " and the previous one"
            FixityResolutionErrorCannotResolve loc ->
                "Can't resolve expression at location " ++ prettify loc

prettifyName :: WithLocation Ident -> String
prettifyName (WithLocation ident loc) =
    prettify ident ++ " (" ++ prettify loc ++ ")"

instance Prettifiable DesugaringError where
    prettify err =
        "Desugaring error: " ++
        case err of
            DesugaringErrorRecord recordError -> prettify recordError
            DesugaringErrorGrouping groupingError -> prettify groupingError
            DesugaringErrorChecking checkingError -> prettify checkingError
            DesugaringErrorFixity fixityError -> prettify fixityError

instance Prettifiable RecordDesugaringError where
    prettify recordError =
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

instance Prettifiable GroupingProcessorError where
    prettify expressionError =
        "Grouping error: " ++
        case expressionError of
            GroupingProcessorErrorDuplicatedTypeDeclaration name ->
                "Multiple type declarations for the expression " ++
                prettifyName name
            GroupingProcessorErrorDuplicatedFixityDeclaration name ->
                "Multiple fixity declarations for the expression " ++
                prettifyName name
            GroupingProcessorErrorMissingExpressionDefinition name ->
                "Mising definition for the expression " ++ prettifyName name
            GroupingProcessorErrorMissingMethodType name ->
                "Missing type declaration for the expression " ++
                prettifyName name
            GroupingProcessorErrorDifferentNumberOfArguments name ->
                "Declarations of " ++
                prettifyName name ++ " have different numbers of arguments"
            GroupingProcessorErrorNameConflict name1 name2 ->
                "Expressions share the same name: " ++
                prettifyName name1 ++ " and " ++ prettifyName name2
            GroupingProcessorErrorUnexpectedFixitySignature name ->
                "Fixity signature of " ++
                prettifyName name ++ " has no corresponding definition."

instance Prettifiable CheckingError where
    prettify err =
        "Ambiguity checking error: " ++
        case err of
            CheckingErrorUndefinedName name ->
                "Undefined name: " ++ prettify name
            CheckingErrorAmbiguousName name sources ->
                "Name " ++
                prettify name ++
                " is ambiguous. It's defined at: " ++
                intercalate ", " (map prettify sources)

instance Prettifiable AmbiguitySource where
    prettify source =
        case source of
            AmbiguitySourceThisFile name ->
                "this file (" ++ prettify name ++ ")"
            AmbiguitySourceImport name -> "module " ++ prettify name

instance Prettifiable InferenceProcessorError where
    prettify err =
        "Inference error: " ++
        case err of
            InferenceProcessorErrorKind kindError -> prettify kindError
            InferenceProcessorErrorTypeSynonym typeSynonymError ->
                prettify typeSynonymError
            InferenceProcessorErrorClass classError -> prettify classError
            InferenceProcessorErrorDataType dataType -> prettify dataType
            InferenceProcessorErrorInstance instErr -> prettify instErr
            InferenceProcessorErrorType infErr ->
                prettifyInferenceError "Type inference error: " infErr
            InferenceProcessorErrorTranslation translationError ->
                prettify translationError

instance Prettifiable KindProcessorError where
    prettify err =
        "Kind inference error:" ++
        case err of
            KindProcessorErrorInference infErr ->
                prettifyInferenceError "Kind inference error: " infErr
            KindProcessorErrorInstanceCheck instErr ->
                prettifyInferenceError "Instance kind checking error: " instErr
            KindProcessorErrorAstCheck checkErr ->
                prettifyInferenceError "AST checking error: " checkErr

instance Prettifiable ClassProcessorError where
    prettify err =
        "Class error: " ++
        case err of
            ClassProcessorErrorRecursive name ->
                "Class " ++
                prettify name ++ " should not be a superclass of itself"
            ClassProcessorErrorMutuallyRecursive classes ->
                "Classes " ++
                intercalate ", " (map prettify classes) ++
                " form recursive superclass hierarchy"
            ClassProcessorErrorDependencyResolution depErr -> prettify depErr
            ClassProcessorErrorUnknownClass name ->
                "Unknown class: " ++ prettify name
            ClassProcessorErrorUnknownGeneratedDataType name ->
                "Unknown generated data type: " ++ prettify name

instance Prettifiable DataTypeProcessorError where
    prettify err =
        "Data type error: " ++
        case err of
            DataTypeProcessorErrorUnknownType name ->
                "Unknown data type: " ++ prettify name
            DataTypeProcessorErrorCanNotDerive name ->
                "Can't derive an instance of the class " ++ prettify name

instance Prettifiable InstanceProcessorError where
    prettify err =
        "Instance error: " ++
        case err of
            InstanceProcessorErrorAlreadyDefined old new ->
                "Instance " ++
                prettifyName new ++ " is already defined at " ++ prettify old
            InstanceProcessorErrorUnknownClass name ->
                "Unknown class: " ++ prettify name
            InstanceProcessorErrorUnknownType name ->
                "Unknown generated data type: " ++ prettify name
            InstanceProcessorErrorUnknownClassComponent className compName ->
                "Unknown component " ++
                prettify compName ++ " of a class " ++ prettify className
            InstanceProcessorErrorMissingInstance className typeName ->
                "Instance of the class " ++
                prettify className ++
                " with type " ++ prettify typeName ++ " is not defined"
            InstanceProcessorErrorUnsatisfiedConstraint instName className ->
                "Constraint " ++
                prettify className ++
                " of the instance " ++ prettify instName ++ " is not satisfied"

instance Prettifiable TranslationProcessorError where
    prettify err =
        "Translation error: " ++
        case err of
            TranslationProcessorErrorUnknownExpression name ->
                "Unknown expression: " ++ prettify name
            TranslationProcessorErrorUnknownClass name ->
                "Unknown class: " ++ prettify name
            TranslationProcessorErrorUnsatisfiedConstraint cls var ->
                "Constraint " ++
                prettify cls ++
                " of variable " ++ prettify var ++ " is not satisfied"
            TranslationProcessorErrorMissingInstance cls inst ->
                "Instance of the class " ++
                prettify cls ++
                " with type " ++ prettify inst ++ " is not defined"
            TranslationProcessorErrorArgsLengthMismatch inst expected got ->
                "Mismatching length of arguments for the instance " ++
                prettify inst ++
                ", expected " ++ show expected ++ ", but got " ++ show got

prettifyInferenceError :: String -> InferenceError -> String
prettifyInferenceError label err =
    label ++
    case err of
        InferenceErrorDependencyResolution dependencyError ->
            prettify dependencyError
        InferenceErrorUnification unificationError -> prettify unificationError
        InferenceErrorEqualityGeneration groupError -> prettify groupError
        InferenceErrorSignatureCheck checkErr -> prettify checkErr

instance Prettifiable SignatureCheckError where
    prettify err =
        "Signature check error: " ++
        case err of
            SignatureCheckErrorBoundVariable boundVar ->
                "A free variable was bound in the type of expression: " ++
                prettifyBoundVariable boundVar
            SignatureCheckErrorUnexpectedConstraint constraint ->
                "Unexpected constraint " ++ prettify constraint

prettifyBoundVariable :: VariableBinding -> String
prettifyBoundVariable (name, binding) =
    prettify name ++ " := " ++ prettify binding

instance (Prettifiable a) => Prettifiable (DependencyResolverError a) where
    prettify dependencyError =
        "Dependency resolution error: " ++
        case dependencyError of
            DependencyResolverErrorUnknownNode node ->
                "Unknown identifier " ++ prettify node

instance Prettifiable UnificationError where
    prettify unificationError =
        "Unification error: " ++
        case unificationError of
            UnificationErrorFunctionNameMismatch name1 name2 ->
                "Can't unify functions " ++
                prettify name1 ++ " and " ++ prettify name2
            UnificationErrorRecursive name _ ->
                "Can't unify recursive expression " ++ prettify name
            UnificationErrorDifferentNumberOfArgs ->
                "Can't unify functions with different number of arguments"

instance Prettifiable EqualitiesGenerationError where
    prettify groupError =
        "Equality generation error: " ++
        case groupError of
            EqualitiesGenerationErrorUnknownName name ->
                "Unknown name " ++ prettifyName name
            EqualitiesGenerationErrorUnknownType name ->
                "Unknown type " ++ prettifyName name

instance Prettifiable TypeSynonymProcessorError where
    prettify typeSynonymsErr =
        "Type synonyms processing error: " ++
        case typeSynonymsErr of
            TypeSynonymProcessorErrorRecursive name ->
                "Type synonym " ++
                prettify name ++ " is recursive, which is not supported"
            TypeSynonymProcessorErrorMutuallyRecursive names ->
                "Type synonyms " ++
                intercalate ", " (map prettify names) ++
                " are mutually recursive, which is not supported"
            TypeSynonymProcessorErrorDependencyResolution dependencyError ->
                prettify dependencyError
            TypeSynonymProcessorErrorExpanding expandingError ->
                prettify expandingError

instance Prettifiable TypeSynonymExpandingError where
    prettify expErr =
        "Type synonym expanding error: " ++
        case expErr of
            TypeSynonymExpandingErrorwWrongNumberOfArgs name expected got ->
                "Wrong number of arguments of a type synonym " ++
                prettify name ++
                ", expected " ++ show expected ++ ", but got " ++ show got
            TypeSynonymExpandingErrorSynonymInInstance className typeName ->
                "Type synonym " ++
                prettifyName typeName ++
                " is used in the instance of class " ++
                prettifyName className ++ ", which is not supported"

instance Prettifiable DependencyBuilderError where
    prettify err =
        "Module dependency error: " ++
        case err of
            DependencyBuilderErrorCycle cycles ->
                "Files " ++ show cycles ++ " has a cycle in imports"
            DependencyBuilderErrorMissingFile name -> "Missing file: " ++ name
            DependencyBuilderErrorMismatchingModuleName name ->
                "Name of the module, declared in " ++
                name ++ " doesn't match the file name"

instance Prettifiable ImportProcessorError where
    prettify err =
        "Imports processor error: " ++
        case err of
            ImportProcessorErrorUnknownModule name ->
                "Unknown module: " ++ prettify name
