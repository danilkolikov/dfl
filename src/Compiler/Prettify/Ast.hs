{- |
Module      :  Compiler.Prettify.Ast
Description :  Pretty-printing of AST
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Functions for pretty-printing of AST
-}
module Compiler.Prettify.Ast where

import qualified Data.HashMap.Lazy as HM
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE (toList)
import Data.Maybe (fromJust)

import Compiler.Prettify.PrettyPrinter
import Compiler.Prettify.Utils
import qualified Frontend.Desugaring.Final.Ast as F
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token

-- | Clas for objects which can be pretty-printed
class PrettyPrintable a where
    prettyPrint :: a -> PrettyPrinter -- ^ Pretty-print object

-- | Pretty print AST
prettifyAst :: (PrettyPrintable a) => a -> String
prettifyAst a = runPrettyPrinter (prettyPrint a) 4

instance (PrettyPrintable a) => PrettyPrintable (Maybe a) where
    prettyPrint Nothing = emptyLine
    prettyPrint (Just x) = prettyPrint x

instance (PrettyPrintable a, PrettyPrintable b) =>
         PrettyPrintable (Either a b) where
    prettyPrint (Left x) = prettyPrint x
    prettyPrint (Right y) = prettyPrint y

instance PrettyPrintable IntT where
    prettyPrint (IntT int) = singleLine (show int)

instance PrettyPrintable FloatT where
    prettyPrint (FloatT float) = singleLine (show float)

instance PrettyPrintable CharT where
    prettyPrint (CharT char) = singleLine (show char)

instance PrettyPrintable StringT where
    prettyPrint (StringT string) = singleLine (show string)

instance PrettyPrintable Keyword where
    prettyPrint keyword =
        singleLine . fromJust $ lookup keyword inversedKeywords

instance PrettyPrintable Operator where
    prettyPrint operator =
        singleLine . fromJust $ lookup operator inversedOperators

instance PrettyPrintable Special where
    prettyPrint special = singleLine [fromJust $ lookup special inversedSpecial]

instance PrettyPrintable EOF where
    prettyPrint _ = emptyLine

instance PrettyPrintable ConId where
    prettyPrint (ConId name) = singleLine name

instance PrettyPrintable ConSym where
    prettyPrint (ConSym name) = singleLine name

instance PrettyPrintable VarId where
    prettyPrint (VarId name) = singleLine name

instance PrettyPrintable VarSym where
    prettyPrint (VarSym name) = singleLine name

instance (PrettyPrintable a) => PrettyPrintable (Qualified a) where
    prettyPrint (Qualified path x) = do
        printedPath <- mapM prettyPrint path
        printedX <- prettyPrint x
        let printed = printedPath ++ [printedX]
            intercalated = intersperse (singleLine' ".") printed
        joinLines intercalated

instance (PrettyPrintable a) => PrettyPrintable (WithLocation a) where
    prettyPrint = prettyPrint . getValue

instance PrettyPrintable Literal where
    prettyPrint (LiteralInteger x) = prettyPrint x
    prettyPrint (LiteralFloat x) = prettyPrint x
    prettyPrint (LiteralChar x) = prettyPrint x
    prettyPrint (LiteralString x) = prettyPrint x

instance PrettyPrintable Module where
    prettyPrint (ModuleExplicit name exports body) = do
        header <-
            joinPrinters
                [ prettyPrint KeywordModule
                , prettyPrint name
                , case exports of
                      Nothing -> emptyLine
                      Just ex -> inParens $ sepByComma ex
                , prettyPrint KeywordWhere
                ]
        printedBody <- prettyPrint body
        multipleLines [header, printedBody]
    prettyPrint (ModuleImplicit body) = prettyPrint body

instance PrettyPrintable Body where
    prettyPrint (Body imps tops) = do
        printedImps <- mapM prettyPrint imps
        printedTops <- mapM prettyPrint tops
        multipleLines $ printedImps ++ printedTops

instance PrettyPrintable ImpExpList where
    prettyPrint ImpExpAll = inParens (prettyPrint OperatorDDot)
    prettyPrint ImpExpNothing = inParens (emptyLine)
    prettyPrint (ImpExpSome lst) = inParens $ sepByComma $ NE.toList lst

instance PrettyPrintable Export where
    prettyPrint (ExportFunction name) = prettyPrint name
    prettyPrint (ExportDataOrClass name lst) =
        joinPrinters [prettyPrint name, prettyPrint lst]
    prettyPrint (ExportModule name) =
        joinPrinters [prettyPrint KeywordModule, prettyPrint name]

instance PrettyPrintable ImpDecl where
    prettyPrint (ImpDecl qualified name as spec) =
        joinPrinters
            [ prettyPrint KeywordImport
            , singleLine $
              if qualified
                  then "qualified"
                  else ""
            , prettyPrint name
            , case as of
                  Nothing -> emptyLine
                  Just t -> joinPrinters [singleLine "as", prettyPrint t]
            , prettyPrint spec
            ]

instance PrettyPrintable ImpSpec where
    prettyPrint (ImpSpec hiding lst) =
        joinPrinters
            [ singleLine $
              if hiding
                  then "hiding"
                  else ""
            , inParens $ sepByComma lst
            ]

instance PrettyPrintable Import where
    prettyPrint (ImportFunction name) = prettyPrint name
    prettyPrint (ImportDataOrClass name lst) =
        joinPrinters [prettyPrint name, prettyPrint lst]

instance PrettyPrintable TopDecl where
    prettyPrint (TopDeclType name type') =
        joinPrinters
            [ prettyPrint KeywordType
            , prettyPrint name
            , prettyPrint OperatorEq
            , prettyPrint type'
            ]
    prettyPrint (TopDeclData context name constrs deriving') =
        joinPrinters
            [ prettyPrint KeywordData
            , inContext context
            , prettyPrint name
            , case constrs of
                  [] -> emptyLine
                  xs ->
                      joinPrinters
                          [prettyPrint OperatorEq, sepBy OperatorBar xs]
            , inDeriving deriving'
            ]
    prettyPrint (TopDeclNewType context name constr deriving') =
        joinPrinters
            [ prettyPrint KeywordNewType
            , inContext context
            , prettyPrint name
            , prettyPrint OperatorEq
            , prettyPrint constr
            , inDeriving deriving'
            ]
    prettyPrint (TopDeclClass context name param where') =
        joinPrinters
            [ prettyPrint KeywordClass
            , inContext context
            , prettyPrint name
            , prettyPrint param
            , inWhere where'
            ]
    prettyPrint (TopDeclInstance context name inst where') =
        joinPrinters
            [ prettyPrint KeywordInstance
            , inContext context
            , prettyPrint name
            , prettyPrint inst
            , inWhere where'
            ]
    prettyPrint (TopDeclDecl decl) = prettyPrint decl

instance PrettyPrintable Decl where
    prettyPrint (DeclGenDecl decl) = prettyPrint decl
    prettyPrint (DeclFunction lhs rhs) =
        joinPrinters [prettyPrint lhs, prettyPrint rhs]

instance PrettyPrintable CDecl where
    prettyPrint (CDeclGenDecl decl) = prettyPrint decl
    prettyPrint (CDeclFunction lhs rhs) =
        joinPrinters [prettyPrint lhs, prettyPrint rhs]

instance PrettyPrintable IDecl where
    prettyPrint (IDeclFunction lhs rhs) =
        joinPrinters [prettyPrint lhs, prettyPrint rhs]

instance PrettyPrintable GenDecl where
    prettyPrint (GenDeclTypeSig vars context type') =
        joinPrinters
            [ sepByComma (NE.toList vars)
            , prettyPrint OperatorQDot
            , inContext context
            , prettyPrint type'
            ]
    prettyPrint (GenDeclFixity fixity prec ops) =
        joinPrinters
            [prettyPrint fixity, prettyPrint prec, sepByComma (NE.toList ops)]

instance PrettyPrintable Fixity where
    prettyPrint InfixL = prettyPrint KeywordInfixL
    prettyPrint InfixR = prettyPrint KeywordInfixR
    prettyPrint Infix = prettyPrint KeywordInfix

instance PrettyPrintable Type where
    prettyPrint (Type args) = sepBy OperatorRArrow (NE.toList args)

instance PrettyPrintable BType where
    prettyPrint (BType args) = notSep $ NE.toList args

instance PrettyPrintable AType where
    prettyPrint (ATypeConstructor name) = prettyPrint name
    prettyPrint (ATypeVar name) = prettyPrint name
    prettyPrint (ATypeTuple f s rest) = inParens (sepByComma $ f : s : rest)
    prettyPrint (ATypeList arg) = inBrackets $ prettyPrint arg
    prettyPrint (ATypeParens arg) = inParens $ prettyPrint arg

instance PrettyPrintable GTyCon where
    prettyPrint (GTyConNamed name) = prettyPrint name
    prettyPrint GTyConUnit = inParens $ emptyLine
    prettyPrint GTyConList = inBrackets $ emptyLine
    prettyPrint GTyConFunction = inParens $ prettyPrint OperatorRArrow
    prettyPrint (GTyConTuple n) = inParens (singleLine $ replicate (n - 1) ',')

instance PrettyPrintable Class where
    prettyPrint (ClassSimple name var) =
        joinPrinters [prettyPrint name, prettyPrint var]
    prettyPrint (ClassApplied name var args) =
        joinPrinters
            [ prettyPrint name
            , inParens $ joinPrinters [prettyPrint var, notSep (NE.toList args)]
            ]

instance PrettyPrintable SimpleClass where
    prettyPrint (SimpleClass name var) =
        joinPrinters [prettyPrint name, prettyPrint var]

instance PrettyPrintable SimpleType where
    prettyPrint (SimpleType name vars) =
        joinPrinters [prettyPrint name, notSep vars]

instance PrettyPrintable Constr where
    prettyPrint (ConstrSimple name args) =
        joinPrinters [prettyPrint name, notSep args]
    prettyPrint (ConstrInfix l op r) =
        joinPrinters [prettyPrint l, prettyPrint op, prettyPrint r]
    prettyPrint (ConstrRecord name fields) =
        joinPrinters [prettyPrint name, inCurly $ sepByComma fields]

instance PrettyPrintable NewConstr where
    prettyPrint (NewConstrSimple name type') =
        joinPrinters [prettyPrint name, prettyPrint type']
    prettyPrint (NewConstrNamed name getter type') =
        joinPrinters
            [ prettyPrint name
            , inCurly $
              joinPrinters
                  [ prettyPrint getter
                  , prettyPrint OperatorQDot
                  , prettyPrint type'
                  ]
            ]

instance PrettyPrintable FieldDecl where
    prettyPrint (FieldDecl vars type') =
        joinPrinters
            [ sepByComma (NE.toList vars)
            , prettyPrint OperatorQDot
            , prettyPrint type'
            ]

instance PrettyPrintable DClass where
    prettyPrint (DClass name) = prettyPrint name

instance PrettyPrintable Inst where
    prettyPrint (InstNamed name []) = prettyPrint name
    prettyPrint (InstNamed name args) =
        inParens $ joinPrinters [prettyPrint name, notSep args]
    prettyPrint (InstTuple f s rest) = inParens $ sepByComma $ f : s : rest
    prettyPrint (InstList arg) = inBrackets $ prettyPrint arg
    prettyPrint (InstFunction fr to) = inParens $ sepBy OperatorRArrow [fr, to]

instance PrettyPrintable FunLHS where
    prettyPrint (FunLHSSimple name pats) =
        joinPrinters [prettyPrint name, notSep $ NE.toList pats]
    prettyPrint (FunLHSInfix l name r) =
        joinPrinters [prettyPrint l, prettyPrint name, prettyPrint r]
    prettyPrint (FunLHSNested f pats) =
        joinPrinters [inParens $ prettyPrint f, notSep $ NE.toList pats]

instance PrettyPrintable RHS where
    prettyPrint (RHSSimple e where') =
        joinPrinters [prettyPrint OperatorEq, prettyPrint e, inWhere where']
    prettyPrint (RHSGuarded gdrhs where') =
        joinPrinters [notSep $ NE.toList gdrhs, inWhere where']

instance PrettyPrintable GdRHS where
    prettyPrint (GdRHS guards e) =
        joinPrinters
            [ prettyPrint OperatorBar
            , sepByComma $ NE.toList guards
            , prettyPrint OperatorEq
            , prettyPrint e
            ]

instance PrettyPrintable Guard where
    prettyPrint (GuardPattern pat e) =
        joinPrinters
            [prettyPrint pat, prettyPrint OperatorLArrow, prettyPrint e]
    prettyPrint (GuardLet decls) =
        joinPrinters [prettyPrint KeywordLet, inCurly $ sepBySemicolon decls]
    prettyPrint (GuardExpr e) = prettyPrint e

instance PrettyPrintable Exp where
    prettyPrint (ExpTyped e context type') =
        joinPrinters
            [ prettyPrint e
            , prettyPrint OperatorQDot
            , inContext context
            , prettyPrint type'
            ]
    prettyPrint (ExpSimple e) = prettyPrint e

instance PrettyPrintable InfixExp where
    prettyPrint (InfixExpApplication l op r) =
        joinPrinters [prettyPrint l, prettyPrint op, prettyPrint r]
    prettyPrint (InfixExpNegated op e) =
        joinPrinters [prettyPrint op, prettyPrint e]
    prettyPrint (InfixExpLExp e) = prettyPrint e

instance PrettyPrintable LExp where
    prettyPrint (LExpAbstraction pats e) =
        joinPrinters
            [ prettyPrint OperatorBackslash
            , notSep $ NE.toList pats
            , prettyPrint OperatorRArrow
            , prettyPrint e
            ]
    prettyPrint (LExpLet decls e) =
        joinPrinters
            [ prettyPrint KeywordLet
            , inCurly $ sepBySemicolon decls
            , prettyPrint KeywordIn
            , prettyPrint e
            ]
    prettyPrint (LExpIf cond then' else') =
        joinPrinters
            [ prettyPrint KeywordIf
            , prettyPrint cond
            , prettyPrint KeywordThen
            , prettyPrint then'
            , prettyPrint KeywordElse
            , prettyPrint else'
            ]
    prettyPrint (LExpCase e alts) =
        joinPrinters
            [ prettyPrint KeywordCase
            , prettyPrint e
            , prettyPrint KeywordOf
            , inCurly $ sepBySemicolon $ NE.toList alts
            ]
    prettyPrint (LExpDo stmts) =
        joinPrinters
            [prettyPrint KeywordDo, inCurly $ sepBySemicolon $ NE.toList stmts]
    prettyPrint (LExpApplication args) = inParens . notSep $ NE.toList args

instance PrettyPrintable AExp where
    prettyPrint (AExpVariable name) = prettyPrint name
    prettyPrint (AExpConstructor name) = prettyPrint name
    prettyPrint (AExpLiteral lit) = prettyPrint lit
    prettyPrint (AExpParens e) = inParens (prettyPrint e)
    prettyPrint (AExpTuple f s rest) = inParens $ sepByComma $ f : s : rest
    prettyPrint (AExpList args) = inBrackets $ sepByComma $ NE.toList args
    prettyPrint (AExpSequence f s e) =
        inBrackets $
        joinPrinters
            [ prettyPrint f
            , case s of
                  Nothing -> emptyLine
                  Just ts ->
                      joinPrinters [prettyPrint SpecialComma, prettyPrint ts]
            , prettyPrint OperatorDDot
            , prettyPrint e
            ]
    prettyPrint (AExpListCompr e quals) =
        inBrackets $
        joinPrinters
            [ prettyPrint e
            , prettyPrint OperatorBar
            , sepByComma $ NE.toList quals
            ]
    prettyPrint (AExpLeftSection e op) =
        inParens $ joinPrinters [prettyPrint e, prettyPrint op]
    prettyPrint (AExpRightSection op e) =
        inParens $ joinPrinters [prettyPrint op, prettyPrint e]
    prettyPrint (AExpRecordConstr name binds) =
        joinPrinters [prettyPrint name, inCurly $ sepByComma binds]
    prettyPrint (AExpRecordUpdate e binds) =
        joinPrinters [prettyPrint e, inCurly $ sepByComma $ NE.toList binds]

instance PrettyPrintable Qual where
    prettyPrint (QualGenerator pat e) =
        joinPrinters
            [prettyPrint pat, prettyPrint OperatorLArrow, prettyPrint e]
    prettyPrint (QualLet decls) =
        joinPrinters [prettyPrint KeywordLet, inCurly $ sepBySemicolon decls]
    prettyPrint (QualGuard e) = prettyPrint e

instance PrettyPrintable Alt where
    prettyPrint (AltSimple pat e where') =
        joinPrinters
            [ prettyPrint pat
            , prettyPrint OperatorRArrow
            , prettyPrint e
            , inWhere where'
            ]
    prettyPrint (AltGuarded pat gds where') =
        joinPrinters [prettyPrint pat, notSep $ NE.toList gds, inWhere where']

instance PrettyPrintable GdPat where
    prettyPrint (GdPat guards e) =
        joinPrinters
            [ prettyPrint OperatorBar
            , sepByComma $ NE.toList guards
            , prettyPrint OperatorRArrow
            , prettyPrint e
            ]

instance PrettyPrintable Stmt where
    prettyPrint (StmtExp e) = prettyPrint e
    prettyPrint (StmtPat pat e) =
        joinPrinters
            [prettyPrint pat, prettyPrint OperatorLArrow, prettyPrint e]
    prettyPrint (StmtLet decls) =
        joinPrinters [prettyPrint KeywordLet, inCurly $ sepBySemicolon decls]

instance PrettyPrintable FBind where
    prettyPrint (FBind var e) =
        joinPrinters [prettyPrint var, prettyPrint OperatorEq, prettyPrint e]

instance PrettyPrintable Pat where
    prettyPrint (PatInfix l op r) =
        joinPrinters [prettyPrint l, prettyPrint op, prettyPrint r]
    prettyPrint (PatSimple pat) = prettyPrint pat

instance PrettyPrintable LPat where
    prettyPrint (LPatSimple pat) = prettyPrint pat
    prettyPrint (LPatNegated n) = joinPrinters [singleLine "-", prettyPrint n]
    prettyPrint (LPatConstructor name args) =
        inParens $ joinPrinters [prettyPrint name, notSep $ NE.toList args]

instance PrettyPrintable APat where
    prettyPrint (APatVariable var as) =
        joinPrinters
            [ prettyPrint var
            , case as of
                  Nothing -> emptyLine
                  Just ts ->
                      joinPrinters [prettyPrint OperatorAt, prettyPrint ts]
            ]
    prettyPrint (APatConstructor name) = prettyPrint name
    prettyPrint (APatRecord name pats) =
        joinPrinters [prettyPrint name, inCurly $ sepByComma pats]
    prettyPrint (APatLiteral l) = prettyPrint l
    prettyPrint APatWildcard = prettyPrint KeywordUnderscore
    prettyPrint (APatParens pat) = inParens (prettyPrint pat)
    prettyPrint (APatTuple f s rest) = inParens $ sepByComma $ f : s : rest
    prettyPrint (APatList pats) = inBrackets $ sepByComma $ NE.toList pats

instance PrettyPrintable FPat where
    prettyPrint (FPat var pat) =
        joinPrinters [prettyPrint var, prettyPrint OperatorEq, prettyPrint pat]

instance PrettyPrintable GCon where
    prettyPrint GConUnit = inParens (emptyLine)
    prettyPrint GConList = inBrackets (emptyLine)
    prettyPrint (GConTuple n) = inParens $ singleLine $ replicate (n - 1) ','
    prettyPrint (GConNamed wl) = prettyPrint wl

instance (PrettyPrintable a, PrettyPrintable b) =>
         PrettyPrintable (FuncLabel a b) where
    prettyPrint (FuncLabelId name) = prettyPrint name
    prettyPrint (FuncLabelSym sym) = inParens (prettyPrint sym)

instance (PrettyPrintable a, PrettyPrintable b) =>
         PrettyPrintable (OpLabel a b) where
    prettyPrint (OpLabelSym sym) = prettyPrint sym
    prettyPrint (OpLabelId name) = inBackticks (prettyPrint name)

instance PrettyPrintable GConSym where
    prettyPrint GConSymColon = singleLine ":"
    prettyPrint (GConSymOp sym) = prettyPrint sym

-- Desugared ast
instance PrettyPrintable F.Ident where
    prettyPrint = singleLine . prettifyIdent

instance PrettyPrintable F.Const where
    prettyPrint cnst =
        singleLine $
        case cnst of
            F.ConstInt i -> show i
            F.ConstFloat f -> show f
            F.ConstChar c -> show c
            F.ConstString s -> show s

instance PrettyPrintable F.Export where
    prettyPrint export =
        case export of
            F.ExportFunction name -> prettyPrint name
            F.ExportModule name -> prettyPrint name
            F.ExportDataOrClass name list ->
                joinPrinters [prettyPrint name, prettyPrint list]

instance (PrettyPrintable a) => PrettyPrintable (F.ImpExpList a) where
    prettyPrint list =
        case list of
            F.ImpExpNothing -> singleLine "()"
            F.ImpExpAll -> singleLine "(..)"
            F.ImpExpSome elems -> inParens . sepByComma . NE.toList $ elems

instance PrettyPrintable F.ImpDecl where
    prettyPrint (F.ImpDecl qualified name asName hiding list) =
        joinPrinters
            [ singleLine "import"
            , singleLine $
              if qualified
                  then "qualified"
                  else ""
            , prettyPrint name
            , case asName of
                  Nothing -> emptyLine
                  Just synonym ->
                      joinPrinters [singleLine "as", prettyPrint synonym]
            , singleLine $
              if hiding
                  then "hiding"
                  else ""
            , prettyPrint list
            ]

instance PrettyPrintable F.Import where
    prettyPrint imp =
        case imp of
            F.ImportFunction name -> prettyPrint name
            F.ImportDataOrClass name list ->
                joinPrinters [prettyPrint name, prettyPrint list]

instance PrettyPrintable F.Type where
    prettyPrint type' =
        case type' of
            F.TypeVar name -> prettyPrint name
            F.TypeConstr name -> prettyPrint name
            F.TypeFunction from to ->
                joinPrinters [prettyPrint from, singleLine "->", prettyPrint to]
            F.TypeApplication func args ->
                inParens $ notSep (func : NE.toList args)

printIndentedMap :: (PrettyPrintable a) => HM.HashMap b a -> PrettyPrinter
printIndentedMap =
    withIndent . multiplePrinters . map (prettyPrint . snd) . HM.toList

instance PrettyPrintable F.Exp where
    prettyPrint exp' =
        case exp' of
            F.ExpVar name -> prettyPrint name
            F.ExpConst c -> prettyPrint c
            F.ExpConstr name -> prettyPrint name
            F.ExpAbstraction ident inner ->
                joinPrinters
                    [ prettyPrint OperatorBackslash
                    , prettyPrint ident
                    , prettyPrint OperatorRArrow
                    , prettyPrint inner
                    ]
            F.ExpApplication func args ->
                inParens . notSep $ func : NE.toList args
            F.ExpLet decls inner ->
                joinPrinters
                    [ prettyPrint KeywordLet
                    , newLine
                    , printIndentedMap decls
                    , newLine
                    , prettyPrint KeywordIn
                    , prettyPrint inner
                    ]
            F.ExpCase var name args ifSuccess ifFail ->
                joinPrinters
                    [ prettyPrint KeywordCase
                    , prettyPrint var
                    , prettyPrint KeywordOf
                    , newLine
                    , withIndent $
                      multiplePrinters
                          [ joinPrinters
                                [ notSep $ name : args
                                , prettyPrint OperatorRArrow
                                , prettyPrint ifSuccess
                                ]
                          , joinPrinters
                                [ prettyPrint KeywordUnderscore
                                , prettyPrint OperatorRArrow
                                , prettyPrint ifFail
                                ]
                          ]
                    ]

instance PrettyPrintable F.Expression where
    prettyPrint (F.Expression name body Nothing) =
        joinPrinters
            [prettyPrint name, prettyPrint OperatorEq, prettyPrint body]
    prettyPrint (F.Expression name body (Just sig)) =
        multiplePrinters
            [ joinPrinters
                  [prettyPrint name, prettyPrint OperatorQDot, prettyPrint sig]
            , prettyPrint (F.Expression name body Nothing)
            ]

instance PrettyPrintable F.Method where
    prettyPrint (F.Method name type' Nothing) =
        joinPrinters
            [prettyPrint name, prettyPrint OperatorQDot, prettyPrint type']
    prettyPrint (F.Method name type' (Just body)) =
        multiplePrinters
            [ prettyPrint (F.Method name type' Nothing)
            , joinPrinters
                  [prettyPrint name, prettyPrint OperatorEq, prettyPrint body]
            ]

instance PrettyPrintable F.TypeSignature where
    prettyPrint (F.TypeSignature context type') =
        joinPrinters [inContext context, prettyPrint type']

instance PrettyPrintable F.Constraint where
    prettyPrint constraint =
        case constraint of
            F.ConstraintParam class' param -> notSep [class', param]
            F.ConstraintType class' type' params ->
                joinPrinters
                    [ prettyPrint class'
                    , inParens $
                      joinPrinters
                          [prettyPrint type', notSep $ NE.toList params]
                    ]

instance PrettyPrintable F.SimpleConstraint where
    prettyPrint (F.SimpleConstraint class' param) = notSep [class', param]

instance PrettyPrintable F.Instance where
    prettyPrint (F.Instance context class' type' args methods) =
        joinPrinters
            [ prettyPrint KeywordInstance
            , inContext context
            , prettyPrint class'
            , prettyPrint type'
            , notSep args
            , newLine
            , printIndentedMap methods
            ]

instance PrettyPrintable F.Class where
    prettyPrint (F.Class context class' param methods) =
        joinPrinters
            [ prettyPrint KeywordClass
            , inContext context
            , prettyPrint class'
            , prettyPrint param
            , newLine
            , printIndentedMap methods
            ]

instance PrettyPrintable F.Constructor where
    prettyPrint (F.Constructor name args fields) =
        joinPrinters [prettyPrint name, notSep args, singleLine $ show fields]

instance PrettyPrintable F.DataType where
    prettyPrint (F.DataType context name params deriving' constructors isNewType) =
        joinPrinters
            [ prettyPrint $
              if isNewType
                  then KeywordNewType
                  else KeywordData
            , inContext context
            , prettyPrint name
            , notSep params
            , inDeriving deriving'
            , newLine
            , printIndentedMap (HM.fromList constructors)
            ]

instance PrettyPrintable F.TypeSynonym where
    prettyPrint (F.TypeSynonym name params type') =
        joinPrinters
            [ prettyPrint KeywordType
            , prettyPrint name
            , notSep params
            , prettyPrint OperatorEq
            , prettyPrint type'
            ]

instance PrettyPrintable F.Module where
    prettyPrint (F.Module name exports imports typeSynonyms dataTypes classes instances exps) =
        joinPrinters
            [ prettyPrint KeywordModule
            , prettyPrint name
            , prettyPrint exports
            , multiplePrinters $ map prettyPrint imports
            , newLine
            , printIndentedMap typeSynonyms
            , newLine
            , printIndentedMap dataTypes
            , newLine
            , printIndentedMap classes
            , newLine
            , printIndentedMap instances
            , newLine
            , printIndentedMap exps
            ]

-- Helper functions
-- | Wrap object in 2 parenthesis
inParens :: PrettyPrinter -> PrettyPrinter
inParens printer =
    joinPrinters [prettyPrint SpecialLParen, printer, prettyPrint SpecialRParen]

-- | Wrap object in 2 curly brackets
inCurly :: PrettyPrinter -> PrettyPrinter
inCurly printer = do
    res <- withIndent printer
    multipleLines [singleLine' "", res]

-- | Wrap object in 2 square brackets
inBrackets :: PrettyPrinter -> PrettyPrinter
inBrackets printer =
    joinPrinters
        [prettyPrint SpecialLBracket, printer, prettyPrint SpecialRBracket]

-- | Wrap object in 2 backticks
inBackticks :: PrettyPrinter -> PrettyPrinter
inBackticks printer =
    joinPrinters
        [prettyPrint SpecialBacktick, printer, prettyPrint SpecialBacktick]

-- | Separate object by a custom separator
sepBy :: (PrettyPrintable a, PrettyPrintable b) => a -> [b] -> PrettyPrinter
sepBy _ [] = emptyLine
sepBy c xs = do
    separator <- prettyPrint c
    elems <- mapM prettyPrint xs
    joinLines $ intersperse separator elems

-- | Separate objects by comma
sepByComma :: (PrettyPrintable a) => [a] -> PrettyPrinter
sepByComma = sepBy SpecialComma

-- | Separate objects by semicolon
sepBySemicolon :: (PrettyPrintable a) => [a] -> PrettyPrinter
sepBySemicolon args = mapM prettyPrint args >>= multipleLines

-- | Not separated list of objects
notSep :: (PrettyPrintable a) => [a] -> PrettyPrinter
notSep = joinPrinters . map prettyPrint

-- | Object in a type context
inContext :: (PrettyPrintable a) => [a] -> PrettyPrinter
inContext list =
    case list of
        [] -> emptyLine
        [x] -> joinPrinters [prettyPrint x, prettyPrint OperatorBoldRArrow]
        xs ->
            joinPrinters
                [inParens (sepByComma xs), prettyPrint OperatorBoldRArrow]

-- | Where block
inWhere :: (PrettyPrintable a) => [a] -> PrettyPrinter
inWhere [] = emptyLine
inWhere xs =
    joinPrinters [prettyPrint KeywordWhere, inCurly (sepBySemicolon xs)]

-- | Expression in a deriving block
inDeriving :: (PrettyPrintable a) => [a] -> PrettyPrinter
inDeriving list =
    case list of
        [] -> emptyLine
        [x] -> joinPrinters [prettyPrint KeywordDeriving, prettyPrint x]
        xs ->
            joinPrinters [prettyPrint KeywordDeriving, inParens (sepByComma xs)]
