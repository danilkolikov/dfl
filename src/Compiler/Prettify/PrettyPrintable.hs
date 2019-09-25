{- |
Module      :  Compiler.Prettify.PrettyPrintable
Description :  Class for pretty printing
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

A class for pretty printing
-}
module Compiler.Prettify.PrettyPrintable where

import Data.List (intersperse)
import Data.Maybe (fromJust)

import Compiler.Prettify.PrettyPrinter
import Compiler.Prettify.Utils
import Frontend.Desugaring.Final.Ast (Const(..), Ident)
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token

-- | Class for objects which can be pretty-printed
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

instance PrettyPrintable GCon where
    prettyPrint GConUnit = inParens emptyLine
    prettyPrint GConList = inBrackets emptyLine
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

instance PrettyPrintable Ident where
    prettyPrint = singleLine . prettify

instance PrettyPrintable Const where
    prettyPrint cnst =
        singleLine $
        case cnst of
            ConstInt i -> show i
            ConstFloat f -> show f
            ConstChar c -> show c
            ConstString s -> show s

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
