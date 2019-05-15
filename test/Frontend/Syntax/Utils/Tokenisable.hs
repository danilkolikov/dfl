{- |
Module      :  Frontend.Syntax.Utils.Tokenisable
Description :  Conversion of AST to tokens
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module converts abstract syntax trees to tokens.
-}
module Frontend.Syntax.Utils.Tokenisable where

import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE (toList)

import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token

-- | Class for types which can be converted to tokens.
class Tokenisable a where
    toTokens :: a -> [Token] -- ^ Convert object to a list of tokens

instance Tokenisable Token where
    toTokens = return

instance (Tokenisable a) => Tokenisable (Maybe a) where
    toTokens Nothing = []
    toTokens (Just x) = toTokens x

instance (Tokenisable a, Tokenisable b) => Tokenisable (Either a b) where
    toTokens (Left x) = toTokens x
    toTokens (Right y) = toTokens y

instance Tokenisable IntT where
    toTokens = tokenContainsToTokens

instance Tokenisable FloatT where
    toTokens = tokenContainsToTokens

instance Tokenisable CharT where
    toTokens = tokenContainsToTokens

instance Tokenisable StringT where
    toTokens = tokenContainsToTokens

instance Tokenisable Keyword where
    toTokens = tokenContainsToTokens

instance Tokenisable Operator where
    toTokens = tokenContainsToTokens

instance Tokenisable Special where
    toTokens = tokenContainsToTokens

instance Tokenisable EOF where
    toTokens = tokenContainsToTokens

instance Tokenisable ConId where
    toTokens name = [TokenName [] (NameConId name)]

instance Tokenisable ConSym where
    toTokens name = [TokenName [] (NameConSym name)]

instance Tokenisable VarId where
    toTokens name = [TokenName [] (NameVarId name)]

instance Tokenisable VarSym where
    toTokens name = [TokenName [] (NameVarSym name)]

-- Hacky instance that supports only 4 types
instance (Tokenisable a) => Tokenisable (Qualified a) where
    toTokens (Qualified path x) =
        case toTokens x of
            [TokenName [] name] -> [TokenName path name]
            _ -> undefined

instance (Tokenisable a) => Tokenisable (WithLocation a) where
    toTokens = toTokens . getValue

instance Tokenisable Literal where
    toTokens (LiteralInteger x) = toTokens x
    toTokens (LiteralFloat x) = toTokens x
    toTokens (LiteralChar x) = toTokens x
    toTokens (LiteralString x) = toTokens x

instance Tokenisable Module where
    toTokens (ModuleExplicit name exports body) =
        [TokenKeyword KeywordModule] ++
        toTokens name ++
        toTokens (fmap (inParens . sepByComma) exports) ++
        [TokenKeyword KeywordWhere] ++ toTokens body
    toTokens (ModuleImplicit body) = toTokens body

instance Tokenisable Body where
    toTokens (Body imps) =
        toTokens
            (inCurly $
             Tokens $
             intercalate [TokenSpecial SpecialSemicolon] (map toTokens imps))

instance Tokenisable ImpExpList where
    toTokens ImpExpAll = toTokens (inParens $ TokenOperator OperatorDDot)
    toTokens ImpExpNothing = toTokens (inParens (Nothing :: Maybe Token))
    toTokens (ImpExpSome lst) = toTokens (inParens $ sepByComma $ NE.toList lst)

instance Tokenisable Export where
    toTokens (ExportFunction name) = toTokens name
    toTokens (ExportDataOrClass name lst) = toTokens name ++ toTokens lst
    toTokens (ExportModule name) = TokenKeyword KeywordModule : toTokens name

instance Tokenisable ImpDecl where
    toTokens (ImpDecl qualified name as spec) =
        TokenKeyword KeywordImport :
        [TokenName [] (NameVarId (VarId "qualified")) | qualified] ++
        toTokens name ++
        (case toTokens as of
             [] -> []
             toks -> TokenName [] (NameVarId (VarId "as")) : toks) ++
        toTokens spec

instance Tokenisable ImpSpec where
    toTokens (ImpSpec hiding lst) =
        [TokenName [] (NameVarId (VarId "hiding")) | hiding] ++
        toTokens (inParens $ sepByComma lst)

instance Tokenisable Import where
    toTokens (ImportFunction name) = toTokens name
    toTokens (ImportDataOrClass name lst) = toTokens name ++ toTokens lst

instance Tokenisable Decl where
    toTokens (DeclGenDecl decl) = toTokens decl
    toTokens (DeclFunction lhs rhs) = toTokens lhs ++ toTokens rhs

instance Tokenisable GenDecl where
    toTokens (GenDeclTypeSig vars context type') =
        toTokens (sepByComma (NE.toList vars)) ++
        [TokenOperator OperatorQDot] ++
        toTokens (InContext context) ++ toTokens type'
    toTokens (GenDeclFixity fixity prec ops) =
        toTokens fixity ++
        toTokens prec ++ toTokens (sepByComma (NE.toList ops))

instance Tokenisable Fixity where
    toTokens InfixL = [TokenKeyword KeywordInfixL]
    toTokens InfixR = [TokenKeyword KeywordInfixR]
    toTokens Infix = [TokenKeyword KeywordInfix]

instance Tokenisable Type where
    toTokens (Type args) =
        toTokens $ SepBy (TokenOperator OperatorRArrow) (NE.toList args)

instance Tokenisable BType where
    toTokens (BType args) = toTokens (NotSep $ NE.toList args)

instance Tokenisable AType where
    toTokens (ATypeConstructor name) = toTokens name
    toTokens (ATypeVar name) = toTokens name
    toTokens (ATypeTuple f s rest) =
        toTokens (inParens (sepByComma $ f : s : rest))
    toTokens (ATypeList arg) = toTokens (inBrackets arg)
    toTokens (ATypeParens arg) = toTokens (inParens arg)

instance Tokenisable GTyCon where
    toTokens (GTyConNamed name) = toTokens name
    toTokens GTyConUnit = toTokens (inParens (Tokens []))
    toTokens GTyConList = toTokens (inBrackets (Tokens []))
    toTokens GTyConFunction = toTokens (inParens (TokenOperator OperatorRArrow))
    toTokens (GTyConTuple n) =
        toTokens
            (inParens (Tokens $ replicate (n - 1) (TokenSpecial SpecialComma)))

instance Tokenisable Class where
    toTokens (ClassSimple name var) = toTokens name ++ toTokens var
    toTokens (ClassApplied name var args) =
        toTokens name ++
        toTokens
            (inParens $
             Tokens $ toTokens var ++ toTokens (NotSep (NE.toList args)))

instance Tokenisable FunLHS where
    toTokens (FunLHSSimple name pats) =
        toTokens name ++ toTokens (NotSep $ NE.toList pats)
    toTokens (FunLHSInfix l name r) = toTokens l ++ toTokens name ++ toTokens r
    toTokens (FunLHSNested f pats) =
        toTokens (inParens f) ++ toTokens (NotSep $ NE.toList pats)

instance Tokenisable RHS where
    toTokens (RHSSimple e where') =
        TokenOperator OperatorEq : toTokens e ++ toTokens (InWhere where')
    toTokens (RHSGuarded gdrhs where') =
        toTokens (NotSep $ NE.toList gdrhs) ++ toTokens (InWhere where')

instance Tokenisable GdRHS where
    toTokens (GdRHS guards e) =
        TokenOperator OperatorBar :
        toTokens (sepByComma $ NE.toList guards) ++
        [TokenOperator OperatorEq] ++ toTokens e

instance Tokenisable Guard where
    toTokens (GuardPattern pat e) =
        toTokens pat ++ [TokenOperator OperatorLArrow] ++ toTokens e
    toTokens (GuardLet decls) =
        TokenKeyword KeywordLet : toTokens (inCurly $ sepBySemicolon decls)
    toTokens (GuardExpr e) = toTokens e

instance Tokenisable Exp where
    toTokens (ExpTyped e context type') =
        toTokens e ++
        [TokenOperator OperatorQDot] ++
        toTokens (InContext context) ++ toTokens type'
    toTokens (ExpSimple e) = toTokens e

instance Tokenisable InfixExp where
    toTokens (InfixExpApplication l op r) =
        toTokens l ++ toTokens op ++ toTokens r
    toTokens (InfixExpNegated e) =
        TokenName [] (NameVarSym (VarSym "-")) : toTokens e
    toTokens (InfixExpLExp e) = toTokens e

instance Tokenisable LExp where
    toTokens (LExpAbstraction pats e) =
        TokenOperator OperatorBackslash :
        toTokens (NotSep $ NE.toList pats) ++
        [TokenOperator OperatorRArrow] ++ toTokens e
    toTokens (LExpLet decls e) =
        TokenKeyword KeywordLet :
        toTokens (inCurly $ sepBySemicolon decls) ++
        [TokenKeyword KeywordIn] ++ toTokens e
    toTokens (LExpIf cond then' else') =
        TokenKeyword KeywordIf :
        toTokens cond ++
        [TokenSpecial SpecialSemicolon, TokenKeyword KeywordThen] ++
        toTokens then' ++
        [TokenSpecial SpecialSemicolon, TokenKeyword KeywordElse] ++
        toTokens else'
    toTokens (LExpCase e alts) =
        TokenKeyword KeywordCase :
        toTokens e ++
        [TokenKeyword KeywordOf] ++
        toTokens (inCurly $ sepBySemicolon $ NE.toList alts)
    toTokens (LExpDo stmts) =
        TokenKeyword KeywordDo :
        toTokens (inCurly $ sepBySemicolon $ NE.toList stmts)
    toTokens (LExpApplication args) = toTokens $ NotSep $ NE.toList args

instance Tokenisable AExp where
    toTokens (AExpVariable name) = toTokens name
    toTokens (AExpConstructor name) = toTokens name
    toTokens (AExpLiteral lit) = toTokens lit
    toTokens (AExpParens e) = toTokens $ inParens e
    toTokens (AExpTuple f s rest) =
        toTokens $ inParens $ sepByComma $ f : s : rest
    toTokens (AExpList args) =
        toTokens $ inBrackets $ sepByComma $ NE.toList args
    toTokens (AExpSequence f s e) =
        toTokens $
        inBrackets $
        Tokens $
        toTokens f ++
        (case toTokens s of
             [] -> []
             ts -> TokenSpecial SpecialComma : ts) ++
        [TokenOperator OperatorDDot] ++ toTokens e
    toTokens (AExpListCompr e quals) =
        toTokens $
        inBrackets $
        Tokens $
        toTokens e ++
        [TokenOperator OperatorBar] ++ toTokens (sepByComma $ NE.toList quals)
    toTokens (AExpLeftSection e op) =
        toTokens $ inParens $ Tokens $ toTokens e ++ toTokens op
    toTokens (AExpRightSection op e) =
        toTokens $ inParens $ Tokens $ toTokens op ++ toTokens e
    toTokens (AExpRecordConstr name binds) =
        toTokens name ++ toTokens (inCurly $ sepByComma binds)
    toTokens (AExpRecordUpdate e binds) =
        toTokens e ++ toTokens (inCurly $ sepByComma $ NE.toList binds)

instance Tokenisable Qual where
    toTokens (QualGenerator pat e) =
        toTokens pat ++ [TokenOperator OperatorLArrow] ++ toTokens e
    toTokens (QualLet decls) =
        TokenKeyword KeywordLet : toTokens (inCurly $ sepBySemicolon decls)
    toTokens (QualGuard e) = toTokens e

instance Tokenisable Alt where
    toTokens (AltSimple pat e where') =
        toTokens pat ++
        [TokenOperator OperatorRArrow] ++
        toTokens e ++ toTokens (InWhere where')
    toTokens (AltGuarded pat gds where') =
        toTokens pat ++
        toTokens (NotSep $ NE.toList gds) ++ toTokens (InWhere where')

instance Tokenisable GdPat where
    toTokens (GdPat guards e) =
        TokenOperator OperatorBar :
        toTokens (sepByComma $ NE.toList guards) ++
        [TokenOperator OperatorRArrow] ++ toTokens e

instance Tokenisable Stmt where
    toTokens (StmtExp e) = toTokens e
    toTokens (StmtPat pat e) =
        toTokens pat ++ [TokenOperator OperatorLArrow] ++ toTokens e
    toTokens (StmtLet decls) =
        TokenKeyword KeywordLet : toTokens (inCurly $ sepBySemicolon decls)

instance Tokenisable FBind where
    toTokens (FBind var e) =
        toTokens var ++ [TokenOperator OperatorEq] ++ toTokens e

instance Tokenisable Pat where
    toTokens (PatInfix l op r) = toTokens l ++ toTokens op ++ toTokens r
    toTokens (PatSimple pat) = toTokens pat

instance Tokenisable LPat where
    toTokens (LPatSimple pat) = toTokens pat
    toTokens (LPatNegated n) =
        TokenName [] (NameVarSym (VarSym "-")) : toTokens n
    toTokens (LPatConstructor name args) =
        toTokens name ++ toTokens (NotSep $ NE.toList args)

instance Tokenisable APat where
    toTokens (APatVariable var as) =
        toTokens var ++
        (case toTokens as of
             [] -> []
             ts -> TokenOperator OperatorAt : ts)
    toTokens (APatConstructor name) = toTokens name
    toTokens (APatRecord name pats) =
        toTokens name ++ toTokens (inCurly $ sepByComma pats)
    toTokens (APatLiteral l) = toTokens l
    toTokens APatWildcard = [TokenKeyword KeywordUnderscore]
    toTokens (APatParens pat) = toTokens $ inParens pat
    toTokens (APatTuple f s rest) =
        toTokens $ inParens $ sepByComma $ f : s : rest
    toTokens (APatList pats) =
        toTokens $ inBrackets $ sepByComma $ NE.toList pats

instance Tokenisable FPat where
    toTokens (FPat var pat) =
        toTokens var ++ [TokenOperator OperatorEq] ++ toTokens pat

instance Tokenisable GCon where
    toTokens GConUnit = toTokens (inParens (Nothing :: Maybe Token))
    toTokens GConList = toTokens (inBrackets (Nothing :: Maybe Token))
    toTokens (GConTuple n) =
        toTokens
            (inParens (Tokens $ replicate (n - 1) (TokenSpecial SpecialComma)))
    toTokens (GConNamed wl) = toTokens wl

instance (Tokenisable a, Tokenisable b) => Tokenisable (FuncLabel a b) where
    toTokens (FuncLabelId name) = toTokens name
    toTokens (FuncLabelSym sym) = toTokens $ inParens sym

instance (Tokenisable a, Tokenisable b) => Tokenisable (OpLabel a b) where
    toTokens (OpLabelSym sym) = toTokens sym
    toTokens (OpLabelId name) = toTokens $ inBackticks name

instance Tokenisable GConSym where
    toTokens GConSymColon = [TokenName [] (NameConSym $ ConSym ":")]
    toTokens (GConSymOp sym) = toTokens sym

-- Helper functions
-- | Converts parts of tokens to tokens
tokenContainsToTokens :: (TokenContains a) => a -> [Token]
tokenContainsToTokens x = [toToken x]

-- | List of tokens
newtype Tokens =
    Tokens [Token]

instance Tokenisable Tokens where
    toTokens (Tokens tokens) = tokens

-- | Object between two tokens
data Between a =
    Between Token
            Token
            a

instance (Tokenisable a) => Tokenisable (Between a) where
    toTokens (Between l r x) = l : toTokens x ++ [r]

-- | Wrap object in 2 parenthesis
inParens :: a -> Between a
inParens = Between (TokenSpecial SpecialLParen) (TokenSpecial SpecialRParen)

-- | Wrap object in 2 curly brackets
inCurly :: a -> Between a
inCurly = Between (TokenSpecial SpecialLCurly) (TokenSpecial SpecialRCurly)

-- | Wrap object in 2 square brackets
inBrackets :: a -> Between a
inBrackets =
    Between (TokenSpecial SpecialLBracket) (TokenSpecial SpecialRBracket)

-- | Wrap object in 2 backticks
inBackticks :: a -> Between a
inBackticks =
    Between (TokenSpecial SpecialBacktick) (TokenSpecial SpecialBacktick)

-- | List of objects, separated by tokens
data SepBy a =
    SepBy Token
          [a]

instance (Tokenisable a) => Tokenisable (SepBy a) where
    toTokens (SepBy token xs) = intercalate [token] (map toTokens xs)

-- | Separate objects by comma
sepByComma :: [a] -> SepBy a
sepByComma = SepBy (TokenSpecial SpecialComma)

-- | Separate objects by semicolon
sepBySemicolon :: [a] -> SepBy a
sepBySemicolon = SepBy (TokenSpecial SpecialSemicolon)

-- | Not separated list of tokens
newtype NotSep a =
    NotSep [a]

instance (Tokenisable a) => Tokenisable (NotSep a) where
    toTokens (NotSep xs) = concatMap toTokens xs

-- | Object in a type context
newtype InContext a =
    InContext [a]

instance (Tokenisable a) => Tokenisable (InContext a) where
    toTokens (InContext []) = []
    toTokens (InContext [x]) = toTokens x ++ [TokenOperator OperatorBoldRArrow]
    toTokens (InContext xs) =
        toTokens (inParens (sepByComma xs)) ++
        [TokenOperator OperatorBoldRArrow]

-- | Where block
newtype InWhere a =
    InWhere [a]

instance (Tokenisable a) => Tokenisable (InWhere a) where
    toTokens (InWhere []) = []
    toTokens (InWhere xs) =
        TokenKeyword KeywordWhere : toTokens (inCurly (sepBySemicolon xs))
