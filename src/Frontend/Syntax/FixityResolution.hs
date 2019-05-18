{- |
Module      :  Frontend.Syntax.FixityResolution
Description :  Fixity resolution of expressions
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Fixity resolution of expressions in DFL. Follows algorithms, defined in
<https:ororwww.haskell.orgoronlinereportorhaskell2010orhaskellch10.html Haskell 2010>.
-}
module Frontend.Syntax.FixityResolution
    ( OperatorName
    , NamedOperator(..)
    , InfixOperator(..)
    , minusInfixOperator
    , defaultInfixOperator
    , FixityResolutionError(..)
    , ResolverState
    , resolverStateLookup
    , FixityResolver
    , runFixityResolver
    , FixityResolvable(..)
    , QualifiedOperator
    , InfixOperation(..)
    , FlatInfix(..)
    , FlatInfixResolved(..)
    , resolveFixity
    , resolveFlatInfix
    , resolveSingleFlatInfix
    ) where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as E
    ( Except
    , except
    , runExcept
    , throwE
    )
import qualified Control.Monad.Trans.State as ST
    ( StateT
    , get
    , modify
    , put
    , runStateT
    )
import Data.Either (fromLeft, fromRight)
import Data.Foldable (asum)
import Data.Functor (($>))
import qualified Data.HashMap.Lazy as HM (HashMap, empty, insert, lookup)
import qualified Data.List.NonEmpty as NE (NonEmpty)
import Data.Maybe (fromMaybe)

import Frontend.Syntax.Ast
import Frontend.Syntax.Position
    ( SourceLocation(..)
    , WithLocation(..)
    , dummyLocation
    )
import Frontend.Syntax.Token
    ( ConId(..)
    , ConSym(..)
    , IntT(..)
    , TokenT
    , VarId(..)
    , VarSym(..)
    )

-- | Name of an operator
type OperatorName = [TokenT]

-- | Class for types which represent named operators
class NamedOperator a where
    getOperatorName :: a -> OperatorName -- ^ Get name of an operator

instance NamedOperator ConId where
    getOperatorName (ConId s) = [s]

instance NamedOperator ConSym where
    getOperatorName (ConSym s) = [s]

instance NamedOperator VarId where
    getOperatorName (VarId s) = [s]

instance NamedOperator VarSym where
    getOperatorName (VarSym s) = [s]

instance (NamedOperator a) => NamedOperator (Qualified a) where
    getOperatorName (Qualified path s) =
        concatMap getOperatorName path ++ getOperatorName s

instance NamedOperator GConSym where
    getOperatorName GConSymColon = [":"]
    getOperatorName (GConSymOp s) = getOperatorName s

instance (NamedOperator a, NamedOperator b) => NamedOperator (OpLabel a b) where
    getOperatorName (OpLabelSym s) = getOperatorName s
    getOperatorName (OpLabelId s) = getOperatorName s

instance (NamedOperator a, NamedOperator b) => NamedOperator (Either a b) where
    getOperatorName = either getOperatorName getOperatorName

-- | Representatin of an infix operator
data InfixOperator =
    InfixOperator OperatorName
                  Fixity
                  Int
    deriving (Show, Eq)

-- | Predefined operator for the unary minus
minusInfixOperator :: InfixOperator
minusInfixOperator = InfixOperator ["-"] InfixL 6

-- | By default, we assume that implicitly defined operators have left fixity,
--   and their precedence is 9
defaultInfixOperator :: OperatorName -> InfixOperator
defaultInfixOperator name =
    if name == ["-"]
        then minusInfixOperator -- Fixity is defined for "-"
        else InfixOperator name InfixL 9

-- | Errors which can be raised during resolution of fixity
data FixityResolutionError
    = FixityResolutionErrorRedefinedOperator InfixOperator
                                             InfixOperator
                                             SourceLocation -- ^ Some operator was redefined
    | FixityResolutionErrorMissingOperand InfixOperator
                                          SourceLocation -- ^ Binary expression misses an operand
    | FixityResolutionErrorMissingOperator SourceLocation -- ^ Binary expression misses an operator
    | FixityResolutionErrorUnexpectedOperator InfixOperator
                                              SourceLocation -- ^ There is an unexpected operator in a binary expression
    | FixityResolutionErrorFixityConflict InfixOperator
                                          InfixOperator
                                          SourceLocation -- ^ Encountered fixity conflict
    | FixityResolutionErrorCannotResolve SourceLocation -- ^ Can't resolve fixity of an expression
    deriving (Show, Eq)

-- | State of a fixity resolver is a stack of scopes of defined operators
type ResolverState = [HM.HashMap OperatorName InfixOperator]

-- | Function looks up a specified operator in a stack.
--   It starts from the top layer, and if this operator is undefined,
--   a 'defaultInfixOperator' is returned.
resolverStateLookup :: OperatorName -> ResolverState -> InfixOperator
resolverStateLookup opName state =
    fromMaybe
        (defaultInfixOperator opName)
        (asum $ map (HM.lookup opName) state)

-- | Resolver of fixity
type FixityResolver a
     = ST.StateT ResolverState (E.Except FixityResolutionError) a

-- | Function runs fixity resolver
runFixityResolver ::
       FixityResolver a
    -> ResolverState
    -> Either FixityResolutionError (a, ResolverState)
runFixityResolver r st = E.runExcept (ST.runStateT r st)

-- | Type for classes which support resolution of fixity
class FixityResolvable a where
    fixityResolver :: a -> FixityResolver a -- ^ Resolve fixity of an expression

instance (FixityResolvable a) => FixityResolvable (Maybe a) where
    fixityResolver = mapM fixityResolver

instance (FixityResolvable a, FixityResolvable b) =>
         FixityResolvable (Either a b) where
    fixityResolver =
        either ((Left <$>) . fixityResolver) ((Right <$>) . fixityResolver)

instance (FixityResolvable a) => FixityResolvable [a] where
    fixityResolver = mapM fixityResolver

instance (FixityResolvable a) => FixityResolvable (NE.NonEmpty a) where
    fixityResolver = mapM fixityResolver

instance (FixityResolvable a) => FixityResolvable (WithLocation a) where
    fixityResolver (WithLocation x loc) = apF1 (`WithLocation` loc) x

instance FixityResolvable Module where
    fixityResolver (ModuleExplicit name export body) =
        apF1 (ModuleExplicit name export) body
    fixityResolver (ModuleImplicit body) = apF1 ModuleImplicit body

instance FixityResolvable Body where
    fixityResolver (Body impDecl topDecl) = apF1 (Body impDecl) topDecl

instance FixityResolvable TopDecl where
    fixityResolver t@TopDeclType {} = return t
    fixityResolver t@TopDeclData {} = return t
    fixityResolver t@TopDeclNewType {} = return t
    fixityResolver (TopDeclClass context name param decls) =
        apF1 (TopDeclClass context name param) decls
    fixityResolver (TopDeclInstance context name inst decls) =
        apF1 (TopDeclInstance context name inst) decls
    fixityResolver (TopDeclDecl decl) = apF1 TopDeclDecl decl

instance FixityResolvable Decl where
    fixityResolver (DeclGenDecl decl) = apF1 DeclGenDecl decl
    fixityResolver (DeclFunction lhs rhs) = apF2 DeclFunction lhs rhs

instance FixityResolvable CDecl where
    fixityResolver (CDeclGenDecl decl) = apF1 CDeclGenDecl decl
    fixityResolver (CDeclFunction lhs rhs) = apF2 CDeclFunction lhs rhs

instance FixityResolvable IDecl where
    fixityResolver (IDeclFunction lhs rhs) = apF2 IDeclFunction lhs rhs

instance FixityResolvable GenDecl where
    fixityResolver t@GenDeclTypeSig {} = return t
    fixityResolver (GenDeclFixity fixity p@(WithLocation (IntT prec) _) ops) =
        GenDeclFixity fixity p <$> mapM processOp ops
      where
        processOp :: WithLocation Op -> FixityResolver (WithLocation Op)
        processOp t@(WithLocation op loc) = do
            state <- ST.get
            let (top:rest) = state
                opName = getOperatorName op
                infixOp = InfixOperator opName (getValue fixity) prec
            -- Check if this operator was already defined
            case HM.lookup opName top of
                Just infixOp' ->
                    lift
                        (E.throwE
                             (FixityResolutionErrorRedefinedOperator
                                  infixOp'
                                  infixOp
                                  loc))
                -- If it's a new operator, put it in the top layer of the stack
                Nothing -> ST.put (HM.insert opName infixOp top : rest) $> t

instance FixityResolvable FunLHS where
    fixityResolver (FunLHSSimple name pats) = apF1 (FunLHSSimple name) pats
    fixityResolver (FunLHSInfix l name r) =
        apF2 (\a b -> FunLHSInfix a name b) l r
    fixityResolver (FunLHSNested lhs pats) = apF2 FunLHSNested lhs pats

instance FixityResolvable RHS where
    fixityResolver (RHSSimple exp' where') =
        resolveDecls where' (apF1 RHSSimple exp')
    fixityResolver (RHSGuarded gdrhs where') =
        resolveDecls where' (apF1 RHSGuarded gdrhs)

instance FixityResolvable GdRHS where
    fixityResolver (GdRHS guards exp') = apF2 GdRHS guards exp'

instance FixityResolvable Guard where
    fixityResolver (GuardLet decls) = resolveDecls decls (return GuardLet)
    fixityResolver (GuardPattern pat exp') = apF2 GuardPattern pat exp'
    fixityResolver (GuardExpr exp') = apF1 GuardExpr exp'

instance FixityResolvable Exp where
    fixityResolver (ExpTyped exp' context type') =
        apF1 (\e -> ExpTyped e context type') exp'
    fixityResolver (ExpSimple exp') = apF1 ExpSimple exp'

instance FixityResolvable InfixExp where
    fixityResolver = resolveFixity

instance FixityResolvable LExp where
    fixityResolver (LExpAbstraction pats exp') = apF2 LExpAbstraction pats exp'
    fixityResolver (LExpLet decls exp') =
        resolveDecls decls (apF1 (flip LExpLet) exp')
    fixityResolver (LExpIf cond true false) = apF3 LExpIf cond true false
    fixityResolver (LExpCase exp' alts) = apF2 LExpCase exp' alts
    fixityResolver (LExpDo stmts) = apF1 LExpDo stmts
    fixityResolver (LExpApplication args) = apF1 LExpApplication args

instance FixityResolvable AExp where
    fixityResolver e@AExpVariable {} = return e
    fixityResolver e@AExpConstructor {} = return e
    fixityResolver e@AExpLiteral {} = return e
    fixityResolver (AExpParens exp') = apF1 AExpParens exp'
    fixityResolver (AExpTuple f s rest) = apF3 AExpTuple f s rest
    fixityResolver (AExpList args) = apF1 AExpList args
    fixityResolver (AExpSequence f s e) = apF3 AExpSequence f s e
    fixityResolver (AExpListCompr e quals) = apF2 AExpListCompr e quals
    fixityResolver (AExpLeftSection e op) = apF1 (`AExpLeftSection` op) e
    fixityResolver (AExpRightSection op e) = apF1 (AExpRightSection op) e
    fixityResolver (AExpRecordConstr name binds) =
        apF1 (AExpRecordConstr name) binds
    fixityResolver (AExpRecordUpdate e binds) = apF2 AExpRecordUpdate e binds

instance FixityResolvable Qual where
    fixityResolver (QualGenerator pat exp') = apF2 QualGenerator pat exp'
    fixityResolver (QualLet decls) = resolveDecls decls (return QualLet)
    fixityResolver (QualGuard exp') = apF1 QualGuard exp'

instance FixityResolvable Alt where
    fixityResolver (AltSimple pat exp' decls) =
        resolveDecls decls (apF2 AltSimple pat exp')
    fixityResolver (AltGuarded pat gdPat decls) =
        resolveDecls decls (apF2 AltGuarded pat gdPat)

instance FixityResolvable GdPat where
    fixityResolver (GdPat guards exp') = apF2 GdPat guards exp'

instance FixityResolvable Stmt where
    fixityResolver (StmtPat pat exp') = apF2 StmtPat pat exp'
    fixityResolver (StmtLet decls) = resolveDecls decls (return StmtLet)
    fixityResolver (StmtExp exp') = apF1 StmtExp exp'

instance FixityResolvable FBind where
    fixityResolver (FBind name exp') = apF1 (FBind name) exp'

instance FixityResolvable Pat where
    fixityResolver = resolveFixity

instance FixityResolvable LPat where
    fixityResolver (LPatSimple aPat) = apF1 LPatSimple aPat
    fixityResolver p@LPatNegated {} = return p
    fixityResolver (LPatConstructor name pats) =
        apF1 (LPatConstructor name) pats

instance FixityResolvable APat where
    fixityResolver (APatVariable name pat) = apF1 (APatVariable name) pat
    fixityResolver p@APatConstructor {} = return p
    fixityResolver (APatRecord name pat) = apF1 (APatRecord name) pat
    fixityResolver p@APatLiteral {} = return p
    fixityResolver p@APatWildcard = return p
    fixityResolver (APatParens p) = apF1 APatParens p
    fixityResolver (APatTuple f s rest) = apF3 APatTuple f s rest
    fixityResolver (APatList args) = apF1 APatList args

instance FixityResolvable FPat where
    fixityResolver (FPat name pat) = apF1 (FPat name) pat

instance FixityResolvable (FuncLabel a b) where
    fixityResolver = return

-- | Type for qualified operators, either usual or constructur operators
type QualifiedOperator = Either QOp QConOp

qualifiedMinus :: QualifiedOperator
qualifiedMinus = Left minus

-- | Type for flattened binary expressions
data FlatInfix a
    = FlatInfixExp (WithLocation a) -- ^ Expression
    | FlatInfixOp (WithLocation QualifiedOperator) -- ^ Operator
    deriving (Show, Eq)

-- | Type for flattened binary expressions with resolved fixity of operators
data FlatInfixResolved a
    = FlatInfixResolvedExp (WithLocation a) -- ^ Expression
    | FlatInfixResolvedOp (WithLocation QualifiedOperator) -- ^ Operator
                          InfixOperator
    deriving (Show, Eq)

-- | Class for types representing binary expressions
class InfixOperation a where
    toFlatInfix :: a -> [FlatInfix a] -- ^ "Flatten" this expression
    makeBinary ::
           WithLocation a
        -> WithLocation QualifiedOperator
        -> WithLocation a
        -> a -- ^ Make binary expression
    makeUnary :: WithLocation QualifiedOperator -> WithLocation a -> a -- ^ Make unary expression

instance InfixOperation Pat where
    toFlatInfix (PatInfix l op r) =
        toFlatInfix (getValue l) ++
        [FlatInfixOp (Right <$> op)] ++ toFlatInfix (getValue r)
    toFlatInfix p@(PatSimple (WithLocation _ loc)) =
        [FlatInfixExp (WithLocation p loc)]
    makeBinary l op = PatInfix l (fromRight undefined <$> op)
    makeUnary = undefined

instance InfixOperation InfixExp where
    toFlatInfix (InfixExpApplication l op r) =
        toFlatInfix (getValue l) ++
        [FlatInfixOp (Left <$> op)] ++ toFlatInfix (getValue r)
    toFlatInfix (InfixExpNegated op e) =
        FlatInfixOp (Left <$> op) : toFlatInfix (getValue e)
    toFlatInfix l@(InfixExpLExp (WithLocation _ loc)) =
        [FlatInfixExp (WithLocation l loc)]
    makeBinary l op = InfixExpApplication l (fromLeft undefined <$> op)
    makeUnary op = InfixExpNegated (fromLeft undefined <$> op)

-- | Resolve fixity of flat expression
resolveSingleFlatInfix :: ResolverState -> FlatInfix a -> FlatInfixResolved a
resolveSingleFlatInfix _ (FlatInfixExp e) = FlatInfixResolvedExp e
resolveSingleFlatInfix st (FlatInfixOp op) =
    FlatInfixResolvedOp op $
    resolverStateLookup (getOperatorName $ getValue op) st

-- | Resolve fixity of an expression, represented as a list of flat infix expressions
resolveFlatInfix ::
       (InfixOperation a)
    => [FlatInfixResolved a]
    -> Either FixityResolutionError (WithLocation a, [FlatInfixResolved a])
resolveFlatInfix = startResolution (InfixOperator [] Infix (-1)) dummyLocation

-- | Start resolution and handle unary expressions
startResolution ::
       (InfixOperation a)
    => InfixOperator
    -> SourceLocation
    -> [FlatInfixResolved a]
    -> Either FixityResolutionError (WithLocation a, [FlatInfixResolved a])
startResolution infixOp@(InfixOperator _ _ prec) loc list
    | [] <- list = Left $ FixityResolutionErrorMissingOperand infixOp loc
    | FlatInfixResolvedOp m@(WithLocation op' loc') infixOp'@InfixOperator {}:rest <-
         list =
        if op' == qualifiedMinus
            then if prec < 6
                     then do
                         (r, rest') <-
                             startResolution minusInfixOperator loc' rest
                         continueResolution
                             infixOp
                             (WithLocation (makeUnary m r) (betweenTokens m r))
                             rest'
                     else Left $
                          FixityResolutionErrorFixityConflict
                              infixOp
                              infixOp'
                              loc'
            else Left $ FixityResolutionErrorUnexpectedOperator infixOp' loc'
    | FlatInfixResolvedExp e:rest <- list = continueResolution infixOp e rest

-- | Continue resolution and handle binary expressions
continueResolution ::
       (InfixOperation a)
    => InfixOperator
    -> WithLocation a
    -> [FlatInfixResolved a]
    -> Either FixityResolutionError (WithLocation a, [FlatInfixResolved a])
continueResolution infixOp1@(InfixOperator _ fix1 prec1) e1 list
    | [] <- list = return (e1, [])
    | FlatInfixResolvedExp e2:_ <- list =
        Left $ FixityResolutionErrorMissingOperator (getLocation e2)
    | FlatInfixResolvedOp op2 infixOp2:rest <- list =
        case infixOp2 of
            (InfixOperator _ fix2 prec2)
                | prec1 == prec2 && (fix1 /= fix2 || fix1 == Infix) ->
                    Left $
                    FixityResolutionErrorFixityConflict
                        infixOp1
                        infixOp2
                        (getLocation op2)
                | prec1 > prec2 || (prec1 == prec2 && fix1 == InfixL) ->
                    return (e1, list)
                | otherwise -> do
                    (r, rest') <-
                        startResolution infixOp2 (getLocation op2) rest
                    continueResolution
                        infixOp1
                        (WithLocation (makeBinary e1 op2 r) (betweenTokens e1 r))
                        rest'

-- | Resolve fixity of a binary expression
resolveFixity :: (InfixOperation a) => a -> FixityResolver a
resolveFixity x = do
    state <- ST.get
    let resolvedInfix = map (resolveSingleFlatInfix state) (toFlatInfix x)
        resolvedExpr = resolveFlatInfix resolvedInfix
    resolved <- lift $ E.except resolvedExpr
    let (res, rest) = resolved
    case rest of
        [] -> return $ getValue res
        f:_ ->
            lift $
            E.throwE $ FixityResolutionErrorCannotResolve (getSourceLocation f)

-- Helper functions
-- | Adds a new scope, resolves declarations, continues resolution on a new scope
--   and then drops it.
resolveDecls ::
       [WithLocation Decl]
    -> FixityResolver ([WithLocation Decl] -> a)
    -> FixityResolver a
resolveDecls decls continue = do
    ST.modify (HM.empty :) -- Add new map of operators
    declsResolved <- fixityResolver decls
    continueResolved <- continue
    ST.modify tail -- Remove map of operators
    return (continueResolved declsResolved)

-- | Apply function to the result of fixity resolution of arguments
apF1 :: (FixityResolvable a) => (a -> b) -> a -> FixityResolver b
apF1 f x = f <$> fixityResolver x

-- | Apply function to the result of fixity resolution of arguments
apF2 ::
       (FixityResolvable a, FixityResolvable b)
    => (a -> b -> c)
    -> a
    -> b
    -> FixityResolver c
apF2 f x y = liftA2 f (fixityResolver x) (fixityResolver y)

-- | Apply function to the result of fixity resolution of arguments
apF3 ::
       (FixityResolvable a, FixityResolvable b, FixityResolvable c)
    => (a -> b -> c -> d)
    -> a
    -> b
    -> c
    -> FixityResolver d
apF3 f x y z = liftA3 f (fixityResolver x) (fixityResolver y) (fixityResolver z)

-- | Get location between two objects
betweenTokens :: WithLocation a -> WithLocation b -> SourceLocation
betweenTokens (WithLocation _ (SourceLocation st _)) (WithLocation _ (SourceLocation _ en)) =
    SourceLocation st en

-- | Get source location from a flat infix expression
getSourceLocation :: FlatInfixResolved a -> SourceLocation
getSourceLocation (FlatInfixResolvedExp e) = getLocation e
getSourceLocation (FlatInfixResolvedOp op _) = getLocation op
