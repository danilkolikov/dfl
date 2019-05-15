{- |
Module      :  Frontend.Syntax.Utils.AstExamples
Description :  Generator of random examples for AST
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Module generates random abstract syntax trees.
-}
module Frontend.Syntax.Utils.AstExamples
    ( WithExamples(..)
    ) where

import Control.Applicative (liftA2, liftA3)
import Control.Monad (liftM4, replicateM)
import qualified Data.HashMap.Lazy as HM (elems)
import qualified Data.List.NonEmpty as NE (NonEmpty(..))

import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..), sourceLocation)
import Frontend.Syntax.Token
import Frontend.Syntax.Utils.RandomSelector
    ( RandomSelector
    , selectFromRandom
    , selectFromRandomRecursive
    , selectFromRandomRecursiveWeighted
    , selectFromRandomWeighted
    , selectRandom
    )

-- | Class of types which have examples for testing
class WithExamples a where
    getExample :: RandomSelector a -- ^ Select one random example
    -- | Select few random examples
    getExamples ::
           Int -- ^ Number of examples to select
        -> RandomSelector [a]
    getExamples n = replicateM n getExample

instance (WithExamples a) => WithExamples (Maybe a) where
    getExample =
        selectFromRandomWeighted
            [ (return Nothing, 3) -- Return Nothing twice more frequent than Just
            , (liftE1 Just, 1)
            ]

instance (WithExamples a, WithExamples b) => WithExamples (Either a b) where
    getExample = selectFromRandom [liftE1 Left, liftE1 Right]

instance (WithExamples a) => WithExamples [a] where
    getExample =
        selectFromRandomWeighted
            [ (return [], 4) -- Return [] 2 times more frequent than 1 element list
            , (liftE1 return, 2) -- Return 1 element list 2 times more frequent than 2 element list
            , (liftE2 (\x y -> [x, y]), 1)
            ]

instance (WithExamples a) => WithExamples (NE.NonEmpty a) where
    getExample =
        selectFromRandomWeighted
            [ (liftE1 (NE.:| []), 2) -- Return 1 element list 2 times more frequent than 2 element list
            , (liftE2 (\x y -> x NE.:| [y]), 1)
            ]

instance WithExamples Bool where
    getExample = selectRandom [False, True]

instance WithExamples IntT where
    getExample = selectRandom $ map IntT [0, 1, 2]

instance WithExamples FloatT where
    getExample = selectRandom $ map FloatT [0.0, 1.0, 2.0]

instance WithExamples CharT where
    getExample = selectRandom $ map CharT ['a', 'b', 'c']

instance WithExamples StringT where
    getExample = selectRandom $ map StringT ["a", "b", "c"]

instance WithExamples Keyword where
    getExample = selectRandom $ HM.elems keywords

instance WithExamples Operator where
    getExample = selectRandom $ HM.elems operators

instance WithExamples Special where
    getExample = selectRandom $ HM.elems specialSymbols

instance WithExamples EOF where
    getExample = return EOF

instance WithExamples ConId where
    getExample = selectRandom [ConId "Constructor", ConId "Type", ConId "Class"]

instance WithExamples ConSym where
    getExample = selectRandom [ConSym ":<", ConSym ":|", ConSym ":+"]

instance WithExamples VarId where
    getExample = selectRandom [VarId "foo", VarId "a", VarId "x"]

instance WithExamples VarSym where
    getExample = selectRandom [VarSym "+", VarSym "*", VarSym "-"]

instance (WithExamples a) => WithExamples (Qualified a) where
    getExample = liftA2 Qualified getExample getExample

instance (WithExamples a) => WithExamples (WithLocation a) where
    getExample = defaultLocation <$> getExample

instance WithExamples Literal where
    getExample =
        selectFromRandom
            [ liftE1 LiteralInteger
            , liftE1 LiteralFloat
            , liftE1 LiteralChar
            , liftE1 LiteralString
            ]

instance WithExamples Module where
    getExample = selectFromRandom [liftE3 ModuleExplicit, liftE1 ModuleImplicit]

instance WithExamples Body where
    getExample = liftE1 Body

instance WithExamples ImpExpList where
    getExample =
        selectFromRandom
            [return ImpExpAll, return ImpExpNothing, liftE1 ImpExpSome]

instance WithExamples Export where
    getExample =
        selectFromRandom
            [ liftE1 ExportFunction
            , liftE2 ExportDataOrClass
            , liftE1 ExportModule
            ]

instance WithExamples ImpDecl where
    getExample = liftE4 ImpDecl

instance WithExamples ImpSpec where
    getExample = liftE2 ImpSpec

instance WithExamples Import where
    getExample =
        selectFromRandom [liftE1 ImportFunction, liftE2 ImportDataOrClass]

instance WithExamples Decl where
    getExample = selectFromRandom [liftE1 DeclGenDecl, liftE2 DeclFunction]

instance WithExamples GenDecl where
    getExample = selectFromRandom [liftE3 GenDeclTypeSig, liftE3 GenDeclFixity]

instance WithExamples Fixity where
    getExample = selectRandom [InfixL, InfixR, Infix]

instance WithExamples Type where
    getExample = liftE1 Type

instance WithExamples BType where
    getExample = liftE1 BType

instance WithExamples AType where
    getExample =
        selectFromRandomRecursive
            [liftE1 ATypeConstructor, liftE1 ATypeVar]
            [liftE3 ATypeTuple, liftE1 ATypeList, liftE1 ATypeParens]

instance WithExamples GTyCon where
    getExample =
        selectFromRandom
            [ liftE1 GTyConNamed
            , return GTyConUnit
            , return GTyConList
            , selectRandom [GTyConTuple 2, GTyConTuple 3]
            , return GTyConFunction
            ]

instance WithExamples Class where
    getExample = selectFromRandom [liftE2 ClassSimple, liftE3 ClassApplied]

instance WithExamples FunLHS where
    getExample =
        selectFromRandomRecursive
            [liftE2 FunLHSSimple, liftE3 FunLHSInfix]
            [liftE2 FunLHSNested]

instance WithExamples RHS where
    getExample = selectFromRandom [liftE2 RHSSimple, liftE2 RHSGuarded]

instance WithExamples GdRHS where
    getExample = liftE2 GdRHS

instance WithExamples Guard where
    getExample =
        selectFromRandom
            [liftE2 GuardPattern, liftE1 GuardLet, liftE1 GuardExpr]

instance WithExamples Exp where
    getExample =
        selectFromRandom [liftE1 ExpSimple, inParens <$> liftE3 ExpTyped]
      where
        inParens :: Exp -> Exp
        inParens exp' =
            let aExp = AExpParens $ defaultLocation exp'
                lExp = LExpApplication $ defaultLocation aExp NE.:| []
                infixExp = InfixExpLExp $ defaultLocation lExp
             in ExpSimple $ defaultLocation infixExp

instance WithExamples InfixExp where
    getExample =
        selectFromRandomRecursive
            [getInfixExample (liftE1 InfixExpLExp) InfixExpApplication]
            [liftE1 InfixExpNegated]

instance WithExamples LExp where
    getExample =
        selectFromRandomRecursive [liftE1 LExpApplication] $
        map
            (inParens <$>)
            [ liftE2 LExpAbstraction
            , liftE2 LExpLet
            , liftE3 LExpIf
            , liftE2 LExpCase
            , liftE1 LExpDo
            , liftE3 LExpIf
            ]
      where
        inParens :: LExp -> LExp
        inParens lExp =
            let infixExp = InfixExpLExp $ defaultLocation lExp
                exp' = ExpSimple $ defaultLocation infixExp
                aExp = AExpParens $ defaultLocation exp'
             in LExpApplication $ defaultLocation aExp NE.:| []

instance WithExamples AExp where
    getExample =
        selectFromRandomRecursive
            [liftE1 AExpVariable, liftE1 AExpConstructor, liftE1 AExpLiteral]
            [ liftE1 AExpParens
            , liftE3 AExpTuple
            , liftE1 AExpList
            , liftE3 AExpSequence
            , liftE2 AExpListCompr
            , liftE2 AExpLeftSection
            , replaceMinusInRightSection <$> liftE2 AExpRightSection
            , liftE2 AExpRecordConstr
            , replaceQConInRecordUpdate <$> liftE2 AExpRecordUpdate
            ]
      where
        replaceMinusInRightSection :: AExp -> AExp
        replaceMinusInRightSection (AExpRightSection (WithLocation op loc) e)
            | (Left (OpLabelSym (Qualified [] (VarSym "-")))) <- op =
                AExpRightSection
                    (WithLocation
                         (Left (OpLabelSym (Qualified [] $ VarSym "+")))
                         loc)
                    e
        replaceMinusInRightSection aExp = aExp
        replaceQConInRecordUpdate :: AExp -> AExp
        replaceQConInRecordUpdate (AExpRecordUpdate (WithLocation e loc) binds)
            | (AExpConstructor (WithLocation (GConNamed _) loc')) <- e =
                AExpRecordUpdate
                    (WithLocation
                         (AExpConstructor (WithLocation GConList loc'))
                         loc)
                    binds
        replaceQConInRecordUpdate aExp = aExp

instance WithExamples Qual where
    getExample =
        selectFromRandom
            [liftE2 QualGenerator, liftE1 QualLet, liftE1 QualGuard]

instance WithExamples Alt where
    getExample = selectFromRandom [liftE3 AltSimple, liftE3 AltGuarded]

instance WithExamples GdPat where
    getExample = liftE2 GdPat

instance WithExamples Stmt where
    getExample =
        selectFromRandom [liftE1 StmtExp, liftE2 StmtPat, liftE1 StmtLet]

instance WithExamples FBind where
    getExample = liftE2 FBind

instance WithExamples Pat where
    getExample = getInfixExample (liftE1 PatSimple) PatInfix

instance WithExamples LPat where
    getExample =
        selectFromRandom
            [liftE1 LPatSimple, liftE1 LPatNegated, liftE2 LPatConstructor]

instance WithExamples APat where
    getExample =
        selectFromRandomRecursive
            [return APatWildcard, liftE1 APatConstructor, liftE1 APatLiteral]
            [ liftE2 APatVariable
            , liftE2 APatRecord
            , liftE1 APatParens
            , liftE3 APatTuple
            , liftE1 APatList
            ]

instance WithExamples FPat where
    getExample = liftE2 FPat

instance WithExamples GCon where
    getExample =
        selectFromRandom
            [ liftE1 GConNamed
            , return GConUnit
            , return GConList
            , selectRandom [GConTuple 2, GConTuple 3]
            ]

instance (WithExamples a, WithExamples b) => WithExamples (FuncLabel a b) where
    getExample = selectFromRandom [liftE1 FuncLabelId, liftE1 FuncLabelSym]

instance (WithExamples a, WithExamples b) => WithExamples (OpLabel a b) where
    getExample = selectFromRandom [liftE1 OpLabelSym, liftE1 OpLabelId]

instance WithExamples GConSym where
    getExample = selectFromRandom [return GConSymColon, liftE1 GConSymOp]

-- Helper functions
-- | Gets a random value and applies the provided function to it
liftE1 :: (WithExamples a) => (a -> b) -> RandomSelector b
liftE1 f = f <$> getExample

-- | Gets 2 random values and applies the provided function to them
liftE2 :: (WithExamples a, WithExamples b) => (a -> b -> c) -> RandomSelector c
liftE2 f = liftA2 f getExample getExample

-- | Gets 3 random values and applies the provided function to them
liftE3 ::
       (WithExamples a, WithExamples b, WithExamples c)
    => (a -> b -> c -> d)
    -> RandomSelector d
liftE3 f = liftA3 f getExample getExample getExample

-- | Gets 4 random values and applies the provided function to them
liftE4 ::
       (WithExamples a, WithExamples b, WithExamples c, WithExamples d)
    => (a -> b -> c -> d -> e)
    -> RandomSelector e
liftE4 f = liftM4 f getExample getExample getExample getExample

-- | Add default location to the object
defaultLocation :: a -> WithLocation a
defaultLocation = (`WithLocation` sourceLocation 1 1 1 1)

-- | Get example of an infix expression
getInfixExample ::
       (WithExamples b)
    => RandomSelector a
    -> (WithLocation a -> WithLocation b -> WithLocation a -> a)
    -> RandomSelector a
getInfixExample getSingle createInfix =
    selectFromRandomRecursiveWeighted
        [(makeInfix (1 :: Int), 4)] -- Return 1 element expression more frequntly than others
        [(makeInfix (2 :: Int), 2), (makeInfix (3 :: Int), 1)]
  where
    makeInfix n
        | n == 1 = getSingle
        | n > 1 = do
            left <- makeInfix (n - 1)
            op <- getExample
            right <- getSingle
            return $
                createInfix (defaultLocation left) op (defaultLocation right)
        | otherwise = undefined
