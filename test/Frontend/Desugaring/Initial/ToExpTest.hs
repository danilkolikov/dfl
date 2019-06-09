{- |
Module      :  Frontend.Desugaring.Initial.ToExpTest
Description :  Tests for desugaring of object to Exp-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Exp-s
-}
module Frontend.Desugaring.Initial.ToExpTest
    ( testSuite
    , getExpExample
    , getAssignmentExample
    , getFunLHSExample
    , getGenDeclExample
    ) where

import Test.Hspec

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToConstTest (getConstExample)
import Frontend.Desugaring.Initial.ToConstraintTest (getConstraintExample)
import Frontend.Desugaring.Initial.ToExp
    ( DesugarToExp(..)
    , desugarFunLHS
    , desugarGdPat
    , desugarGuard
    , desugarOperator
    , desugarQual
    , desugarStmt
    , desugarToAlt
    , desugarToAssignment
    , desugarToBinding
    , withDecls
    )
import Frontend.Desugaring.Initial.ToIdentTest (IdentExample, getIdentExample)
import Frontend.Desugaring.Initial.ToPatternTest (getPatternExample)
import Frontend.Desugaring.Initial.ToTypeTest (getTypeExample)
import Frontend.Desugaring.Initial.Util
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

type ExpExample a = RandomSelector (WithLocation a, WithLocation D.Exp)

class WithExpExamples a where
    getExpExample :: ExpExample a

instance WithExpExamples RHS where
    getExpExample =
        selectFromRandom
            [ do (expEx, expRes) <- getExpExample
                 return (expEx $> RHSSimple expEx [], expRes)
            , do (expEx, expRes) <- getExpExample
                 (declsEx, declsRes) <- randomList 2 getAssignmentExample
                 withSameLocation $
                     return
                         ( RHSSimple expEx declsEx
                         , withDecls (concat declsRes) expRes)
            , do (gdPatsEx, gdPatsRes) <- randomNonEmpty 2 getGdPatExample
                 (declsEx, declsRes) <- randomList 2 getAssignmentExample
                 loc <- getRandomSourceLocation
                 let wrap = (`WithLocation` loc)
                     gdPatToGdRHS (GdPat guards exp') = GdRHS guards exp'
                     gdrhs = fmap (gdPatToGdRHS <$>) gdPatsEx
                     ex = RHSGuarded gdrhs declsEx
                     unitPat = makePattern uNIT_NAME
                     alt = D.AltGuarded unitPat gdPatsRes (concat declsRes)
                     unitExp = makeExp uNIT_NAME
                     res = D.ExpCase unitExp (wrap alt NE.:| [])
                 return (wrap ex, wrap res)
            ]

instance WithExpExamples Exp where
    getExpExample =
        selectFromRandomRecursive
            [ do (expEx, expRes) <- getExpExample
                 return (expEx $> ExpSimple expEx, expRes)
            ]
            [ do (expEx, expRes) <- getExpExample
                 (contextEx, contextRes) <- randomList 2 getConstraintExample
                 (typeEx, typeRes) <- getTypeExample
                 withSameLocation $
                     return
                         ( ExpTyped expEx contextEx typeEx
                         , D.ExpTyped expRes contextRes typeRes)
            ]

instance WithExpExamples InfixExp where
    getExpExample =
        selectFromRandomRecursive
            [ do (expEx, expRes) <- getExpExample
                 return (expEx $> InfixExpLExp expEx, expRes)
            ]
            [ do (expEx, expRes) <- getExpExample
                 let ident = makeExp nEGATE_NAME
                 withSameLocation $
                     return
                         ( InfixExpNegated undefined expEx
                         , D.ExpApplication ident (expRes NE.:| []))
            , do (lEx, lRes) <- getExpExample
                 (rEx, rRes) <- getExpExample
                 (opEx, opRes) <- getOperatorExample
                 withSameLocation $
                     return
                         ( InfixExpApplication lEx opEx rEx
                         , D.ExpApplication opRes (lRes NE.:| [rRes]))
            ]

instance WithExpExamples LExp where
    getExpExample =
        selectFromRandomRecursive
            [ do (expEx, expRes) <- getExpExample
                 return (expEx $> LExpApplication (expEx NE.:| []), expRes)
            ]
            [ do (expsEx, expsRes) <- randomNonEmpty 3 getExpExample
                 withSameLocation $
                     return
                         ( LExpApplication expsEx
                         , D.ExpApplication
                               (NE.head expsRes)
                               (NE.fromList (NE.tail expsRes)))
            , do (patsEx, patsRes) <- randomNonEmpty 2 getPatternExample
                 (expEx, expRes) <- getExpExample
                 withSameLocation $
                     return
                         ( LExpAbstraction patsEx expEx
                         , D.ExpAbstraction patsRes expRes)
            , do (declsEx, declsRes) <- randomList 2 getAssignmentExample
                 (expEx, expRes) <- getExpExample
                 withSameLocation $
                     return
                         ( LExpLet declsEx expEx
                         , withDecls (concat declsRes) expRes)
            , do (condEx, condRes) <- getExpExample
                 (trueEx, trueRes) <- getExpExample
                 (falseEx, falseRes) <- getExpExample
                 let trueAlt = trueRes $> D.AltSimple truePattern trueRes
                     falseAlt = falseRes $> D.AltSimple falsePattern falseRes
                 withSameLocation $
                     return
                         ( LExpIf condEx trueEx falseEx
                         , D.ExpCase condRes (trueAlt NE.:| [falseAlt]))
            , do (expEx, expRes) <- getExpExample
                 (altsEx, altsRes) <- randomNonEmpty 2 getAltExample
                 withSameLocation $
                     return (LExpCase expEx altsEx, D.ExpCase expRes altsRes)
            , do (expEx, expRes) <- getExpExample
                 (stmtsEx, stmtsRes) <- randomList 2 getStmtExample
                 let stmts = stmtsEx ++ [expEx $> StmtExp expEx]
                 withSameLocation $
                     return
                         (LExpDo (NE.fromList stmts), D.ExpDo stmtsRes expRes)
            ]

instance WithExpExamples AExp where
    getExpExample =
        selectFromRandomRecursive
            [ do (nameEx, nameRes) <- getIdentExample
                 withSameLocation $
                     return (AExpVariable nameEx, D.ExpVar nameRes)
            , do (nameEx, nameRes) <- getIdentExample
                 withSameLocation $
                     return (AExpConstructor nameEx, D.ExpConstr nameRes)
            , do (constEx, constRes) <- getConstExample
                 withSameLocation $
                     return (AExpLiteral constEx, D.ExpConst constRes)
            ]
            [ do (expEx, expRes) <- getExpExample
                 return (expEx $> AExpParens expEx, expRes)
            , do (firstEx, firstRes) <- getExpExample
                 (secondEx, secondRes) <- getExpExample
                 (restEx, restRes) <- randomList 2 getExpExample
                 let func = makeExp' $ D.IdentParametrised tUPLE_NAME 4
                 withSameLocation $
                     return
                         ( AExpTuple firstEx secondEx restEx
                         , D.ExpApplication
                               func
                               (firstRes NE.:| secondRes : restRes))
            , do (patsEx, patsRes) <- randomNonEmpty 2 getExpExample
                 loc <- getRandomSourceLocation
                 let firstRes NE.:| [secondRes] = patsRes
                     constr = makeConstr cOLON_NAME
                     empty = makeConstr lIST_NAME
                     lastRes = D.ExpApplication constr (secondRes NE.:| [empty])
                     lastRes' = WithLocation lastRes loc
                     res = D.ExpApplication constr (firstRes NE.:| [lastRes'])
                     ex = AExpList patsEx
                 return (WithLocation ex loc, WithLocation res loc)
            , do (firstEx, firstRes) <- getExpExample
                 (secondEx, secondRes) <- getExpExample
                 (thirdEx, thirdRes) <- getExpExample
                 withSameLocation $
                     selectFromRandom
                         [ let function = makeExp eNUM_FROM_NAME
                            in return
                                   ( AExpSequence firstEx Nothing Nothing
                                   , D.ExpApplication
                                         function
                                         (firstRes NE.:| []))
                         , let function = makeExp eNUM_FROM_THEN_NAME
                            in return
                                   ( AExpSequence
                                         firstEx
                                         (Just secondEx)
                                         Nothing
                                   , D.ExpApplication
                                         function
                                         (firstRes NE.:| [secondRes]))
                         , let function = makeExp eNUM_FROM_TO_NAME
                            in return
                                   ( AExpSequence firstEx Nothing (Just thirdEx)
                                   , D.ExpApplication
                                         function
                                         (firstRes NE.:| [thirdRes]))
                         , let function = makeExp eNUM_FROM_THEN_TO_NAME
                            in return
                                   ( AExpSequence
                                         firstEx
                                         (Just secondEx)
                                         (Just thirdEx)
                                   , D.ExpApplication
                                         function
                                         (firstRes NE.:| [secondRes, thirdRes]))
                         ]
            , do (expEx, expRes) <- getExpExample
                 (qualsEx, qualsRes) <- randomNonEmpty 2 getQualExample
                 withSameLocation $
                     return
                         ( AExpListCompr expEx qualsEx
                         , D.ExpListCompr expRes qualsRes)
            , do (expEx, expRes) <- getExpExample
                 (opEx, opRes) <- getOperatorExample
                 withSameLocation $
                     return
                         ( AExpLeftSection expEx opEx
                         , D.ExpLeftSection expRes opRes)
            , do (opEx, opRes) <- getOperatorExample
                 (expEx, expRes) <- getExpExample
                 withSameLocation $
                     return
                         ( AExpRightSection opEx expEx
                         , D.ExpRightSection opRes expRes)
            , do (nameEx, nameRes) <- getIdentExample
                 (bindingsEx, bindingsRes) <- randomList 2 getBindingExample
                 withSameLocation $
                     return
                         ( AExpRecordConstr nameEx bindingsEx
                         , D.ExpRecordConstr nameRes bindingsRes)
            , do (expEx, expRes) <- getExpExample
                 (bindingsEx, bindingsRes) <- randomNonEmpty 2 getBindingExample
                 withSameLocation $
                     return
                         ( AExpRecordUpdate expEx bindingsEx
                         , D.ExpRecordUpdate expRes bindingsRes)
            ]

checkExpDesugaring ::
       (WithExpExamples a, DesugarToExp a) => ExpExample a -> Expectation
checkExpDesugaring = checkDesugaring desugarToExp

getOperatorExample :: RandomSelector (WithLocation QOp, WithLocation D.Exp)
getOperatorExample =
    selectFromRandom
        [ do (opEx, opRes) <- getIdentExample :: IdentExample QVarOp
             return (Left <$> opEx, opRes $> D.ExpVar opRes)
        , do (opEx, opRes) <- getIdentExample :: IdentExample QConOp
             return (Right <$> opEx, opRes $> D.ExpConstr opRes)
        ]

getAssignmentExample ::
       RandomSelector (WithLocation Decl, [WithLocation D.Assignment])
getAssignmentExample =
    selectFromRandom
        [ do (genDeclEx, genDeclRes) <- getGenDeclExample D.AssignmentType
             return (genDeclEx $> DeclGenDecl genDeclEx, genDeclRes)
        , do (patEx, patRes) <- getPatternExample
             (rhsEx, rhsRes) <- getExpExample
             loc <- getRandomSourceLocation
             let wrap = (`WithLocation` loc)
             return
                 ( wrap $ DeclFunction (Right <$> patEx) rhsEx
                 , [wrap $ D.AssignmentPattern patRes rhsRes])
        , do (lhsEx, (nameRes, patsRes)) <- getFunLHSExample
             (rhsEx, rhsRes) <- getExpExample
             loc <- getRandomSourceLocation
             let wrap = (`WithLocation` loc)
             return
                 ( wrap $ DeclFunction (wrap $ Left lhsEx) rhsEx
                 , [wrap $ D.AssignmentName nameRes patsRes rhsRes])
        ]

getGenDeclExample ::
       (WithLocation D.Ident -> [WithLocation D.Constraint] -> WithLocation D.Type -> a)
    -> RandomSelector (WithLocation GenDecl, [WithLocation a])
getGenDeclExample wrapResult =
    selectFromRandom
        [ do loc <- getRandomSourceLocation
             let wrap = (`WithLocation` loc)
                 genDecl = GenDeclFixity undefined undefined undefined
             return (wrap genDecl, [])
        , do (varsEx, varsRes) <- randomNonEmpty 2 getIdentExample
             (contextEx, contextRes) <- randomList 2 getConstraintExample
             (typeEx, typeRes) <- getTypeExample
             loc <- getRandomSourceLocation
             let wrap = (`WithLocation` loc)
                 ex = wrap $ GenDeclTypeSig varsEx contextEx typeEx
                 processSingle var = wrap $ wrapResult var contextRes typeRes
             return (ex, map processSingle (NE.toList varsRes))
        ]

getFunLHSExample ::
       RandomSelector ( FunLHS
                      , ( WithLocation D.Ident
                        , NE.NonEmpty (WithLocation D.Pattern)))
getFunLHSExample =
    selectFromRandomRecursive
        [ do (nameEx, nameRes) <- getIdentExample
             (patsEx, patsRes) <- randomNonEmpty 2 getPatternExample
             return (FunLHSSimple nameEx patsEx, (nameRes, patsRes))
        , do (lEx, lRes) <- getPatternExample
             (nameEx, nameRes) <- getIdentExample
             (rEx, rRes) <- getPatternExample
             return (FunLHSInfix lEx nameEx rEx, (nameRes, lRes NE.:| [rRes]))
        ]
        [ do (lhsEx, (nameRes, patsRes)) <- getFunLHSExample
             (patsEx, newPatsRes) <- randomNonEmpty 2 getPatternExample
             loc <- getRandomSourceLocation
             return
                 ( FunLHSNested (WithLocation lhsEx loc) patsEx
                 , ( nameRes
                   , NE.fromList (NE.toList patsRes ++ NE.toList newPatsRes)))
        ]

getAltExample :: RandomSelector (WithLocation Alt, WithLocation D.Alt)
getAltExample =
    selectFromRandomRecursive
        [ do (patEx, patRes) <- getPatternExample
             (expEx, expRes) <- getExpExample
             withSameLocation $
                 return (AltSimple patEx expEx [], D.AltSimple patRes expRes)
        ]
        [ do (patEx, patRes) <- getPatternExample
             (expEx, expRes) <- getExpExample
             (declsEx, declsRes) <- randomList 2 getAssignmentExample
             loc <- getRandomSourceLocation
             let res = withDecls (concat declsRes) expRes
                 alt = D.AltSimple patRes (expRes $> res)
                 ex = AltSimple patEx expEx declsEx
             return (WithLocation ex loc, WithLocation alt loc)
        , do (patEx, patRes) <- getPatternExample
             (gdPatsEx, gdPatsRes) <- randomNonEmpty 2 getGdPatExample
             (declsEx, declsRes) <- randomList 2 getAssignmentExample
             withSameLocation $
                 return
                     ( AltGuarded patEx gdPatsEx declsEx
                     , D.AltGuarded patRes gdPatsRes (concat declsRes))
        ]

getStmtExample :: RandomSelector (WithLocation Stmt, WithLocation D.Stmt)
getStmtExample =
    selectFromRandomRecursive
        [ do (expEx, expRes) <- getExpExample
             withSameLocation $ return (StmtExp expEx, D.StmtExp expRes)
        ]
        [ do (patEx, patRes) <- getPatternExample
             (expEx, expRes) <- getExpExample
             withSameLocation $
                 return (StmtPat patEx expEx, D.StmtPattern patRes expRes)
        , do (declsEx, declsRes) <- randomList 2 getAssignmentExample
             withSameLocation $
                 return (StmtLet declsEx, D.StmtLet (concat declsRes))
        ]

getQualExample :: RandomSelector (WithLocation Qual, WithLocation D.Stmt)
getQualExample =
    selectFromRandomRecursive
        [ do (expEx, expRes) <- getExpExample
             withSameLocation $ return (QualGuard expEx, D.StmtExp expRes)
        ]
        [ do (patEx, patRes) <- getPatternExample
             (expEx, expRes) <- getExpExample
             withSameLocation $
                 return (QualGenerator patEx expEx, D.StmtPattern patRes expRes)
        , do (declsEx, declsRes) <- randomList 2 getAssignmentExample
             withSameLocation $
                 return (QualLet declsEx, D.StmtLet (concat declsRes))
        ]

getGuardExample :: RandomSelector (WithLocation Guard, WithLocation D.Stmt)
getGuardExample =
    selectFromRandomRecursive
        [ do (expEx, expRes) <- getExpExample
             withSameLocation $ return (GuardExpr expEx, D.StmtExp expRes)
        ]
        [ do (patEx, patRes) <- getPatternExample
             (expEx, expRes) <- getExpExample
             withSameLocation $
                 return (GuardPattern patEx expEx, D.StmtPattern patRes expRes)
        , do (declsEx, declsRes) <- randomList 2 getAssignmentExample
             withSameLocation $
                 return (GuardLet declsEx, D.StmtLet (concat declsRes))
        ]

getGdPatExample ::
       RandomSelector (WithLocation GdPat, WithLocation D.GuardedExp)
getGdPatExample = do
    (guardsEx, guardsRes) <- randomNonEmpty 2 getGuardExample
    (expEx, expRes) <- getExpExample
    withSameLocation $
        return (GdPat guardsEx expEx, D.GuardedExp guardsRes expRes)

getBindingExample :: RandomSelector (WithLocation FBind, WithLocation D.Binding)
getBindingExample = do
    (nameEx, nameRes) <- getIdentExample
    (expEx, expRes) <- getExpExample
    withSameLocation $ return (FBind nameEx expEx, D.Binding nameRes expRes)

testSuite :: IO ()
testSuite =
    hspec $ do
        describe "desugarToExp" $ do
            it "desugars AExp" $
                checkExpDesugaring (getExpExample :: ExpExample AExp)
            it "desugars LExp" $
                checkExpDesugaring (getExpExample :: ExpExample LExp)
            it "desugars InfixExp" $
                checkExpDesugaring (getExpExample :: ExpExample InfixExp)
        describe "desugarToAssignment" $
            it "desugars Decl" $
            checkDesugaring desugarToAssignment getAssignmentExample
        describe "desugarOperator" $
            it "desugars QOp" $
            checkDesugaring desugarOperator getOperatorExample
        describe "desugarToAlt" $
            it "desugars Alt" $ checkDesugaring desugarToAlt getAltExample
        describe "desugarStmt" $
            it "desugars Stmt" $ checkDesugaring desugarStmt getStmtExample
        describe "desugarQual" $
            it "desugars Qual" $ checkDesugaring desugarQual getQualExample
        describe "desugarToBinding" $
            it "desugars FBind" $
            checkDesugaring desugarToBinding getBindingExample
        describe "desugarGdPat" $
            it "desugars GdPat" $ checkDesugaring desugarGdPat getGdPatExample
        describe "desugarGuard" $
            it "desugars Guard" $ checkDesugaring desugarGuard getGuardExample
        describe "desugarFunLHS" $
            it "desugars FunLHS" $
            checkDesugaring desugarFunLHS getFunLHSExample
