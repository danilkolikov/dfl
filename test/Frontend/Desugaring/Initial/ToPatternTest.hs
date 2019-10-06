{- |
Module      :  Frontend.Desugaring.Initial.ToPatternTest
Description :  Tests for desugaring of object to Pattern-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Pattern-s
-}
module Frontend.Desugaring.Initial.ToPatternTest
    ( testSuite
    , getPatternExample
    ) where

import Test.Hspec

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE

import Core.PredefinedIdents
import Frontend.Desugaring.Initial.Ast
    ( Const(..)
    , InfixPattern(..)
    , Pattern(..)
    , PatternBinding(..)
    )
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToConstTest (getConstExample)
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Desugaring.Initial.ToPattern
    ( DesugarToPattern(..)
    , desugarToPatternBinding
    )
import Frontend.Desugaring.Initial.Utils
import Frontend.Syntax.Ast
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Syntax.Token (FloatT(..), IntT(..))
import Frontend.Utils.RandomSelector

type PatternExample a = RandomSelector (WithLocation a, WithLocation Pattern)

class WithPatternExamples a where
    getPatternExample :: PatternExample a

instance WithPatternExamples APat where
    getPatternExample =
        selectFromRandomRecursive
            [ do (nameEx, nameRes) <- getIdentExample
                 withSameLocation $
                     return (APatConstructor nameEx, PatternConstr nameRes [])
            , do (constEx, constRes) <- getConstExample
                 withSameLocation $
                     return (APatLiteral constEx, PatternConst constRes)
            , withSameLocation $ return (APatWildcard, PatternWildcard)
            ]
            [ do (varEx, varRes) <- getIdentExample
                 (patEx, patRes) <- randomMaybe getPatternExample
                 withSameLocation $
                     return (APatVariable varEx patEx, PatternVar varRes patRes)
            , do (nameEx, nameRes) <- getIdentExample
                 (bindingsEx, bindingsRes) <-
                     randomList 2 getPatternBindingExample
                 withSameLocation $
                     return
                         ( APatRecord nameEx bindingsEx
                         , PatternRecord nameRes bindingsRes)
            , do (patEx, patRes) <- getPatternExample
                 return (patEx $> APatParens patEx, patRes)
            , do (firstEx, firstRes) <- getPatternExample
                 (secondEx, secondRes) <- getPatternExample
                 (restEx, restRes) <- randomList 2 getPatternExample
                 let ident = makeIdent $ tUPLE 4
                 withSameLocation $
                     return
                         ( APatTuple firstEx secondEx restEx
                         , PatternConstr ident (firstRes : secondRes : restRes))
            , do (patsEx, patsRes) <- randomNonEmpty 2 getPatternExample
                 loc <- getRandomSourceLocation
                 let firstRes NE.:| [secondRes] = patsRes
                     constr = makeIdent cOLON
                     empty = makePattern lIST
                     lastRes = PatternConstr constr [secondRes, empty]
                     lastRes' = WithLocation lastRes loc
                     res = PatternConstr constr [firstRes, lastRes']
                     ex = APatList patsEx
                 return (WithLocation ex loc, WithLocation res loc)
            ]

instance WithPatternExamples LPat where
    getPatternExample =
        selectFromRandomRecursive
            [ do location <- getRandomSourceLocation
                 let wrap = (`WithLocation` location)
                 selectFromRandom
                     [ return
                           ( wrap $ LPatNegated $ wrap $ Left $ IntT 4
                           , wrap $ PatternConst $ wrap $ ConstInt (-4))
                     , return
                           ( wrap $ LPatNegated $ wrap $ Right $ FloatT 4.2
                           , wrap $ PatternConst $ wrap $ ConstFloat (-4.2))
                     ]
            ]
            [ do (patEx, patRes) <- getPatternExample
                 return (patEx $> LPatSimple patEx, patRes)
            , do (nameEx, nameRes) <- getIdentExample
                 (argsEx, argsRes) <- randomNonEmpty 3 getPatternExample
                 withSameLocation $
                     return
                         ( LPatConstructor nameEx argsEx
                         , PatternConstr nameRes (NE.toList argsRes))
            ]

type InfixPatternExample a
     = RandomSelector (WithLocation a, WithLocation InfixPattern)

class WithInfixPatternExamples a where
    getInfixPatternExample :: InfixPatternExample a

instance WithPatternExamples Pat where
    getPatternExample = do
        (expEx, expRes) <- getInfixPatternExample
        return (expEx, expRes $> PatternInfix expRes)

instance WithInfixPatternExamples Pat where
    getInfixPatternExample =
        selectFromRandomRecursive
            [ do (patEx, patRes) <- getPatternExample
                 return
                     ( patEx $> PatSimple patEx
                     , patRes $> InfixPatternSimple patRes)
            ]
            [ do (opEx, opRes) <- getIdentExample
                 (lEx, lRes) <- getInfixPatternExample
                 (rEx, rRes) <- getInfixPatternExample
                 withSameLocation $
                     return
                         ( PatInfix lEx opEx rEx
                         , InfixPatternApplication lRes opRes rRes)
            ]

checkPatternDesugaring ::
       (WithPatternExamples a, DesugarToPattern a)
    => PatternExample a
    -> Expectation
checkPatternDesugaring = checkDesugaring desugarToPattern

getPatternBindingExample ::
       RandomSelector (WithLocation FPat, WithLocation PatternBinding)
getPatternBindingExample = do
    (nameEx, nameRes) <- getIdentExample
    (patEx, patRes) <- getPatternExample
    withSameLocation $ return (FPat nameEx patEx, PatternBinding nameRes patRes)

testSuite :: IO ()
testSuite =
    hspec $ do
        describe "desugarToPattern" $ do
            it "should desugar APat" $
                checkPatternDesugaring
                    (getPatternExample :: PatternExample APat)
            it "should desugar LPat" $
                checkPatternDesugaring
                    (getPatternExample :: PatternExample LPat)
            it "should desugar Pat" $
                checkPatternDesugaring (getPatternExample :: PatternExample Pat)
        describe "desugarToPatternBinding" $
            it "should desugar FPat" $
            checkDesugaring desugarToPatternBinding getPatternBindingExample
