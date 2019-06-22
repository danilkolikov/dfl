{- |
Module      :  Frontend.Desugaring.Initial.ToTypeTest
Description :  Tests for desugaring of object to Type-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Type-s
-}
module Frontend.Desugaring.Initial.ToTypeTest
    ( testSuite
    , getTypeExample
    ) where

import Test.Hspec

import Data.Functor (($>))
import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D (Ident(..), Type(..))
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Desugaring.Initial.ToType (DesugarToType(..))
import Frontend.Desugaring.Initial.Utils
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

type TypeExample a = RandomSelector (WithLocation a, WithLocation D.Type)

class WithTypeExamples a where
    getTypeExample :: TypeExample a

instance WithTypeExamples AType where
    getTypeExample =
        selectFromRandomRecursive
            [ do (nameEx, nameRes) <- getIdentExample
                 return
                     (nameEx $> ATypeVar nameEx, nameRes $> D.TypeVar nameRes)
            , do (nameEx, nameRes) <- getIdentExample
                 return
                     ( nameEx $> ATypeConstructor nameEx
                     , nameRes $> D.TypeConstr nameRes)
            ]
            [ do (firstEx, firstRes) <- getTypeExample
                 (secondEx, secondRes) <- getTypeExample
                 (restEx, restRes) <- randomList 2 getTypeExample
                 let ident = makeTypeConstr' $ D.IdentParametrised tUPLE_NAME 4
                 withSameLocation $
                     return
                         ( ATypeTuple firstEx secondEx restEx
                         , D.TypeApplication
                               ident
                               (firstRes NE.:| secondRes : restRes))
            , do (typeEx, typeRes) <- getTypeExample
                 let ident = makeTypeConstr lIST_NAME
                 withSameLocation $
                     return
                         ( ATypeList typeEx
                         , D.TypeApplication ident (typeRes NE.:| []))
            , do (typeEx, typeRes) <- getTypeExample
                 return (typeEx $> ATypeParens typeEx, typeRes)
            ]

instance WithTypeExamples BType where
    getTypeExample =
        selectFromRandomRecursive
            [ do (typeEx, typeRes) <- getTypeExample
                 return (typeEx $> BType (typeEx NE.:| []), typeRes)
            ]
            [ do (typesEx, typesRes) <- randomNonEmpty 3 getTypeExample
                 withSameLocation $
                     return
                         ( BType typesEx
                         , D.TypeApplication
                               (NE.head typesRes)
                               (NE.fromList . NE.tail $ typesRes))
            ]

instance WithTypeExamples Type where
    getTypeExample =
        selectFromRandomRecursive
            [ do (typeEx, typeRes) <- getTypeExample
                 return (typeEx $> Type (typeEx NE.:| []), typeRes)
            ]
            [ do (typesEx, typesRes) <- randomNonEmpty 3 getTypeExample
                 location <- getRandomSourceLocation
                 let type' = Type typesEx
                     func = makeTypeConstr fUNCTION_NAME
                     (fRes NE.:| [sRes, tRes]) = typesRes
                     rightFunc = D.TypeApplication func (sRes NE.:| [tRes])
                     rightFunc' = WithLocation rightFunc location
                     resFunc = D.TypeApplication func (fRes NE.:| [rightFunc'])
                 return
                     ( WithLocation type' location
                     , WithLocation resFunc location)
            ]

checkTypeDesugaring ::
       (WithTypeExamples a, DesugarToType a) => TypeExample a -> Expectation
checkTypeDesugaring = checkDesugaring desugarToType

testSuite :: IO ()
testSuite =
    hspec $
    describe "desugarToType" $ do
        it "should desugar AType" $
            checkTypeDesugaring (getTypeExample :: TypeExample AType)
        it "should desugar BType" $
            checkTypeDesugaring (getTypeExample :: TypeExample BType)
        it "should desugar Type" $
            checkTypeDesugaring (getTypeExample :: TypeExample Type)
