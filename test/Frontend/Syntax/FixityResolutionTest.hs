{- |
Module      :  Frontend.Syntax.FixityResolutionTest
Description :  Tests for fixity resolution
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for the code of fixity resolution.
-}
module Frontend.Syntax.FixityResolutionTest where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NE

import Test.Hspec

import Frontend.Syntax.Ast
import Frontend.Syntax.FixityResolution
import Frontend.Syntax.Position
    ( WithLocation(..)
    , dummyLocation
    , sourceLocation
    )
import Frontend.Syntax.Token

testSuite :: IO ()
testSuite =
    hspec $ do
        let withDummyLocation = (`WithLocation` dummyLocation)
            rawVar loc name =
                (`WithLocation` loc) . InfixExpLExp . (`WithLocation` loc) $
                LExpApplication
                    (((`WithLocation` loc) . AExpVariable . (`WithLocation` loc) $
                      (FuncLabelId . Qualified [] . VarId $ name)) NE.:|
                     [])
            var = rawVar dummyLocation
            rawOp loc name =
                (`WithLocation` loc) . Left . OpLabelSym . Qualified [] $
                VarSym name
            op = rawOp dummyLocation
            notQualifiedOp name =
                withDummyLocation . Left . OpLabelSym $ VarSym name
            patVar name =
                withDummyLocation .
                PatSimple . withDummyLocation . LPatSimple . withDummyLocation $
                APatVariable
                    (withDummyLocation . FuncLabelId $ VarId name)
                    Nothing
            patOp name =
                withDummyLocation . OpLabelSym . GConSymOp . Qualified [] $
                ConSym name
            minus' = withDummyLocation minus
        describe "toFlatInfix" $ do
            it "correctly processes expressions" $
                let x = var "x"
                    y = var "y"
                    z = var "z"
                    w = var "w"
                    plus = op "+"
                    mul = op "*"
                    div' = op "/"
                 in toFlatInfix
                        (InfixExpApplication
                             (withDummyLocation (InfixExpApplication x plus y))
                             mul
                             (withDummyLocation
                                  (InfixExpNegated
                                       minus'
                                       (withDummyLocation
                                            (InfixExpApplication z div' w))))) `shouldBe`
                    [ FlatInfixExp x
                    , FlatInfixOp (Left <$> plus)
                    , FlatInfixExp y
                    , FlatInfixOp (Left <$> mul)
                    , FlatInfixOp (Left <$> minus')
                    , FlatInfixExp z
                    , FlatInfixOp (Left <$> div')
                    , FlatInfixExp w
                    ]
            it "correctly processes patterns" $
                let x = patVar "x"
                    y = patVar "y"
                    z = patVar "z"
                    w = patVar "w"
                    col = patOp ":"
                    app = patOp ":|"
                 in toFlatInfix
                        (PatInfix
                             (withDummyLocation
                                  (PatInfix
                                       (withDummyLocation (PatInfix x col y))
                                       col
                                       z))
                             app
                             w) `shouldBe`
                    [ FlatInfixExp x
                    , FlatInfixOp (Right <$> col)
                    , FlatInfixExp y
                    , FlatInfixOp (Right <$> col)
                    , FlatInfixExp z
                    , FlatInfixOp (Right <$> app)
                    , FlatInfixExp w
                    ]
        describe "resolverStateLookup" $ do
            it "starts lookup from the top scope" $
                resolverStateLookup
                    ["-"]
                    [ HM.singleton ["-"] (InfixOperator ["-"] InfixL 7)
                    , HM.singleton ["-"] (InfixOperator ["-"] InfixL 8)
                    ] `shouldBe`
                InfixOperator ["-"] InfixL 7
            it "finds operators from different scopes" $
                resolverStateLookup
                    ["-"]
                    [ HM.singleton ["+"] (InfixOperator ["+"] InfixL 7)
                    , HM.singleton ["-"] (InfixOperator ["-"] InfixL 8)
                    ] `shouldBe`
                InfixOperator ["-"] InfixL 8
            it "returns default operator if it's not found" $
                resolverStateLookup
                    ["*"]
                    [HM.singleton ["+"] (InfixOperator ["+"] InfixL 7)] `shouldBe`
                InfixOperator ["*"] InfixL 9
            it "returns correct fixity for '-'" $
                resolverStateLookup ["-"] [HM.empty] `shouldBe`
                minusInfixOperator
        describe "resolveSingleFlatInfix" $ do
            let x = var "x"
                plus = Left <$> op "+"
                plusInfix = InfixOperator ["+"] InfixL 7
                state = [HM.singleton ["+"] plusInfix]
            it "doesn't change expressions" $
                resolveSingleFlatInfix state (FlatInfixExp x) `shouldBe`
                FlatInfixResolvedExp x
            it "correctly resolves fixity of operators" $
                resolveSingleFlatInfix
                    state
                    (FlatInfixOp plus :: FlatInfix InfixExp) `shouldBe`
                FlatInfixResolvedOp plus plusInfix
        describe "resolveFlatInfix" $ do
            it "resolves fixity of expressions" $ do
                let x = var "x"
                    y = var "y"
                    z = var "z"
                    plus = op "+"
                    mul = op "*"
                -- Same precendence, left fixity
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Left <$> plus)
                          (InfixOperator ["+"] InfixL 4)
                    , FlatInfixResolvedExp y
                    , FlatInfixResolvedOp
                          (Left <$> minus')
                          (InfixOperator ["-"] InfixL 4)
                    , FlatInfixResolvedExp z
                    ] `shouldBe`
                    Right
                        ( withDummyLocation
                              (InfixExpApplication
                                   (withDummyLocation
                                        (InfixExpApplication x plus y))
                                   minus'
                                   z)
                        , [])
                -- Different precedence, left fixity
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Left <$> plus)
                          (InfixOperator ["+"] InfixL 4)
                    , FlatInfixResolvedExp y
                    , FlatInfixResolvedOp
                          (Left <$> mul)
                          (InfixOperator ["*"] InfixL 5)
                    , FlatInfixResolvedExp z
                    ] `shouldBe`
                    Right
                        ( withDummyLocation
                              (InfixExpApplication
                                   x
                                   plus
                                   (withDummyLocation
                                        (InfixExpApplication y mul z)))
                        , [])
                -- Same precedence, right fixity
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Left <$> plus)
                          (InfixOperator ["+"] InfixR 4)
                    , FlatInfixResolvedExp y
                    , FlatInfixResolvedOp
                          (Left <$> minus')
                          (InfixOperator ["-"] InfixR 4)
                    , FlatInfixResolvedExp z
                    ] `shouldBe`
                    Right
                        ( withDummyLocation
                              (InfixExpApplication
                                   x
                                   plus
                                   (withDummyLocation
                                        (InfixExpApplication y minus' z)))
                        , [])
                -- Different precendence, right fixity
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Left <$> plus)
                          (InfixOperator ["+"] InfixR 5)
                    , FlatInfixResolvedExp y
                    , FlatInfixResolvedOp
                          (Left <$> minus')
                          (InfixOperator ["-"] InfixR 4)
                    , FlatInfixResolvedExp z
                    ] `shouldBe`
                    Right
                        ( withDummyLocation
                              (InfixExpApplication
                                   (withDummyLocation
                                        (InfixExpApplication x plus y))
                                   minus'
                                   z)
                        , [])
                -- Fixity conflicts
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Left <$> plus)
                          (InfixOperator ["+"] InfixL 4)
                    , FlatInfixResolvedExp y
                    , FlatInfixResolvedOp
                          (Left <$> minus')
                          (InfixOperator ["-"] InfixR 4)
                    , FlatInfixResolvedExp z
                    ] `shouldBe`
                    Left
                        (FixityResolutionErrorFixityConflict
                             (InfixOperator ["+"] InfixL 4)
                             (InfixOperator ["-"] InfixR 4)
                             dummyLocation)
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Left <$> plus)
                          (InfixOperator ["+"] Infix 4)
                    , FlatInfixResolvedExp y
                    , FlatInfixResolvedOp
                          (Left <$> minus')
                          (InfixOperator ["-"] InfixR 4)
                    , FlatInfixResolvedExp z
                    ] `shouldBe`
                    Left
                        (FixityResolutionErrorFixityConflict
                             (InfixOperator ["+"] Infix 4)
                             (InfixOperator ["-"] InfixR 4)
                             dummyLocation)
                -- Negation before the same precedence
                resolveFlatInfix
                    [ FlatInfixResolvedOp (Left <$> minus') minusInfixOperator
                    , FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Left <$> plus)
                          (InfixOperator ["+"] InfixL 6)
                    , FlatInfixResolvedExp y
                    ] `shouldBe`
                    Right
                        ( withDummyLocation
                              (InfixExpApplication
                                   (withDummyLocation (InfixExpNegated minus' x))
                                   plus
                                   y)
                        , [])
                -- Negation after operator with the same precendece
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Left <$> plus)
                          (InfixOperator ["+"] InfixL 6)
                    , FlatInfixResolvedOp (Left <$> minus') minusInfixOperator
                    , FlatInfixResolvedExp y
                    ] `shouldBe`
                    Left
                        (FixityResolutionErrorFixityConflict
                             (InfixOperator ["+"] InfixL 6)
                             minusInfixOperator
                             dummyLocation)
                -- Negation after operator with higher precedence
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Left <$> plus)
                          (InfixOperator ["+"] InfixL 7)
                    , FlatInfixResolvedOp (Left <$> minus') minusInfixOperator
                    , FlatInfixResolvedExp y
                    ] `shouldBe`
                    Left
                        (FixityResolutionErrorFixityConflict
                             (InfixOperator ["+"] InfixL 7)
                             minusInfixOperator
                             dummyLocation)
                -- Negation after operator with lower precedence
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Left <$> plus)
                          (InfixOperator ["+"] InfixL 5)
                    , FlatInfixResolvedOp (Left <$> minus') minusInfixOperator
                    , FlatInfixResolvedExp y
                    ] `shouldBe`
                    Right
                        ( withDummyLocation
                              (InfixExpApplication
                                   x
                                   plus
                                   (withDummyLocation (InfixExpNegated minus' y)))
                        , [])
            it "resolves fixity of patterns" $ do
                let x = patVar "x"
                    y = patVar "y"
                    z = patVar "z"
                    col = patOp ":"
                    app = patOp ":|"
                -- Different precendence, left fixity
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Right <$> col)
                          (InfixOperator [":"] InfixL 4)
                    , FlatInfixResolvedExp y
                    , FlatInfixResolvedOp
                          (Right <$> app)
                          (InfixOperator [":|"] InfixL 5)
                    , FlatInfixResolvedExp z
                    ] `shouldBe`
                    Right
                        ( withDummyLocation
                              (PatInfix
                                   x
                                   col
                                   (withDummyLocation (PatInfix y app z)))
                        , [])
                -- Missing operand
                resolveFlatInfix
                    [ FlatInfixResolvedExp x
                    , FlatInfixResolvedOp
                          (Right <$> col)
                          (InfixOperator [":"] InfixL 4)
                    ] `shouldBe`
                    Left
                        (FixityResolutionErrorMissingOperand
                             (InfixOperator [":"] InfixL 4)
                             dummyLocation)
                -- Missing operator
                resolveFlatInfix
                    [FlatInfixResolvedExp x, FlatInfixResolvedExp y] `shouldBe`
                    Left (FixityResolutionErrorMissingOperator dummyLocation)
        describe "resolveFixity" $ do
            it "resolves expressions" $ do
                let x = var "x"
                    y = var "y"
                    z = var "z"
                    plus = op "+"
                    mul = op "*"
                    state =
                        [ HM.fromList
                              [ (["+"], InfixOperator ["+"] InfixL 6)
                              , (["*"], InfixOperator ["*"] InfixL 7)
                              ]
                        ]
                runFixityResolver
                    (resolveFixity
                         (InfixExpApplication
                              (withDummyLocation (InfixExpApplication x plus y))
                              mul
                              z))
                    state `shouldBe`
                    Right
                        ( InfixExpApplication
                              x
                              plus
                              (withDummyLocation (InfixExpApplication y mul z))
                        , state)
            it "resolves patterns" $ do
                let x = patVar "x"
                    y = patVar "y"
                    z = patVar "z"
                    col = patOp ":"
                    app = patOp ":|"
                    state =
                        [ HM.fromList
                              [ ([":"], InfixOperator [":"] InfixR 6)
                              , ([":|"], InfixOperator [":|"] InfixR 6)
                              ]
                        ]
                runFixityResolver
                    (resolveFixity
                         (PatInfix (withDummyLocation (PatInfix x col y)) app z))
                    state `shouldBe`
                    Right
                        ( PatInfix x col (withDummyLocation (PatInfix y app z))
                        , state)
        describe "fixityResolver" $ do
            it "adds new operators to scope" $ do
                let exp1 =
                        GenDeclFixity
                            (withDummyLocation Infix)
                            (withDummyLocation $ IntT 6)
                            (notQualifiedOp "+" NE.:| [notQualifiedOp "-"])
                runFixityResolver (fixityResolver exp1) [HM.empty] `shouldBe`
                    Right
                        ( exp1
                        , [ HM.fromList
                                [ (["+"], InfixOperator ["+"] Infix 6)
                                , (["-"], InfixOperator ["-"] Infix 6)
                                ]
                          ])
            it "adds a new scope for each block of declarations" $ do
                let x = var "x"
                    y = var "y"
                    plus = op "+"
                    -- This expression is invalid if "+" has the same precedence
                    -- as "-" (6)
                    testExp =
                        InfixExpApplication
                            x
                            plus
                            (withDummyLocation (InfixExpNegated minus' y))
                    -- Initially plus has lower fixity, so this expression
                    -- Is acceptable
                    state = [HM.singleton ["+"] (InfixOperator ["+"] Infix 5)]
                    -- We override this precedence in the block of declarations
                    -- So resolution should fail
                    fixityDecl =
                        DeclGenDecl . withDummyLocation $
                        GenDeclFixity
                            (withDummyLocation InfixL)
                            (withDummyLocation $ IntT 6)
                            (notQualifiedOp "+" NE.:| [])
                    exp1 =
                        LExpLet
                            [withDummyLocation fixityDecl]
                            (withDummyLocation . ExpSimple $
                             withDummyLocation testExp)
                runFixityResolver (fixityResolver exp1) state `shouldBe`
                    Left
                        (FixityResolutionErrorFixityConflict
                             (InfixOperator ["+"] InfixL 6)
                             minusInfixOperator
                             dummyLocation)
            it "fails when operator is already defined" $ do
                let exp1 =
                        GenDeclFixity
                            (withDummyLocation Infix)
                            (withDummyLocation $ IntT 6)
                            (notQualifiedOp "+" NE.:| [])
                    state = [HM.singleton ["+"] (InfixOperator ["+"] Infix 5)]
                runFixityResolver (fixityResolver exp1) state `shouldBe`
                    Left
                        (FixityResolutionErrorRedefinedOperator
                             (InfixOperator ["+"] Infix 5)
                             (InfixOperator ["+"] Infix 6)
                             dummyLocation)
                let exp2 =
                        GenDeclFixity
                            (withDummyLocation Infix)
                            (withDummyLocation $ IntT 6)
                            (notQualifiedOp "+" NE.:| [notQualifiedOp "+"])
                runFixityResolver (fixityResolver exp2) [HM.empty] `shouldBe`
                    Left
                        (FixityResolutionErrorRedefinedOperator
                             (InfixOperator ["+"] Infix 6)
                             (InfixOperator ["+"] Infix 6)
                             dummyLocation)
            it "correctly changes locations" $ do
                let state =
                        [ HM.fromList
                              [ (["+"], InfixOperator ["+"] InfixL 6)
                              , (["*"], InfixOperator ["*"] InfixL 7)
                              ]
                        ]
                runFixityResolver
                    (fixityResolver
                         (InfixExpNegated
                              (WithLocation minus (sourceLocation 1 1 1 2))
                              (WithLocation
                                   (InfixExpApplication
                                        (WithLocation
                                             (InfixExpApplication
                                                  (rawVar
                                                       (sourceLocation 1 3 1 4)
                                                       "x")
                                                  (rawOp
                                                       (sourceLocation 1 5 1 6)
                                                       "+")
                                                  (rawVar
                                                       (sourceLocation 1 7 1 8)
                                                       "y"))
                                             (sourceLocation 1 3 1 8))
                                        (rawOp (sourceLocation 1 9 1 10) "*")
                                        (rawVar (sourceLocation 1 10 1 11) "z"))
                                   (sourceLocation 1 3 1 11))))
                    state `shouldBe`
                    Right
                        ( InfixExpApplication
                              (WithLocation
                                   (InfixExpNegated
                                        (WithLocation
                                             minus
                                             (sourceLocation 1 1 1 2))
                                        (rawVar (sourceLocation 1 3 1 4) "x"))
                                   (sourceLocation 1 1 1 4))
                              (rawOp (sourceLocation 1 5 1 6) "+")
                              (WithLocation
                                   (InfixExpApplication
                                        (rawVar (sourceLocation 1 7 1 8) "y")
                                        (rawOp (sourceLocation 1 9 1 10) "*")
                                        (rawVar (sourceLocation 1 10 1 11) "z"))
                                   (sourceLocation 1 7 1 11))
                        , state)
