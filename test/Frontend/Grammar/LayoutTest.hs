module Frontend.Grammar.LayoutTest where

import Control.Monad.Trans.State (runState)
import Test.Hspec

import Frontend.Grammar.Layout
    ( Layout(..)
    , LayoutError(..)
    , algorithmL
    , filterConsequentIndents
    , getFirstIndent
    , insertIndents
    , prepareLayout
    , restoreMissingTokens
    , withExpectedIndent
    , withIndents
    )
import Frontend.Grammar.Position
    ( WithLocation(..)
    , dummyLocation
    , sourceLocation
    )
import Frontend.Grammar.Token

testSuite :: IO ()
testSuite =
    hspec $ do
        describe "withExpectedIndent" $
            let checkAllKeywords singleCheck =
                    mapM_
                        singleCheck
                        [KeywordLet, KeywordWhere, KeywordDo, KeywordOf]
             in do it "adds expected indent after keywords" $
                       checkAllKeywords $ \keyword ->
                           let token =
                                   WithLocation
                                       (TokenKeyword keyword)
                                       (sourceLocation 1 1 1 2)
                               next =
                                   WithLocation
                                       (TokenKeyword KeywordUnderscore)
                                       (sourceLocation 1 3 1 4)
                            in withExpectedIndent token (Just next) `shouldBe`
                               [LayoutToken token, LayoutExpectedIndent 3]
                   it "adds ExpectedIndent 0 if there are no more tokens" $
                       checkAllKeywords $ \keyword ->
                           let token =
                                   WithLocation
                                       (TokenKeyword keyword)
                                       (sourceLocation 1 1 1 2)
                            in withExpectedIndent token Nothing `shouldBe`
                               [LayoutToken token, LayoutExpectedIndent 0]
                   it "doesn't add expected indent, if there is {" $
                       checkAllKeywords $ \keyword ->
                           let token =
                                   WithLocation
                                       (TokenKeyword keyword)
                                       (sourceLocation 1 1 1 2)
                               next =
                                   WithLocation
                                       (TokenSpecial SpecialLCurly)
                                       (sourceLocation 1 3 1 4)
                            in withExpectedIndent token (Just next) `shouldBe`
                               [LayoutToken token]
                   it "doesn't add indents after arbitrary tokens" $
                       mapM_
                           (\token ->
                                let wl =
                                        WithLocation
                                            token
                                            (sourceLocation 1 2 3 4)
                                 in withExpectedIndent wl Nothing `shouldBe`
                                    [LayoutToken wl])
                           [ TokenEOF EOF
                           , TokenChar (CharT 'a')
                           , TokenInteger (IntT 1)
                           , TokenString (StringT "a")
                           , TokenFloat (FloatT 1.1)
                           ]
        describe "withIndent" $ do
            it "appends indent before the first token on the line" $
                let token =
                        WithLocation
                            (TokenKeyword KeywordUnderscore)
                            (sourceLocation 1 2 1 3)
                 in runState (withIndents token Nothing) 0 `shouldBe`
                    ([LayoutIndent 2, LayoutToken token], 1)
            it "doesn't append indent before other tokens on the line" $
                let token =
                        WithLocation
                            (TokenKeyword KeywordUnderscore)
                            (sourceLocation 1 2 1 3)
                 in runState (withIndents token Nothing) 1 `shouldBe`
                    ([LayoutToken token], 1)
            it "appends indent after keywords" $
                let token =
                        WithLocation
                            (TokenKeyword KeywordWhere)
                            (sourceLocation 1 1 1 2)
                 in runState (withIndents token Nothing) 1 `shouldBe`
                    ([LayoutToken token, LayoutExpectedIndent 0], 1)
        describe "getFirstIndent" $ do
            it "Doesn't return indent after module or {" $ do
                getFirstIndent
                    [WithLocation (TokenKeyword KeywordModule) dummyLocation] `shouldBe`
                    Nothing
                getFirstIndent
                    [WithLocation (TokenKeyword KeywordModule) dummyLocation] `shouldBe`
                    Nothing
            it "Returns indent otherwise" $
                getFirstIndent
                    [ WithLocation
                          (TokenKeyword KeywordUnderscore)
                          (sourceLocation 1 2 1 3)
                    ] `shouldBe`
                Just (LayoutExpectedIndent 2)
        describe "insertIndents" $ do
            it "Doesn't change empty list" $
                runState (insertIndents []) 0 `shouldBe` ([], 0)
            it "Surrounds 1 token with indents" $ do
                let simpleToken =
                        WithLocation
                            (TokenKeyword KeywordUnderscore)
                            (sourceLocation 1 2 1 3)
                    keywordToken =
                        WithLocation
                            (TokenKeyword KeywordWhere)
                            (sourceLocation 1 2 1 3)
                runState (insertIndents [simpleToken]) 0 `shouldBe`
                    ([LayoutIndent 2, LayoutToken simpleToken], 1)
                runState (insertIndents [keywordToken]) 0 `shouldBe`
                    ( [ LayoutIndent 2
                      , LayoutToken keywordToken
                      , LayoutExpectedIndent 0
                      ]
                    , 1)
            it "Correctly inserts indents" $ do
                let firstSimpleToken =
                        WithLocation
                            (TokenKeyword KeywordUnderscore)
                            (sourceLocation 1 2 1 3)
                    secondSimpleToken =
                        WithLocation
                            (TokenKeyword KeywordUnderscore)
                            (sourceLocation 1 4 1 5)
                    firstSpecialToken =
                        WithLocation
                            (TokenKeyword KeywordDo)
                            (sourceLocation 1 2 1 3)
                    secondSpecialToken =
                        WithLocation
                            (TokenSpecial SpecialLCurly)
                            (sourceLocation 1 4 1 5)
                runState (insertIndents [firstSimpleToken, secondSimpleToken]) 0 `shouldBe`
                    ( [ LayoutIndent 2
                      , LayoutToken firstSimpleToken
                      , LayoutToken secondSimpleToken
                      ]
                    , 1)
                runState
                    (insertIndents [firstSpecialToken, secondSimpleToken])
                    0 `shouldBe`
                    ( [ LayoutIndent 2
                      , LayoutToken firstSpecialToken
                      , LayoutExpectedIndent 4
                      , LayoutToken secondSimpleToken
                      ]
                    , 1)
                runState
                    (insertIndents [firstSpecialToken, secondSpecialToken])
                    0 `shouldBe`
                    ( [ LayoutIndent 2
                      , LayoutToken firstSpecialToken
                      , LayoutToken secondSpecialToken
                      ]
                    , 1)
                runState (insertIndents [firstSimpleToken, firstSpecialToken]) 0 `shouldBe`
                    ( [ LayoutIndent 2
                      , LayoutToken firstSimpleToken
                      , LayoutToken firstSpecialToken
                      , LayoutExpectedIndent 0
                      ]
                    , 1)
        describe "filterConsequentIndents" $ do
            it "filters out indents after expected indents" $
                filterConsequentIndents [LayoutExpectedIndent 2, LayoutIndent 3] `shouldBe`
                [LayoutExpectedIndent 2]
            it "Doesn't filter out the rest" $ do
                let token =
                        WithLocation
                            (TokenKeyword KeywordUnderscore)
                            (sourceLocation 1 2 1 3)
                filterConsequentIndents
                    [LayoutExpectedIndent 2, LayoutToken token] `shouldBe`
                    [LayoutExpectedIndent 2, LayoutToken token]
                filterConsequentIndents [LayoutIndent 2, LayoutToken token] `shouldBe`
                    [LayoutIndent 2, LayoutToken token]
                filterConsequentIndents [LayoutToken token, LayoutToken token] `shouldBe`
                    [LayoutToken token, LayoutToken token]
        describe "prepareLayout" $ do
            it "Doesn't change empty list" $ prepareLayout [] `shouldBe` []
            it "Correctly prepares single elements" $ do
                let token1 =
                        WithLocation
                            (TokenKeyword KeywordUnderscore)
                            (sourceLocation 1 2 1 3)
                prepareLayout [token1] `shouldBe`
                    [LayoutExpectedIndent 2, LayoutToken token1]
                let token2 =
                        WithLocation
                            (TokenSpecial SpecialLCurly)
                            (sourceLocation 1 2 1 3)
                prepareLayout [token2] `shouldBe`
                    [LayoutIndent 2, LayoutToken token2]
                let token3 =
                        WithLocation
                            (TokenKeyword KeywordWhere)
                            (sourceLocation 1 2 1 3)
                prepareLayout [token3] `shouldBe`
                    [ LayoutExpectedIndent 2
                    , LayoutToken token3
                    , LayoutExpectedIndent 0
                    ]
            it "Correctly prepares multiple elements" $ do
                let token1 =
                        WithLocation
                            (TokenKeyword KeywordWhere)
                            (sourceLocation 1 2 1 3)
                    token2 =
                        WithLocation
                            (TokenSpecial SpecialLCurly)
                            (sourceLocation 1 4 1 5)
                    token3 =
                        WithLocation
                            (TokenKeyword KeywordUnderscore)
                            (sourceLocation 2 4 2 5)
                prepareLayout [token1, token2] `shouldBe`
                    [ LayoutExpectedIndent 2
                    , LayoutToken token1
                    , LayoutToken token2
                    ]
                prepareLayout [token1, token3] `shouldBe`
                    [ LayoutExpectedIndent 2
                    , LayoutToken token1
                    , LayoutExpectedIndent 4
                    , LayoutToken token3
                    ]
                prepareLayout [token1, token2, token3] `shouldBe`
                    [ LayoutExpectedIndent 2
                    , LayoutToken token1
                    , LayoutToken token2
                    , LayoutIndent 4
                    , LayoutToken token3
                    ]
        let lCurly = WithLocation (TokenSpecial SpecialLCurly) dummyLocation
            rCurly = WithLocation (TokenSpecial SpecialRCurly) dummyLocation
            semicolon =
                WithLocation (TokenSpecial SpecialSemicolon) dummyLocation
        describe "algorithmL" $ do
            let token n =
                    WithLocation
                        (TokenKeyword KeywordUnderscore)
                        (sourceLocation 1 n 1 (n + 1))
                layoutToken = LayoutToken . token
                in' =
                    WithLocation
                        (TokenKeyword KeywordIn)
                        (sourceLocation 1 2 1 4)
            it "Adds semicolon according to layout" $
                algorithmL [LayoutIndent 2] [2] `shouldBe`
                Right [semicolon, rCurly]
            it "Adds implicit } if it's required" $
                algorithmL [LayoutIndent 2] [3] `shouldBe` Right [rCurly]
            it "Skips indents, if they are not required" $
                algorithmL [LayoutIndent 2, layoutToken 1] [] `shouldBe`
                Right [token 1]
            it "Adds implicit { if it's required" $ do
                algorithmL [LayoutExpectedIndent 2] [] `shouldBe`
                    Right [lCurly, rCurly]
                algorithmL [LayoutExpectedIndent 3] [2] `shouldBe`
                    Right [lCurly, rCurly, rCurly]
            it "Adds implicit {} if it's required" $
                algorithmL [LayoutExpectedIndent 3] [1] `shouldBe`
                Right [lCurly, rCurly, rCurly]
            it "Preserves explicit }" $
                algorithmL [LayoutToken rCurly] [0] `shouldBe` Right [rCurly]
            it "Fails if explicit } closes implicit scope" $
                algorithmL [LayoutToken rCurly] [1] `shouldBe`
                Left (LayoutErrorRedundantClosingBracket dummyLocation)
            it "Preserves explicit {}" $
                algorithmL [LayoutToken lCurly, LayoutToken rCurly] [] `shouldBe`
                Right [lCurly, rCurly]
            it "Adds implicit } in case of (let ... in)" $
                algorithmL [LayoutToken in'] [2] `shouldBe` Right [rCurly, in']
            it "Preserves simple tokens" $
                algorithmL [layoutToken 1, layoutToken 2] [] `shouldBe`
                Right [token 1, token 2]
            it "Finishes with empty lists" $
                algorithmL [] [] `shouldBe` Right []
            it "Adds implicit } in the end of program" $ do
                algorithmL [] [1] `shouldBe` Right [rCurly]
                algorithmL [] [1, 2] `shouldBe` Right [rCurly, rCurly]
            it "Fails if misses closing curly" $
                algorithmL [] [0] `shouldBe`
                Left LayoutErrorMissingClosingBracket
        describe "restoreMissingTokens" $ do
            it "Preserves empty program" $
                restoreMissingTokens [] `shouldBe` Right []
            it "Adds missing tokens in the begin of program" $ do
                let makeToken token =
                        WithLocation token (sourceLocation 1 2 1 4)
                restoreMissingTokens [makeToken (TokenKeyword KeywordModule)] `shouldBe`
                    Right [makeToken (TokenKeyword KeywordModule)]
                restoreMissingTokens [lCurly] `shouldBe`
                    Left LayoutErrorMissingClosingBracket
                restoreMissingTokens [lCurly, rCurly] `shouldBe`
                    Right [lCurly, rCurly]
                restoreMissingTokens
                    [makeToken (TokenKeyword KeywordUnderscore)] `shouldBe`
                    Right
                        [ lCurly
                        , makeToken (TokenKeyword KeywordUnderscore)
                        , rCurly
                        ]
            it "Adds implicit brackets and semicolons" $
                restoreMissingTokens
                    [ WithLocation
                          (TokenKeyword KeywordDo)
                          (sourceLocation 1 2 1 4)
                    , WithLocation
                          (TokenKeyword KeywordUnderscore)
                          (sourceLocation 1 5 1 6)
                    , WithLocation
                          (TokenKeyword KeywordData)
                          (sourceLocation 2 5 2 7)
                    ] `shouldBe`
                Right
                    [ lCurly
                    , WithLocation
                          (TokenKeyword KeywordDo)
                          (sourceLocation 1 2 1 4)
                    , lCurly
                    , WithLocation
                          (TokenKeyword KeywordUnderscore)
                          (sourceLocation 1 5 1 6)
                    , semicolon
                    , WithLocation
                          (TokenKeyword KeywordData)
                          (sourceLocation 2 5 2 7)
                    , rCurly
                    , rCurly
                    ]
