{- |
Module      :  Frontend.Desugaring.Initial.ToModuleTest
Description :  Tests for desugaring of object to Module-s
Copyright   :  (c) Danil Kolikov, 2019
License     :  MIT

Test suite for desugaring of objects to Module-s
-}
module Frontend.Desugaring.Initial.ToModuleTest
    ( testSuite
    , getModuleExample
    ) where

import Test.Hspec

import qualified Data.List.NonEmpty as NE

import qualified Frontend.Desugaring.Initial.Ast as D
import Frontend.Desugaring.Initial.TestUtils
import Frontend.Desugaring.Initial.ToIdentTest (getIdentExample)
import Frontend.Desugaring.Initial.ToModule
    ( desugarExport
    , desugarImpDecl
    , desugarImpSpec
    , desugarImport
    , desugarToImpExp
    , desugarToModule
    )
import Frontend.Desugaring.Initial.ToTopDeclTest (getTopDeclExample)
import Frontend.Desugaring.Initial.Util
import Frontend.Syntax.Ast
import Frontend.Syntax.EntityName
import Frontend.Syntax.Position (WithLocation(..))
import Frontend.Utils.RandomSelector

getModuleExample :: RandomSelector (WithLocation Module, WithLocation D.Module)
getModuleExample =
    selectFromRandom
        [ do (nameEx, nameRes) <- getIdentExample
             (exportsEx, exportsRes) <- getExportsExample
             (bodyEx, (imps, tops)) <- getBodyExample
             withSameLocation $
                 return
                     ( ModuleExplicit nameEx exportsEx bodyEx
                     , D.Module nameRes exportsRes imps tops)
        , do (bodyEx, (imps, tops)) <- getBodyExample
             let moduleName = makeIdent dEFAULT_MODULE_NAME
             withSameLocation $
                 return
                     ( ModuleImplicit bodyEx
                     , D.Module moduleName D.ImpExpAll imps tops)
        ]

getExportExample :: RandomSelector (WithLocation Export, WithLocation D.Export)
getExportExample =
    selectFromRandom
        [ do (nameEx, nameRes) <- getIdentExample
             withSameLocation $
                 return (ExportFunction nameEx, D.ExportFunction nameRes)
        , do (nameEx, nameRes) <- getIdentExample
             (listEx, listRes) <- getImpExpListExample
             withSameLocation $
                 return
                     ( ExportDataOrClass nameEx listEx
                     , D.ExportDataOrClass nameRes listRes)
        , do (nameEx, nameRes) <- getIdentExample
             withSameLocation $
                 return (ExportModule nameEx, D.ExportModule nameRes)
        ]

getExportsExample ::
       RandomSelector ( Maybe [WithLocation Export]
                      , D.ImpExpList (WithLocation D.Export))
getExportsExample =
    selectFromRandom
        [ return (Nothing, D.ImpExpAll)
        , return (Just [], D.ImpExpNothing)
        , do (expEx, expRes) <- randomList 3 getExportExample
             return (Just expEx, D.ImpExpSome (NE.fromList expRes))
        ]

getBodyExample ::
       RandomSelector ( WithLocation Body
                      , ([WithLocation D.ImpDecl], [WithLocation D.TopDecl]))
getBodyExample =
    selectFromRandom
        [ do loc <- getRandomSourceLocation
             return (WithLocation (Body [] []) loc, ([], []))
        , do (impEx, impRes) <- randomList 2 getImpDeclExample
             (topEx, topRes) <- randomList 2 getTopDeclExample
             loc <- getRandomSourceLocation
             return
                 (WithLocation (Body impEx topEx) loc, (impRes, concat topRes))
        ]

getImportExample :: RandomSelector (WithLocation Import, WithLocation D.Import)
getImportExample =
    selectFromRandom
        [ do (nameEx, nameRes) <- getIdentExample
             withSameLocation $
                 return (ImportFunction nameEx, D.ImportFunction nameRes)
        , do (nameEx, nameRes) <- getIdentExample
             (listEx, listRes) <- getImpExpListExample
             withSameLocation $
                 return
                     ( ImportDataOrClass nameEx listEx
                     , D.ImportDataOrClass nameRes listRes)
        ]

getImpExpListExample ::
       RandomSelector ( WithLocation ImpExpList
                      , D.ImpExpList (WithLocation D.Ident))
getImpExpListExample = do
    loc <- getRandomSourceLocation
    selectFromRandom
        [ return (WithLocation ImpExpNothing loc, D.ImpExpNothing)
        , return (WithLocation ImpExpAll loc, D.ImpExpAll)
        , do (namesEx, namesRes) <- randomList 3 getIdentExample
             return
                 ( WithLocation (ImpExpSome (NE.fromList namesEx)) loc
                 , D.ImpExpSome (NE.fromList namesRes))
        ]

getImpDeclExample ::
       RandomSelector (WithLocation ImpDecl, WithLocation D.ImpDecl)
getImpDeclExample = do
    qual <- selectFromRandom [return True, return False]
    (nameEx, nameRes) <- getIdentExample
    (asEx, asRes) <- randomMaybe getIdentExample
    (impSpecEx, (hiding, list)) <- getImpSpecExample
    withSameLocation $
        return
            ( ImpDecl qual nameEx asEx impSpecEx
            , D.ImpDecl qual nameRes asRes hiding list)

getImpSpecExample ::
       RandomSelector ( Maybe (WithLocation ImpSpec)
                      , (Bool, D.ImpExpList (WithLocation D.Import)))
getImpSpecExample = do
    loc <- getRandomSourceLocation
    selectFromRandom
        [ return (Nothing, (False, D.ImpExpAll))
        , return (Just (WithLocation (ImpSpec False []) loc), (False, D.ImpExpNothing))
        , return (Just (WithLocation (ImpSpec True []) loc), (True, D.ImpExpNothing))
        , do (nameEx, nameRes) <- randomList 2 getImportExample
             return
                 ( Just (WithLocation (ImpSpec True nameEx) loc)
                 , (True, D.ImpExpSome (NE.fromList nameRes)))
        ]

testSuite :: IO ()
testSuite =
    hspec $ do
        describe "desugarToModule" $
            it "should desugar Module" $
            checkDesugaring 10 2 desugarToModule getModuleExample
        describe "desugarToImport" $
            it "should desugar Import" $
            checkDesugaring 10 2 desugarImport getImportExample
        describe "desugarToExport" $
            it "should desugar Export" $
            checkDesugaring 10 2 desugarExport getExportExample
        describe "desugarToImpExp" $
            it "should desugar ImpExpList" $
            checkDesugaring 10 2 desugarToImpExp getImpExpListExample
        describe "desugarToImpDecl" $
            it "should desugar ImpDecl" $
            checkDesugaring 10 2 desugarImpDecl getImpDeclExample
        describe "desugarImpSpec" $
            it "should desugar ImpSpec" $
            checkDesugaring 10 2 desugarImpSpec getImpSpecExample
