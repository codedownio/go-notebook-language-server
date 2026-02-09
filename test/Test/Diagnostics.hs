{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Diagnostics where

import Data.String.Interpolate
import Language.LSP.Protocol.Types
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import TestLib.LSP
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers


spec :: (
  Helpers.LspContext ctx m
  , HasFile ctx "go-notebook-language-server"
  , HasFile ctx "gopls"
  ) => SpecFree ctx m ()
spec = describe "Diagnostics" $ do
  describe "Raw" $ do
    it "provides diagnostics for undefined variable" $ do
      let testCode = [__i|
            package main

            func main() {
                x := 42
                fmt.Println(y)
            }
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        _ <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        diagnostics <- LSP.waitForDiagnosticsSource "compiler"
        let ranges = Helpers.getDiagnosticRanges' diagnostics
        info [i|Got diagnostics: #{ranges}|]

        length ranges `shouldBeAtLeast` 1

    it "provides diagnostics for type errors" $ do
      let testCode = [__i|
            package main

            var x int = "hello"
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        _ <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        diagnostics <- LSP.waitForDiagnosticsSource "compiler"
        let ranges = Helpers.getDiagnosticRanges' diagnostics
        info [i|Got diagnostics: #{ranges}|]

        length ranges `shouldBeAtLeast` 1

    it "provides diagnostics for missing import in function" $ do
      let testCode = [__i|
            package main

            func hello() {
                fmt.Println("hello")
            }
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        _ <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        diagnostics <- LSP.waitForDiagnosticsSource "compiler"
        let ranges = Helpers.getDiagnosticRanges' diagnostics
        info [i|Got diagnostics: #{ranges}|]

        length ranges `shouldBeAtLeast` 1

  describe "Notebook" $ do
    it "provides diagnostics for undefined variable" $ do
      let testCode = [__i|
            x := 42
            fmt.Println(y)
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        _ <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        diagnostics <- LSP.waitForDiagnosticsSource "compiler"
        let ranges = Helpers.getDiagnosticRanges' diagnostics
        info [i|Got diagnostics: #{ranges}|]

        length ranges `shouldBeAtLeast` 1

    it "provides diagnostics for type errors" $ do
      let testCode = [__i|
            var x int = "hello"
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        _ <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        diagnostics <- LSP.waitForDiagnosticsSource "compiler"
        let ranges = Helpers.getDiagnosticRanges' diagnostics
        info [i|Got diagnostics: #{ranges}|]

        length ranges `shouldBeAtLeast` 1

    it "provides diagnostics for missing import in function" $ do
      let testCode = [__i|
            func hello() {
                fmt.Println("hello")
            }
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        _ <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        diagnostics <- LSP.waitForDiagnosticsSource "compiler"
        let ranges = Helpers.getDiagnosticRanges' diagnostics
        info [i|Got diagnostics: #{ranges}|]

        length ranges `shouldBeAtLeast` 1

  where
    shouldBeAtLeast actual expected =
      if actual >= expected
        then return ()
        else expectationFailure [i|Expected at least #{expected} diagnostics but got #{actual}|]
