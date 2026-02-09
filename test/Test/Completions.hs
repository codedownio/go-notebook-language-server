{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Completions where

import Control.Lens ((^.))
import Data.String.Interpolate
import qualified Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Types
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import TestLib.Helpers
import TestLib.LSP
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Language.LSP.Test.Helpers as Helpers


spec :: (
  Helpers.LspContext ctx m
  , HasFile ctx "go-notebook-language-server"
  , HasFile ctx "gopls"
  ) => SpecFree ctx m ()
spec = describe "Completions" $ do
  describe "Raw" $ do
    it "provides completions for fmt package" $ do
      let testCode = [__i|
            package main

            import "fmt"

            func main() {
                fmt.
            }
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        completions <- LSP.getCompletions doc (Position 5 8)
        info [i|Got completions: #{completions}|]

        let labels = map (^. Lens.label) completions
        labels `listShouldContain` "Println"
        labels `listShouldContain` "Printf"

    it "provides completions for local variables" $ do
      let testCode = [__i|
            package main

            func main() {
                myVariable := 42
                myDouble := 3.14
                my
            }
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        completions <- LSP.getCompletions doc (Position 5 6)
        info [i|Got completions: #{completions}|]

        let labels = map (^. Lens.label) completions
        labels `listShouldContain` "myVariable"
        labels `listShouldContain` "myDouble"

    it "provides completions for struct fields" $ do
      let testCode = [__i|
            package main

            type Point struct {
                X int
                Y int
            }

            func main() {
                p := Point{X: 1, Y: 2}
                p.
            }
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        completions <- LSP.getCompletions doc (Position 9 6)
        info [i|Got completions: #{completions}|]

        let labels = map (^. Lens.label) completions
        labels `listShouldContain` "X"
        labels `listShouldContain` "Y"

    it "provides completions for function parameters" $ do
      let testCode = [__i|
            package main

            func greet(name string, age int) {
                na
            }
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        completions <- LSP.getCompletions doc (Position 3 6)
        info [i|Got completions: #{completions}|]

        let labels = map (^. Lens.label) completions
        labels `listShouldContain` "name"

  describe "Notebook" $ do
    it "provides completions for fmt package" $ do
      let testCode = [__i|
            import "fmt"

            fmt.
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        completions <- LSP.getCompletions doc (Position 2 4)
        info [i|Got completions: #{completions}|]

        let labels = map (^. Lens.label) completions
        labels `listShouldContain` "Println"
        labels `listShouldContain` "Printf"

    it "provides completions for local variables" $ do
      let testCode = [__i|
            myVariable := 42
            myDouble := 3.14
            my
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        completions <- LSP.getCompletions doc (Position 2 2)
        info [i|Got completions: #{completions}|]

        let labels = map (^. Lens.label) completions
        labels `listShouldContain` "myVariable"
        labels `listShouldContain` "myDouble"

    it "provides completions for struct fields" $ do
      let testCode = [__i|
            type Point struct {
                X int
                Y int
            }

            p := Point{X: 1, Y: 2}
            p.
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        completions <- LSP.getCompletions doc (Position 6 2)
        info [i|Got completions: #{completions}|]

        let labels = map (^. Lens.label) completions
        labels `listShouldContain` "X"
        labels `listShouldContain` "Y"

    it "provides completions for function parameters" $ do
      let testCode = [__i|
            func greet(name string, age int) {
                na
            }
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        completions <- LSP.getCompletions doc (Position 1 6)
        info [i|Got completions: #{completions}|]

        let labels = map (^. Lens.label) completions
        labels `listShouldContain` "name"
