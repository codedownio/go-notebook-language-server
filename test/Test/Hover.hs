{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Test.Hover where

import Control.Monad.IO.Unlift
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
spec = describe "Hover" $ do
  describe "Raw" $ do
    it "hovers over variable declaration" $ do
      let testCode = [__i|
            package main

            func main() {
                x := 42
                _ = x
            }
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        LSP.getHover doc (Position 3 4) >>= \case
          Nothing -> liftIO $ expectationFailure "Expected hover for variable 'x'"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'x': #{hoverText}|]
            liftIO $ hoverText `textShouldContain` "int"

    it "hovers over function declaration" $ do
      let testCode = [__i|
            package main

            func add(a, b int) int {
                return a + b
            }
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        LSP.getHover doc (Position 2 5) >>= \case
          Nothing -> liftIO $ expectationFailure "Expected hover for function 'add'"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'add': #{hoverText}|]
            liftIO $ hoverText `textShouldContain` "func"

    it "hovers over struct field" $ do
      let testCode = [__i|
            package main

            type Point struct {
                X int
                Y int
            }

            func main() {
                p := Point{X: 1, Y: 2}
                _ = p.X
            }
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        LSP.getHover doc (Position 9 10) >>= \case
          Nothing -> liftIO $ expectationFailure "Expected hover for field 'X'"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'X': #{hoverText}|]
            liftIO $ hoverText `textShouldContain` "int"

    it "hovers over type declaration" $ do
      let testCode = [__i|
            package main

            type MyInt int

            func main() {
                var x MyInt = 42
                _ = x
            }
            |]

      doRawSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        LSP.getHover doc (Position 2 5) >>= \case
          Nothing -> liftIO $ expectationFailure "Expected hover for type 'MyInt'"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'MyInt': #{hoverText}|]
            liftIO $ hoverText `textShouldContain` "MyInt"

  describe "Notebook" $ do
    it "hovers over variable declaration" $ do
      let testCode = [__i|
            x := 42
            _ = x
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        LSP.getHover doc (Position 0 0) >>= \case
          Nothing -> liftIO $ expectationFailure "Expected hover for variable 'x'"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'x': #{hoverText}|]
            liftIO $ hoverText `textShouldContain` "int"

    it "hovers over function declaration" $ do
      let testCode = [__i|
            func add(a, b int) int {
                return a + b
            }
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        LSP.getHover doc (Position 0 5) >>= \case
          Nothing -> liftIO $ expectationFailure "Expected hover for function 'add'"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'add': #{hoverText}|]
            liftIO $ hoverText `textShouldContain` "func"

    it "hovers over struct field" $ do
      let testCode = [__i|
            type Point struct {
                X int
                Y int
            }

            p := Point{X: 1, Y: 2}
            _ = p.X
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        LSP.getHover doc (Position 6 6) >>= \case
          Nothing -> liftIO $ expectationFailure "Expected hover for field 'X'"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'X': #{hoverText}|]
            liftIO $ hoverText `textShouldContain` "int"

    it "hovers over type declaration" $ do
      let testCode = [__i|
            type MyInt int

            var x MyInt = 42
            _ = x
            |]

      doNotebookSession testCode $ \(Helpers.LspSessionInfo {..}) -> do
        doc <- LSP.openDoc lspSessionInfoFileName LanguageKind_Go

        LSP.getHover doc (Position 0 5) >>= \case
          Nothing -> liftIO $ expectationFailure "Expected hover for type 'MyInt'"
          Just hover -> do
            let hoverText = Helpers.allHoverText hover
            info [i|Got hover text for 'MyInt': #{hoverText}|]
            liftIO $ hoverText `textShouldContain` "MyInt"
