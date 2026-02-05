{-# LANGUAGE OverloadedStrings #-}

-- | Example1: Mixed code with import, function declaration, and statements
module Test.Transformer.Example1 where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.Sandwich
import TestLib.Common


testCode :: T.Text
testCode = [__i|
  import "fmt"

  func hello() {
      fmt.Println("hello")
  }

  fmt.Println("top level statement")
  x := 42
  fmt.Println(x)
  |]

expectedFinalOutput :: T.Text
expectedFinalOutput = [__i|
  package main

  import "fmt"
  func hello() {
      fmt.Println("hello")
  }
  func _notebookExec() {


    fmt.Println("top level statement")
    x := 42
    fmt.Println(x)
  }
  |]

txParams :: DeclarationSifterParams
txParams = DeclarationSifterParams "go-parser" "_notebookExec" ["package main", ""]

spec :: TopSpec
spec = describe "Example1 (mixed: import + func + statements)" $ do
  it "produces expected output" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter, _) <- project txParams inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    it "transforms statement in wrapper body" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 6 0) (Position 9 2) sifter

    it "transforms declaration" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 0 0) (Position 2 0) sifter

    it "transforms function body" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 3 0) (Position 4 0) sifter

    it "clamps column when untransforming from indentation area" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      Just pos <- return $ untransformPosition txParams sifter (Position 9 1)
      pos `shouldBe` Position 6 0

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
