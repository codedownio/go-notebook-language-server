{-# LANGUAGE OverloadedStrings #-}

-- | Example2: Only imports (no statements to wrap)
module Test.Transformer.Example2 where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.Sandwich
import TestLib.Common


testCode :: T.Text
testCode = [__i|import "fmt"
import "strings"|]

expectedFinalOutput :: T.Text
expectedFinalOutput = [__i|package main

import "fmt"
import "strings"|]

txParams :: DeclarationSifterParams
txParams = DeclarationSifterParams "go-parser" "_notebookExec" ["package main", ""]

spec :: TopSpec
spec = describe "Example2 (only imports)" $ do
  it "produces expected output (no wrapper)" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter, _) <- project txParams inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    it "transforms first import" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 0 7) (Position 2 7) sifter

    it "transforms second import" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 1 0) (Position 3 0) sifter

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
