{-# LANGUAGE OverloadedStrings #-}

-- | Example4: Only declarations (type, var, const, func - no executable statements)
module Test.Transformer.Example4 where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.Sandwich
import TestLib.Common


testCode :: T.Text
testCode = [__i|import "fmt"

type Point struct {
    X, Y int
}

var globalCounter = 0

const MaxSize = 100

func Add(a, b int) int {
    return a + b
}|]

expectedFinalOutput :: T.Text
expectedFinalOutput = [i|package main

import "fmt"
type Point struct {
X, Y int
}
var globalCounter = 0
const MaxSize = 100
func Add(a, b int) int {
return a + b
}



|]

txParams :: DeclarationSifterParams
txParams = DeclarationSifterParams "go-parser" "_notebookExec" ["package main", ""]

spec :: TopSpec
spec = describe "Example4 (only declarations)" $ do
  it "produces expected output (declarations reordered, empty lines wrapped)" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter, _) <- project txParams inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    it "transforms import" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 0 0) (Position 2 0) sifter

    it "transforms type declaration" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 2 0) (Position 3 0) sifter

    it "transforms var declaration" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 6 0) (Position 6 0) sifter

    it "transforms const declaration" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 8 0) (Position 7 0) sifter

    it "transforms func body" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 11 0) (Position 9 0) sifter

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
