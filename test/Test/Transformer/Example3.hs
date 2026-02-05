{-# LANGUAGE OverloadedStrings #-}

-- | Example3: Only statements (all wrapped in _notebookExec)
module Test.Transformer.Example3 where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types (Position(..))
import Language.LSP.Transformer
import Test.Sandwich
import TestLib.Common


-- | Input: only executable statements, no declarations
-- Line 0: x := 42
-- Line 1: y := x * 2
-- Line 2: println(y)
testCode :: T.Text
testCode = [__i|x := 42
y := x * 2
println(y)|]

-- | Output: package header + all statements wrapped (with 2-space indent)
-- Line 0: package main
-- Line 1: (empty header)
-- Line 2: func _notebookExec() {
-- Line 3:   x := 42
-- Line 4:   y := x * 2
-- Line 5:   println(y)
-- Line 6: }
expectedFinalOutput :: T.Text
expectedFinalOutput = T.intercalate "\n" [
    "package main"
  , ""
  , "func _notebookExec() {"
  , "  x := 42"
  , "  y := x * 2"
  , "  println(y)"
  , "}"
  ]

txParams :: DeclarationSifterParams
txParams = DeclarationSifterParams "go-parser" "_notebookExec" ["package main", ""]

spec :: TopSpec
spec = describe "Example3 (only statements)" $ do
  it "produces expected output (everything wrapped)" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter, _) <- project txParams inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    it "transforms first statement" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      -- Input (0, 0) -> Output (3, 2) with wrapper indent
      transformAndUntransform txParams (Position 0 0) (Position 3 2) sifter

    it "transforms middle of statement" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      -- Input (1, 5) "x * 2" -> Output (4, 7) with wrapper indent
      transformAndUntransform txParams (Position 1 5) (Position 4 7) sifter

    it "transforms last statement" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      -- Input (2, 0) -> Output (5, 2)
      transformAndUntransform txParams (Position 2 0) (Position 5 2) sifter

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
