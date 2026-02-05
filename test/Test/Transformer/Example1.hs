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


-- | Input notebook code (what the user writes in the notebook)
-- Line numbers (0-indexed):
-- 0: import "fmt"
-- 1: (empty)
-- 2: func hello() {
-- 3:     fmt.Println("hello")
-- 4: }
-- 5: (empty)
-- 6: fmt.Println("top level statement")
-- 7: x := 42
-- 8: fmt.Println(x)
testCode :: T.Text
testCode = [__i|import "fmt"

func hello() {
    fmt.Println("hello")
}

fmt.Println("top level statement")
x := 42
fmt.Println(x)|]

-- | Expected output after transformation
-- Line numbers (0-indexed):
-- 0: package main
-- 1: (empty header line)
-- 2: import "fmt"
-- 3: func hello() {
-- 4: fmt.Println("hello")  -- NOTE: sifted declaration keeps original indent (4 spaces from input)
-- 5: }
-- 6: func _notebookExec() {
-- 7: (empty, from input line 1)
-- 8: (empty, from input line 5)
-- 9:   fmt.Println("top level statement")  -- wrapped statements get 2-space indent
-- 10:   x := 42
-- 11:   fmt.Println(x)
-- 12: }
expectedFinalOutput :: T.Text
expectedFinalOutput = T.intercalate "\n" [
    "package main"
  , ""
  , "import \"fmt\""
  , "func hello() {"
  , "fmt.Println(\"hello\")"
  , "}"
  , "func _notebookExec() {"
  , ""
  , ""
  , "  fmt.Println(\"top level statement\")"
  , "  x := 42"
  , "  fmt.Println(x)"
  , "}"
  ]

txParams :: DeclarationSifterParams
txParams = DeclarationSifterParams "go-parser" "_notebookExec" ["package main", ""]

spec :: TopSpec
spec = describe "Example1 (mixed: import + func + statements)" $ do
  it "produces expected output" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter, _) <- project txParams inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    it "transforms statement in wrapper body (line 6 -> line 9, col +2)" $ do
      -- Input: fmt.Println at (6, 0) -> Output: at (9, 2) due to wrapper indentation
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 6 0) (Position 9 2) sifter

    it "transforms declaration (stays at original column)" $ do
      -- Input: import "fmt" at (0, 0) -> Output: at (2, 0) - no column change
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 0 0) (Position 2 0) sifter

    it "transforms function body (inside func hello)" $ do
      -- Input: fmt.Println inside func at (3, 0) -> Output: at (4, 0) - no wrapper indent
      -- NOTE: (3, 0) not (3, 4) because __i quasi-quoter stripped the 4-space indent
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      transformAndUntransform txParams (Position 3 0) (Position 4 0) sifter

    it "clamps column to 0 when untransforming from indentation area" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      -- Output line 9, col 1 is in the 2-space indent area -> clamp to col 0 in input
      Just pos <- return $ untransformPosition txParams sifter (Position 9 1)
      pos `shouldBe` Position 6 0

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
