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


-- | Input: declarations only
-- Line 0: import "fmt"
-- Line 1: (empty)
-- Line 2: type Point struct {
-- Line 3:     X, Y int
-- Line 4: }
-- Line 5: (empty)
-- Line 6: var globalCounter = 0
-- Line 7: (empty)
-- Line 8: const MaxSize = 100
-- Line 9: (empty)
-- Line 10: func Add(a, b int) int {
-- Line 11:     return a + b
-- Line 12: }
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

-- | Output: package header + reordered declarations (import, type, var, const, func)
-- NO wrapper since only whitespace/empty lines are non-declaration
-- (the check `any (not . T.null . T.strip) nonSiftedLines` fails)
-- The empty lines just go at the end without wrapping
-- Line 0: package main
-- Line 1: (empty header)
-- Line 2: import "fmt"
-- Line 3: type Point struct {
-- Line 4: X, Y int   (indentation stripped by __i quasi-quoter)
-- Line 5: }
-- Line 6: var globalCounter = 0
-- Line 7: const MaxSize = 100
-- Line 8: func Add(a, b int) int {
-- Line 9: return a + b
-- Line 10: }
-- Line 11-14: (empty lines from input)
expectedFinalOutput :: T.Text
expectedFinalOutput = T.intercalate "\n" [
    "package main"
  , ""
  , "import \"fmt\""
  , "type Point struct {"
  , "X, Y int"
  , "}"
  , "var globalCounter = 0"
  , "const MaxSize = 100"
  , "func Add(a, b int) int {"
  , "return a + b"
  , "}"
  , ""
  , ""
  , ""
  , ""
  ]

txParams :: DeclarationSifterParams
txParams = DeclarationSifterParams "go-parser" "_notebookExec" ["package main", ""]

spec :: TopSpec
spec = describe "Example4 (only declarations)" $ do
  it "produces expected output (declarations reordered, empty lines wrapped)" $ do
    let inputDoc = listToDoc (T.splitOn "\n" testCode)
    (outputDoc, _ :: DeclarationSifter, _) <- project txParams inputDoc
    T.intercalate "\n" (docToList outputDoc) `shouldBe` expectedFinalOutput

  describe "position transformations" $ do
    -- Output line mapping (no wrapper since only empty lines are non-sifted):
    -- input 0  (import)  -> output 2
    -- input 2  (type)    -> output 3
    -- input 3  (X, Y)    -> output 4
    -- input 4  (})       -> output 5
    -- input 6  (var)     -> output 6
    -- input 8  (const)   -> output 7
    -- input 10 (func)    -> output 8
    -- input 11 (return)  -> output 9
    -- input 12 (})       -> output 10
    -- input 1,5,7,9 (empty) -> output 11,12,13,14

    it "transforms import" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      -- import at (0, 0) -> (2, 0)
      transformAndUntransform txParams (Position 0 0) (Position 2 0) sifter

    it "transforms type declaration" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      -- type Point at (2, 0) -> (3, 0)
      transformAndUntransform txParams (Position 2 0) (Position 3 0) sifter

    it "transforms var declaration" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      -- var at (6, 0) -> (6, 0)
      transformAndUntransform txParams (Position 6 0) (Position 6 0) sifter

    it "transforms const declaration" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      -- const at (8, 0) -> (7, 0)
      transformAndUntransform txParams (Position 8 0) (Position 7 0) sifter

    it "transforms func body" $ do
      let inputDoc = listToDoc (T.splitOn "\n" testCode)
      (_, sifter :: DeclarationSifter, _) <- project txParams inputDoc
      -- return inside Add at (11, 0) -> (9, 0)  (no indent due to __i quasi-quoter stripping)
      transformAndUntransform txParams (Position 11 0) (Position 9 0) sifter

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
