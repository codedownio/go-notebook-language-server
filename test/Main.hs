{-# LANGUAGE DataKinds #-}

import Test.Sandwich

import qualified Test.Transformer.Example1
import qualified Test.Transformer.Example2
import qualified Test.Transformer.Example3
import qualified Test.Transformer.Example4


spec :: TopSpec
spec = do
  Test.Transformer.Example1.spec
  Test.Transformer.Example2.spec
  Test.Transformer.Example3.spec
  Test.Transformer.Example4.spec

  -- TODO: Integration tests with gopls
  -- describe "Integration tests" $
  --   introduceMaybeBubblewrap $
  --   introduceNixContext nixpkgsReleaseDefault $
  --   introduceBinaryViaNixPackage @"gopls" "gopls" $
  --   introduceGnls $ do
  --     Test.Completions.spec
  --     Test.Diagnostics.spec
  --     Test.Hover.spec

main :: IO ()
main = runSandwichWithCommandLineArgs defaultOptions spec
