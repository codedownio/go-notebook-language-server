{-# LANGUAGE TypeOperators #-}

module Language.LSP.Notebook (
  GoNotebookTransformer
  , transformerParams
  , idTransformerParams
  ) where

import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Transformer


-- Go notebook transformer - DeclarationSifter with package header and executable wrapping
type GoNotebookTransformer = DeclarationSifter

transformerParams :: Params GoNotebookTransformer
transformerParams = DeclarationSifterParams "go-parser" "_notebookExec" ["package main", ""]

idTransformerParams :: Params GoNotebookTransformer
idTransformerParams = DeclarationSifterParams "true" "" []
