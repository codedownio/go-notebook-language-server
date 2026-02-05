
module Language.LSP.Test where

import Data.String.Interpolate
import qualified Data.Text as T
import Language.LSP.Notebook.GoParser
import System.Exit (ExitCode(..))
import UnliftIO.Process


testCode :: T.Text
testCode = [i|import "fmt"

x := 42

func greet(name string) {
	fmt.Printf("Hello, %s!\\n", name)
}

type Person struct {
	Name string
	Age  int
}

greet("World")
fmt.Println(x)
|]

main :: IO ()
main = do
  putStrLn "=== Testing go-parser integration ==="
  putStrLn "\n--- Input Go code ---"
  putStrLn $ T.unpack testCode

  putStrLn "\n--- Calling go-parser ---"
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (proc "go-parser" []) (T.unpack testCode)

  case exitCode of
    ExitSuccess -> do
      let jsonOutput = T.pack stdout
      putStrLn "go-parser output:"
      putStrLn stdout

      putStrLn "\n--- Parsing with GoParser ---"
      case parseGoDeclarations jsonOutput of
        Left err -> do
          putStrLn $ "Failed to parse JSON: " ++ err
        Right decls -> do
          putStrLn $ "Successfully parsed " ++ show (length decls) ++ " declarations:"
          mapM_ printDeclaration decls

          putStrLn "\n--- Analysis ---"
          let imports = filter isImportLike decls
          putStrLn $ "Import-like declarations: " ++ show (length imports)
          mapM_ (putStrLn . ("  - " ++) . show) imports

    ExitFailure code -> do
      putStrLn $ "go-parser failed with exit code: " ++ show code
      putStrLn "stderr:"
      putStrLn stderr

printDeclaration :: GoDeclaration -> IO ()
printDeclaration GoDeclaration{..} = do
  let typeStr = case declType of
        Import -> "Import"
        Func -> "Func"
        Type -> "Type"
        Var -> "Var"
        Const -> "Const"
        Other t -> "Other(" ++ T.unpack t ++ ")"
  putStrLn $ "  " ++ typeStr ++ " at lines " ++
             maybe "?" show startLine ++ "-" ++ maybe "?" show endLine
