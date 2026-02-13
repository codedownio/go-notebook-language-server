{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TestLib.LSP (
  doNotebookSession
  , doRawSession

  , introduceMaybeBubblewrap
  , introduceGnls

  , transformRoundTrip
  , transformRoundTripCode
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.List as L
import Data.Function (fix)
import Data.String.Interpolate
import Data.Text
import GHC.Stack
import Language.LSP.Notebook.DeclarationSifter
import Language.LSP.Protocol.Types
import Language.LSP.Transformer hiding ((:>))
import System.FilePath
import System.IO
import System.IO.Temp
import Test.Sandwich
import Test.Sandwich.Contexts.Files
import Test.Sandwich.Contexts.Nix
import UnliftIO (MonadUnliftIO)
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process
import qualified "lsp-test" Language.LSP.Test as LSP hiding (message)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Language.LSP.Test.Helpers as Helpers
import qualified System.Directory as Dir


doNotebookSession :: (
  Helpers.LspContext ctx m
  , HasFile ctx "go-notebook-language-server"
  , HasFile ctx "gopls"
  ) => T.Text -> (Helpers.LspSessionInfo -> LSP.Session (ExampleT ctx m) a) -> ExampleT ctx m a
doNotebookSession = doSession "main.ipynb"

doRawSession :: (
  Helpers.LspContext ctx m
  , HasFile ctx "go-notebook-language-server"
  , HasFile ctx "gopls"
  ) => T.Text -> (Helpers.LspSessionInfo -> LSP.Session (ExampleT ctx m) a) -> ExampleT ctx m a
doRawSession = doSession "main.go"

doSession :: (
  Helpers.LspContext ctx m
  , HasFile ctx "go-notebook-language-server"
  , HasFile ctx "gopls"
  ) => FilePath -> T.Text -> (Helpers.LspSessionInfo -> LSP.Session (ExampleT ctx m) a) -> ExampleT ctx m a
doSession fileName codeToUse cb = do
  goNotebookLSPath <- askFile @"go-notebook-language-server"
  goplsPath <- askFile @"gopls"

  Just dir <- getCurrentFolder
  withTempDirectory dir "go-lsp-test" $ \tmpDir -> do
    let testFile = tmpDir </> fileName

    -- Write the notebook code
    liftIO $ TIO.writeFile testFile codeToUse

    -- Create go.mod for gopls
    -- let goModPath = tmpDir </> "go.mod"
    -- let goModContent = T.pack $ Prelude.unlines [
    --       "module testmodule",
    --       "",
    --       "go 1.21"
    --       ]
    -- liftIO $ TIO.writeFile goModPath goModContent

    let lspConfig = Helpers.LanguageServerConfig {
          lspConfigName = "go-notebook-language-server"
          , lspConfigVersion = Nothing
          , lspConfigDescription = Just "Go notebook language server"
          , lspConfigDisplayName = Just "Go Notebook Language Server"
          , lspConfigIcon = Nothing
          , lspConfigExtensions = [".go"]
          , lspConfigAttrs = S.fromList ["go"]
          , lspConfigType = Helpers.LanguageServerTypeStream
          , lspConfigPrimary = Just True
          , lspConfigArgs = [
              T.pack goNotebookLSPath
              , "--wrapped-server", T.pack goplsPath
              , "--log-level", "debug"
              ]
          , lspConfigLanguageId = Just LanguageKind_Go
          , lspConfigInitializationOptions = Nothing
          , lspConfigNotebookSuffix = ".go"
          , lspConfigKernelName = Nothing
          , lspConfigEnv = Nothing
          , lspConfigFile = Nothing
          , lspConfigIsBuiltIn = Just False
          }

    pathToUse <- getBasicPath
    closure <- getGoLspClosure goNotebookLSPath goplsPath pathToUse

    let lspSessionOptions = (Helpers.defaultLspSessionOptions lspConfig) {
          Helpers.lspSessionOptionsInitialFileName = fileName
          , Helpers.lspSessionOptionsInitialLanguageKind = LanguageKind_Go
          , Helpers.lspSessionOptionsInitialCode = codeToUse
          , Helpers.lspSessionOptionsExtraFiles = [] -- [("go.mod", TE.encodeUtf8 goModContent)]
          , Helpers.lspSessionOptionsPathEnvVar = pathToUse
          , Helpers.lspSessionOptionsReadOnlyBinds = closure
          , Helpers.lspSessionOptionsModifySessionConfig = \x -> x { LSP.messageTimeout = 10 }
          }

    Helpers.withLspSession lspSessionOptions cb

getBasicPath :: (MonadUnliftIO m, MonadLogger m) => m FilePath
getBasicPath = do
  bracket (liftIO $ openFile "/dev/null" WriteMode) (liftIO . hClose) $ \devNullHandle -> do
    result <- readCreateProcess ((proc "nix" ["run", ".#print-basic-path"]) { std_err = UseHandle devNullHandle }) ""
    let ret = T.unpack $ T.strip $ T.pack result
    info [i|getBasicPath: #{ret}|]
    return ret

-- | Get the full Nix closure of go-notebook-language-server + gopls + PATH entries
getGoLspClosure :: (MonadUnliftIO m, MonadLogger m) => FilePath -> FilePath -> FilePath -> m [FilePath]
getGoLspClosure goNotebookLSPath goplsPath pathToUse = do
  let paths = [goNotebookLSPath, goplsPath] ++ splitSearchPath pathToUse
  closure <- (fmap T.unpack . Prelude.filter (/= "") . T.splitOn "\n" . T.pack) <$> readCreateProcess (
    proc "nix" (["path-info", "-r"] ++ paths)
    ) ""
  info [i|Got Nix closure with #{L.length closure} entries|]
  return closure

-- | Introduce maybeBubblewrap context, trying to find bwrap executable
-- Falls back to Nothing on macOS or if bwrap is not found
introduceMaybeBubblewrap :: (
  HasBaseContext context, MonadIO m, MonadMask m, MonadUnliftIO m
  ) => SpecFree (LabelValue "maybeBubblewrap" (Maybe FilePath) :> context) m () -> SpecFree context m ()
introduceMaybeBubblewrap = introduceWith [i|maybeBubblewrap|] Helpers.maybeBubblewrap $ \action -> do
#ifdef darwin_HOST_OS
  void $ action Nothing
#else
  liftIO (Dir.findExecutable "bwrap") >>= \case
    Nothing -> do
      info "bubblewrap (bwrap) not found, proceeding without sandbox"
      void $ action Nothing
    Just path -> do
      info [i|Found bubblewrap at: #{path}|]
      void $ action (Just path)
#endif

introduceGnls :: forall context m. (
  HasBaseContext context, HasNixContext context, MonadUnliftIO m
  )
  => SpecFree (LabelValue "file-go-notebook-language-server" (EnvironmentFile "go-notebook-language-server") :> context) m ()
  -> SpecFree context m ()
introduceGnls = introduce [i|go-notebook-language-server (binary via Nix derivation)|] (Label :: Label "file-go-notebook-language-server" (EnvironmentFile "go-notebook-language-server")) alloc (const $ return ())
  where
    alloc = do
      projectRoot <- getProjectRoot
      dir <- buildNixCallPackageDerivation (goNotebookLanguageServerDerivation projectRoot)
      liftIO (findExecutablesInDirectories [dir </> "bin"] "go-notebook-language-server") >>= \case
        (x:_) -> return (EnvironmentFile x :: EnvironmentFile "go-notebook-language-server")
        _ -> expectationFailure [i|Couldn't find binary in #{dir </> "bin"}|]

goNotebookLanguageServerDerivation :: FilePath -> T.Text
goNotebookLanguageServerDerivation projectRoot = [i|
{ ... }:

let
  flake = builtins.getFlake "#{projectRoot}";
in flake.packages.x86_64-linux.default
|]


getProjectRoot :: (HasCallStack, MonadIO m) => m FilePath
getProjectRoot = do
  startDir <- getCurrentDirectory
  flip fix startDir $ \loop dir -> do
    doesDirectoryExist (dir </> ".git") >>= \case
      True -> return dir
      False -> let dir' = takeDirectory dir in
                 if | dir == dir' -> throwIO $ userError [i|Couldn't find project root starting from #{startDir}|]
                    | otherwise -> loop dir'


transformRoundTripCode :: (MonadIO m, MonadFail m) => Text -> Position -> Position -> m ()
transformRoundTripCode testCode from to = do
  let inputDoc = listToDoc (T.splitOn "\n" testCode)
  let params = DeclarationSifterParams "go-parser" "_notebookExec" ["package main", ""]
  (_, sifter :: DeclarationSifter, _) <- project params inputDoc
  transformRoundTrip params sifter from to

transformRoundTrip :: (MonadIO m, MonadFail m) => Params DeclarationSifter -> DeclarationSifter -> Position -> Position -> m ()
transformRoundTrip params sifter from to = do
  Just pos <- return $ transformPosition params sifter from
  pos `shouldBe` to

  Just pos' <- return $ untransformPosition params sifter to
  pos' `shouldBe` from
