{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.LSP.Notebook.DeclarationSifter (
  DeclarationSifter(..)
  , DeclarationSifterParams(..)
  ) where

import Control.Monad.IO.Class
import qualified Data.Set as Set
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.LSP.Notebook.GoParser
import Language.LSP.Protocol.Types
import Language.LSP.Transformer
import System.Exit (ExitCode(..))
import UnliftIO.Process


-- | The transformation is essentially a permutation of lines.
-- We store both directions for O(1) transform and untransform.
data DeclarationSifter = DeclarationSifter {
  forward :: Vector Int           -- forward[origLine] = outputLine
  , inverse :: Vector Int         -- inverse[outputLine] = origLine (-1 for synthetic)
  , wrapperBodyStart :: Int       -- first output line of wrapper body (0 if no wrapper)
  , wrapperBodyEnd :: Int         -- last output line of wrapper body (0 if no wrapper)
  } deriving Show

data DeclarationSifterParams = DeclarationSifterParams {
  parserCommand :: FilePath
  , execFunctionName :: Text
  , packageHeader :: [Text]       -- e.g. ["package main", ""]
  } deriving Show

instance Transformer DeclarationSifter where
  type Params DeclarationSifter = DeclarationSifterParams

  getParams _ = DeclarationSifterParams "go-parser" "_notebookExec" ["package main", ""]

  project :: MonadIO m => Params DeclarationSifter -> Doc -> m (Doc, DeclarationSifter, Either Text ())
  project params doc = do
    result <- parseGoCode params doc
    let originalLines = docToList doc
    let numOrigLines = length originalLines
    let headerLines = packageHeader params
    let headerSize = length headerLines
    case result of
      Left err -> return (doc, mkIdentitySifter numOrigLines, Left (T.pack err))
      Right declarations -> do
        let (siftedIdxs, nonSiftedIdxs) = partitionIndices originalLines declarations
            siftedLines = map (originalLines !!) siftedIdxs
            nonSiftedLines = map (originalLines !!) nonSiftedIdxs

        if not (null nonSiftedLines) && any (not . T.null . T.strip) nonSiftedLines
          then do
            -- Wrap executable lines in a function
            let wrappedLines = wrapInFunction (execFunctionName params) nonSiftedLines
                allLines = headerLines ++ siftedLines ++ wrappedLines
                numSifted = length siftedIdxs
                -- Wrapper structure: header, sifted, func header, body lines, closing brace
                bodyStart = headerSize + numSifted + 1  -- after package header + sifted + function header
                bodyEnd = headerSize + numSifted + length nonSiftedLines  -- before closing brace
                sifter = mkSifter headerSize numOrigLines siftedIdxs nonSiftedIdxs (Just (bodyStart, bodyEnd))
            return (listToDoc allLines, sifter, Right ())
          else do
            -- No wrapper needed
            let allLines = headerLines ++ siftedLines ++ nonSiftedLines
                sifter = mkSifter headerSize numOrigLines siftedIdxs nonSiftedIdxs Nothing
            return (listToDoc allLines, sifter, Right ())

  transformPosition :: Params DeclarationSifter -> DeclarationSifter -> Position -> Maybe Position
  transformPosition _ sifter (Position l c)
    | origLine < 0 || origLine >= V.length (forward sifter) = Nothing
    | otherwise =
        let outputLine = forward sifter V.! origLine
            c' = if inWrapperBody sifter outputLine then c + 2 else c
        in Just (Position (fromIntegral outputLine) c')
    where
      origLine = fromIntegral l

  untransformPosition :: Params DeclarationSifter -> DeclarationSifter -> Position -> Maybe Position
  untransformPosition _ sifter (Position l c)
    | outputLine < 0 || outputLine >= V.length (inverse sifter) = Nothing
    | origLine < 0 = Nothing  -- synthetic line
    | otherwise =
        let c' = if inWrapperBody sifter outputLine
                 then if c >= 2 then c - 2 else 0
                 else c
        in Just (Position (fromIntegral origLine) c')
    where
      outputLine = fromIntegral l
      origLine = inverse sifter V.! outputLine

-- | Check if an output line is in the wrapper body (needs column adjustment)
inWrapperBody :: DeclarationSifter -> Int -> Bool
inWrapperBody sifter outputLine =
  wrapperBodyStart sifter > 0 &&
  outputLine >= wrapperBodyStart sifter &&
  outputLine <= wrapperBodyEnd sifter

-- | Create identity sifter (no transformation)
mkIdentitySifter :: Int -> DeclarationSifter
mkIdentitySifter n = DeclarationSifter
  { forward = V.fromList [0..n-1]
  , inverse = V.fromList [0..n-1]
  , wrapperBodyStart = 0
  , wrapperBodyEnd = 0
  }

-- | Build the permutation vectors from sifted and non-sifted indices
-- headerOffset is the number of synthetic header lines (e.g. 2 for "package main\n")
mkSifter :: Int -> Int -> [Int] -> [Int] -> Maybe (Int, Int) -> DeclarationSifter
mkSifter headerOffset numOrigLines siftedIdxs nonSiftedIdxs mWrapperBody =
  DeclarationSifter
    { forward = V.fromList fwdList
    , inverse = V.fromList invList
    , wrapperBodyStart = maybe 0 fst mWrapperBody
    , wrapperBodyEnd = maybe 0 snd mWrapperBody
    }
  where
    numSifted = length siftedIdxs
    hasWrapper = maybe False (const True) mWrapperBody

    -- Build forward: origLine -> outputLine
    fwdList = map toOutput [0..numOrigLines-1]
    toOutput origLine =
      case lookup origLine siftedWithOutputPos of
        Just outPos -> outPos
        Nothing -> case lookup origLine nonSiftedWithOutputPos of
          Just outPos -> outPos
          Nothing -> -1  -- shouldn't happen

    siftedWithOutputPos = zip siftedIdxs [headerOffset..]
    nonSiftedWithOutputPos =
      if hasWrapper
      then zip nonSiftedIdxs [headerOffset + numSifted + 1..]  -- skip func header line
      else zip nonSiftedIdxs [headerOffset + numSifted..]

    -- Build inverse: outputLine -> origLine (-1 for synthetic)
    invList = headerInv ++ siftedOrig ++ wrapperInv ++ nonSiftedOrig ++ closingInv
    headerInv = replicate headerOffset (-1)
    siftedOrig = siftedIdxs
    (wrapperInv, nonSiftedOrig, closingInv) =
      if hasWrapper
      then ([-1], nonSiftedIdxs, [-1])  -- func header, body, closing brace
      else ([], nonSiftedIdxs, [])

parseGoCode :: MonadIO m => DeclarationSifterParams -> Doc -> m (Either String [GoDeclaration])
parseGoCode (DeclarationSifterParams {parserCommand}) doc = do
  let input = T.unpack (docToText doc)
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (proc parserCommand []) input
  case exitCode of
    ExitSuccess -> case parseGoDeclarations (T.pack stdout) of
      Left err -> return $ Left [i|Failed to parse Go: #{err}. Stdout: #{stdout}. Stderr: #{stderr}.|]
      Right x -> return $ Right x
    ExitFailure n -> return $ Left ([i|go-parser failed with code #{n}: |] <> stderr)
  where
    docToText :: Doc -> Text
    docToText = T.intercalate "\n" . docToList

-- | Partition line indices into sifted (declarations) and non-sifted (executable)
-- Returns (siftedIndices, nonSiftedIndices) where siftedIndices are in output order
partitionIndices :: [Text] -> [GoDeclaration] -> ([Int], [Int])
partitionIndices originalLines declarations = (allSiftedIndices, nonSiftedIndices)
  where
    declRanges = [(declType decl, getLineRange decl) | decl <- declarations, isValidRange decl]

    -- Group by declaration type in desired order:
    -- imports first, then types, then var/const, then funcs
    importRanges = [range | (Import, range) <- declRanges]
    typeRanges = [range | (Type, range) <- declRanges]
    varRanges = [range | (Var, range) <- declRanges]
    constRanges = [range | (Const, range) <- declRanges]
    funcRanges = [range | (Func, range) <- declRanges]

    -- Sifted indices in OUTPUT order
    allSiftedIndices = concatMap rangeToIndices (importRanges ++ typeRanges ++ varRanges ++ constRanges ++ funcRanges)
    siftedSet = Set.fromList allSiftedIndices

    -- Non-sifted indices in original order
    nonSiftedIndices = [i | i <- [0..length originalLines - 1], i `Set.notMember` siftedSet]

    getLineRange :: GoDeclaration -> (Int, Int)
    getLineRange (GoDeclaration {startLine = Just start, endLine = Just end}) = (start, end)
    getLineRange (GoDeclaration {startLine = Just start, endLine = Nothing}) = (start, start)
    getLineRange _ = (0, 0)

    isValidRange :: GoDeclaration -> Bool
    isValidRange (GoDeclaration {startLine = Just _, endLine = Just _}) = True
    isValidRange (GoDeclaration {startLine = Just _, endLine = Nothing}) = True
    isValidRange _ = False

    rangeToIndices :: (Int, Int) -> [Int]
    rangeToIndices (start, end) = [start - 1 .. end - 1]  -- Convert to 0-based

-- | Wrap executable lines in a function
wrapInFunction :: Text -> [Text] -> [Text]
wrapInFunction functionName executableLines =
  [functionStart] ++ indentedBody ++ [functionEnd]
  where
    functionStart = "func " <> functionName <> "() {"
    functionEnd = "}"
    indentedBody = map (\l -> if T.null l then l else "  " <> l) executableLines
