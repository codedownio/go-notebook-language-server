{-# LANGUAGE QuasiQuotes #-}

module Streams (
  parseOne
  , ParseResult(..)
  , freshParse
  ) where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString (Result, IResult(..), Parser, parse, try)
import qualified Data.Attoparsec.ByteString as Attoparsec
import Data.Attoparsec.ByteString.Char8 (decimal, string, skipWhile)
import qualified Data.ByteString as BS
import Data.ByteString.Builder.Extra (defaultChunkSize)
import Data.String.Interpolate
import Data.Text
import System.IO


data ParseResult
  = ParseSuccess BS.ByteString BS.ByteString  -- message and remainder
  | ParseFail [String] String                  -- contexts and error
  | ParseEOF                                   -- end of input
  deriving (Show)

-- | Parse one LSP message from the handle using Attoparsec's incremental parsing.
-- Returns the message bytes and any remainder, or an error.
parseOne :: MonadIO m => Text -> Handle -> Result BS.ByteString -> m (ParseResult, Result BS.ByteString)
parseOne src h = go
  where
    go (Fail bytes ctxs err) = do
      liftIO $ hPutStrLn stderr [i|[Streams, #{src}] parseOne: Fail #{bytes} #{ctxs} #{err}|]
      pure (ParseFail ctxs err, parse parseHeaders "")
    go (Partial c) = do
      liftIO $ hPutStrLn stderr [i|[Streams, #{src}] parseOne: Partial, about to hGetSome (blocking)...|]
      bs <- liftIO $ BS.hGetSome h defaultChunkSize
      liftIO $ hPutStrLn stderr [i|[Streams, #{src}] parseOne: hGetSome returned #{BS.length bs} bytes: |START|#{bs}|END||]
      if BS.null bs
        then do
          liftIO $ hPutStrLn stderr [i|[Streams, #{src}] parseOne: got empty ByteString, returning ParseEOF|]
          pure (ParseEOF, parse parseHeaders "")
        else go (c bs)
    go (Done remainder msg) = do
      liftIO $ hPutStrLn stderr [i|[Streams, #{src}] parseOne: Done, msg=#{BS.length msg} bytes, remainder=#{BS.length remainder} bytes|]
      pure (ParseSuccess msg remainder, parse parseHeaders remainder)

-- | Parse Content-Length and Content-Type headers, then consume the message body.
-- Copied from Language.LSP.Server.Control since it's a hidden module.
parseHeaders :: Parser BS.ByteString
parseHeaders = do
  try contentType <|> return ()
  len <- contentLength
  try contentType <|> return ()
  _ <- string _ONE_CRLF
  Attoparsec.take len
 where
  contentLength = do
    _ <- string "Content-Length: "
    len <- decimal
    _ <- string _ONE_CRLF
    return len

  contentType = do
    _ <- string "Content-Type: "
    skipWhile (/= '\r')
    _ <- string _ONE_CRLF
    return ()

_ONE_CRLF :: BS.ByteString
_ONE_CRLF = "\r\n"

-- | Start a fresh parse
freshParse :: Result BS.ByteString
freshParse = parse parseHeaders ""
