{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Language.LSP.Notebook.GoParser where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import GHC.Generics

data GoDeclType =
  Import
  | Func
  | Type
  | Var
  | Const
  | Other Text
  deriving (Show, Eq, Generic)

instance FromJSON GoDeclType where
  parseJSON = withText "GoDeclType" $ \t -> case t of
    "Import" -> pure Import
    "Func" -> pure Func
    "Type" -> pure Type
    "Var" -> pure Var
    "Const" -> pure Const
    other -> pure (Other other)

data GoDeclaration = GoDeclaration {
  declType :: GoDeclType
  , startLine :: Maybe Int
  , startCh :: Maybe Int
  , endLine :: Maybe Int
  , endCh :: Maybe Int
  } deriving (Show, Eq, Generic)

instance FromJSON GoDeclaration where
  parseJSON = withObject "GoDeclaration" $ \o -> GoDeclaration
    <$> o .: "type"
    <*> o .: "start_line"
    <*> o .: "start_ch"
    <*> o .: "end_line"
    <*> o .: "end_ch"

parseGoDeclarations :: Text -> Either String [GoDeclaration]
parseGoDeclarations jsonText = eitherDecode (BL.fromStrict $ T.encodeUtf8 jsonText)

isImportLike :: GoDeclaration -> Bool
isImportLike (GoDeclaration {declType = Import}) = True
isImportLike _ = False
