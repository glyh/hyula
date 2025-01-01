{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- REF:
  - https://markkarpov.com/tutorial/megaparsec.html
-}

module UrlParser where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

data Uri = Uri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  }
  deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text)
  , authHost :: Text
  , authPort :: Maybe Int
  }
  deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme <?> "valid scheme"
  _ <- char ':'
  uriAuthority <- optional . try $ do
    _ <- string "//"
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar <?> "username"
      _ <- char ':'
      password <- T.pack <$> some alphaNumChar <?> "password"
      _ <- char '@'
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
    authPort <- optional (char ':' *> label "port number" L.decimal)
    return Authority{..}
  return Uri{..}

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttps
  | SchemeHttp
  | SchemeIrc
  | SchemeMailto
  deriving (Show, Eq)

pScheme :: Parser Scheme
pScheme =
  choice
    [ SchemeData <$ string "data"
    , SchemeFile <$ string "file"
    , SchemeFtp <$ string "ftp"
    , SchemeHttps <$ string "https"
    , SchemeHttp <$ string "http"
    , SchemeIrc <$ string "irc"
    , SchemeMailto <$ string "mailto"
    ]
