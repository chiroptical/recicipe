{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Parser where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

newtype Entries = Entries
  { entries :: [Text]
  }
  deriving (Eq, Show)

notBrace :: Parser Char
notBrace = noneOf ("[]" :: String)

parseToken :: Parser Text
parseToken = dbg "token" . try $ do
  thing <- char '[' *> many (anySingleBut ']') <* char ']'
  return . Text.pack $ thing

parseEntries :: Parser Entries
parseEntries = dbg "entries" . try $ do
  entries <- many notBrace *> sepEndBy parseToken (many notBrace)
  return . Entries $ entries
