{-# LANGUAGE DeriveGeneric #-}
module Text.OrgMode.Elements.Head where

import Control.Applicative hiding ((<|>), many)

import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec
import GHC.Generics

import Text.OrgMode.Elements.Keyword
import Text.OrgMode.Parsers.Utils

data Head = Head { level    :: Int
                 , keyword  :: Maybe Text
                 , priority :: Maybe Char
                 , title    :: Maybe Text
                 , tags     :: [Text]
                 } deriving (Show, Generic)

sectionHead :: Parsec Text [TodoSeq] Head
sectionHead = do
  stars  <- length <$> many1 (char '*') <* blanks
  todokw <- parseTodoKw                 <* blanks
  pri    <- parsePri                    <* blanks
  title  <- parseTitle                  <* blanks
  tags   <- parseTags                   <* blanks
  eoh
  return $ Head stars todokw pri title tags

parsePri :: Parsec Text [TodoSeq] (Maybe Char)
parsePri =
  Just <$> tryAhead (string "[#" *> anyChar <* char ']') <|> return Nothing

parseTitle :: Parsec Text [TodoSeq] (Maybe Text)
parseTitle = optionMaybe $ T.pack <$> title
  where
    title = manyTill anyChar
            (try $ lookAhead (blanks >> parseTags >> blanks >> eoh))

parseTodoKw :: Parsec Text [TodoSeq] (Maybe Text)
parseTodoKw = try parseTodoKw' <|> return Nothing
  where
    parseTodoKw' = do
      tds  <- getState
      todo <- manyTill anyChar blank
      case haveTodo tds (T.pack todo) of
        True  -> return $ Just $ T.pack todo
        False -> fail "no such kw in list"
    haveTodo [] _ = False
    haveTodo ((TodoSeq ts ds):tds) todo =
      ts `contains` todo || ds `contains` todo || haveTodo tds todo
    contains [] _ = False
    contains ((Todo n _):tds) txt = n == txt || contains tds txt

parseTags :: Parsec Text [TodoSeq] [Text]
parseTags = char ':' *> sepBy1 parseTag (char ' ') <* (char ':') <|> return []

parseTag :: Parsec Text [TodoSeq] Text
parseTag = T.pack <$> many1 (noneOf ": ")

eoh :: Parsec Text [TodoSeq] ()
eoh = eof <|> (newline >> return ())
