{-# LANGUAGE TupleSections #-}

module Text.OrgMode.Elements.Keyword where

import           Control.Applicative ((<$>))

import           Data.Maybe
import           Data.List

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Parsec
import           Text.Parsec.Text

import           Text.OrgMode.Types

data TodoSeq = TodoSeq [Todo] [Done] deriving Show

type Modifier = Text
type Done = Todo
data Todo = Todo Text (Maybe Modifier) deriving Show

defaultTodos :: TodoSeq
defaultTodos = TodoSeq [Todo "TODO" Nothing] [Todo "DONE" Nothing]

data Keyword = Keyword Name Value

getTodos :: Text -> Either ParseError ([TodoSeq], Text)
getTodos raw =
  let ls = T.lines raw
      (ts, ls') = partition (\l -> T.isPrefixOf "#+TODO:" l) ls
  in mapM runTodosParser ts >>= return . (,T.unlines ls')

runTodosParser :: Text -> Either ParseError TodoSeq
runTodosParser txt = do
  let txt'   = fromJust $ T.stripPrefix "#+TODO:" txt
      (t, d) = T.breakOn "|" txt'
  ts <- runParser parseTodos () "" $ T.strip t
  ds <- runParser parseTodos () "" $ T.strip $ T.drop 1 d

  case (ts, ds) of
    ([], []) -> return defaultTodos
    _        -> return $ TodoSeq ts ds

parseTodos :: Parser [Todo]
parseTodos = sepBy parseTodo (many1 space)

parseTodo :: Parser Todo
parseTodo = do
  (try parseMod) <|> (parseNot)
    where
      parseMod = do
        n <- many1 (noneOf " (")
        m <- between (char '(') (char ')') $ many1 $ noneOf " )"
        return $ Todo (T.pack n) $ Just $ T.pack m
      parseNot = do
        n <- many1 $ noneOf " "
        return $ Todo (T.pack n) Nothing

parseKeyword :: Parser Keyword
parseKeyword = do
  spaces >> string "#+"
  name <- T.pack <$> manyTill anyChar (char  ':')
  spaces
  value <- T.pack <$> manyTill anyChar newline
  return $ Keyword name value
