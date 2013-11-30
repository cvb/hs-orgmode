{-# LANGUAGE ExistentialQuantification #-}

module Text.OrgMode.Plain where

import           Control.Applicative ((<$>), (<*))

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Parsec hiding (tokens)

import           Text.OrgMode.Types

import           Text.OrgMode.Keywords.TodoKeywords
import           Text.OrgMode.Elements.Head


class Show a => Elem a

instance Elem Head
instance Elem Section

data Node = forall a.Elem a => Node a

instance Show Node where show (Node a) = show a

type PlainDoc = [Node]

parsePlain :: Text -> Either ParseError PlainDoc
parsePlain raw = do
  (todos, raw') <- getTodos raw
  b <- runParser plain todos "" raw'
  return $ b

-- plain :: Parsec Text s PlainDoc
plain = many $ ((Node <$> parseSectionHead) <|> parseSection)

