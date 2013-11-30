{-# LANGUAGE ExistentialQuantification #-}

module Text.OrgMode.Plain where

import           Control.Applicative ((<$>))

import           Data.Text (Text)

import           Text.Parsec hiding (tokens)

import           Text.OrgMode.Elements.Head
import           Text.OrgMode.Elements.Section
import           Text.OrgMode.Elements.Keyword


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

plain :: Parsec Text [TodoSeq] PlainDoc
plain = many $ ((Node <$> sectionHead) <|> (Node <$> section))
