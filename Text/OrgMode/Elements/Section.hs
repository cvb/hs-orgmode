{-# LANGUAGE ExistentialQuantification #-}
module Text.OrgMode.Elements.Section  where

import           Control.Applicative ((<$>), (<*))

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Parsec hiding (tokens)

import           Text.OrgMode.Types

import           Text.OrgMode.Elements.List
import           Text.OrgMode.Elements.Drawer
import           Text.OrgMode.Elements.Paragraph

import           Text.OrgMode.Parsers.Utils


data Section = Section [SElement] deriving Show

parseSection = try tillHead <|> tillEnd
  where
    tillHead = Section <$>
               manyTill parseSElement (try $ lookAhead parseSectionHead)
    tillEnd  = Section <$> many1 parseSElement
    -- selem =  SElement <$> parseSElement

parseSElement :: Parsec Text [TodoSeq] SElement
parseSElement =  SElement <$> try parseList
             <|> SElement <$> try parseDrawer
             -- <|> SElement <$> try parseFootNote
             -- <|> SElement <$> try parseTable
             <|> SElement <$> parseParagraph
