{-# LANGUAGE FlexibleContexts #-}
module Text.OrgMode.Elements.Drawer  where

import           Control.Applicative ((<$>), (<*), (<*>), (*>))

import           Data.Functor.Identity

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Parsec hiding (tokens)
import           Text.Parsec.Text

import           Text.OrgMode.Types
import           Text.OrgMode.Parsers.Utils

data Drawer = Drawer [SElement] | PropertyDrawer [Property] deriving Show
data Property = Property Name Value deriving Show

instance SectionElement Drawer

parseDrawer :: Stream Text Identity a => Parsec Text u Drawer
parseDrawer = do
  spaces *> char ':'
  name <- manyTill anyChar $ char ':'
  char ':' *> spaces *> newline
  case name of
    "PROPERTIES" -> PropertyDrawer <$> manyTill (try property) drawerEnd
    _            -> fail "custom drawer is not implemented"

drawerEnd = spaces *> string ":END:" *>
            spaces *> (eof <|> (newline >> return ()))

-- parsePropertyDrawer :: Stream Text Identity a => Parsec Text u PropertyDrawer
-- parsePropertyDrawer = return $ PropertyDrawer []

property :: Stream Text Identity a => Parsec Text u Property
property = do
  spaces *> char ':'
  name <- T.pack <$> manyTill anyChar (char ':')
  spaces
  val  <- T.pack <$> manyTill anyChar newline
  return $ Property name val
