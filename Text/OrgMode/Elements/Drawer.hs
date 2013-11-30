{-# LANGUAGE FlexibleContexts #-}
module Text.OrgMode.Elements.Drawer  where

import           Control.Applicative ((<$>), (*>))

import           Data.Functor.Identity

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Parsec hiding (tokens)

import           Text.OrgMode.Types


data Drawer = Drawer [Element] | PropertyDrawer [Property] deriving Show
data Property = Property Name Value deriving Show

instance SectionElement Drawer

drawer :: Stream Text Identity a => Parsec Text u Drawer
drawer = do
  spaces *> char ':'
  name <- manyTill anyChar (char ':')
  spaces
  case name of
    "PROPERTIES" -> PropertyDrawer <$> manyTill (try property) (try drawerEnd)
    _            -> fail "custom drawer is not implemented"

drawerEnd :: Stream Text Identity a => Parsec Text u ()
drawerEnd = spaces *> string ":END:" *> spaces

-- parsePropertyDrawer :: Stream Text Identity a => Parsec Text u PropertyDrawer
-- parsePropertyDrawer = return $ PropertyDrawer []

property :: Stream Text Identity a => Parsec Text u Property
property = do
  spaces *> char ':'
  name <- T.pack <$> manyTill anyChar (char ':')
  spaces
  val  <- T.pack <$> manyTill anyChar newline
  return $ Property name val
