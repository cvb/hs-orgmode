{-# LANGUAGE FlexibleContexts, ExistentialQuantification #-}
module Text.OrgMode.Elements.List  where

import           Control.Applicative ((<$>), (<*), (<*>))

import           Data.Functor.Identity

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Parsec hiding (tokens)
-- import           Text.Parsec.Text

import           Text.OrgMode.Types
import           Text.OrgMode.Elements.Paragraph

import           Text.OrgMode.Parsers.Utils


data List = List Int [Item] deriving Show

data Item =
  Item { ind      :: Int
       , bullet   :: Bullet
       , checkbox :: Maybe Checkbox
       , tag      :: Maybe Tag
       , text     :: Text
       , content  :: [ListEl]
       }
  deriving Show

data ListEl = forall b.Show b => ListEl b

type Tag = Text
data Checkbox = CbEmpty | CbFilled | CbHyphen deriving Show
type Name  = Text
type Value = Text

data Bullet = BAsterics
            | BPlus
            | BMinus
            | BCounter Char (Maybe Char)
            deriving Show

instance Show ListEl where show (ListEl b) = show b

instance SectionElement List where

-- list :: (Stream Text Identity a) => Parsec Text u List
list :: Stream Text Identity b => Parsec Text s List
list = do
  i <- try $ lookAhead indent
  itms <- many1 $ try $ listItem i
  return $ List i itms

parseBullet :: Stream Text Identity b => Parsec Text s Bullet
parseBullet = -- string "*" >> (return $ Asterics)
  choice [string "*" >> (return $ BAsterics)
         ,string "+" >> (return $ BPlus)
         ,string "-" >> (return $ BMinus)
         ,BCounter <$> alphaNum <*> optionMaybe (oneOf ".)")
         ]
  <* eot

eot :: Stream Text Identity b => Parsec Text s ()
eot = eof <|> (space >> return ())

tryStr ::  Stream Text Identity b => String -> Parsec Text s String
tryStr = try . string

itemHead :: Stream Text Identity b => Indent -> Parsec Text s Item
itemHead _ = do
  i <- indent
  m <- parseBullet
  c <- parseCheckbox
  t <- parseTag
  b <- T.pack <$> manyTill anyChar newline
  return $ Item i m c (T.pack <$> t) b []
  where
    parseCheckbox = optionMaybe $
                    choice [tryStr "[X]" >> return CbFilled
                           ,tryStr "[]"  >> return CbEmpty
                           ,tryStr "[ ]" >> return CbEmpty
                           ,tryStr "[-]" >> return CbHyphen
                           ] <* eot
    parseTag = optionMaybe $ try $ manyTill anyChar (try $ string " ::") <* eot

listItem :: Stream Text Identity b => Indent -> Parsec Text s Item
listItem i = do
  h <- itemHead i
  c <- parseContent i
  return h{ content = c }

parseContent :: Stream Text Identity b => Indent -> Parsec Text s [ListEl]
parseContent i = manyTill listEl $ tryEnd i

listEl :: Stream Text Identity b => Parsec Text s ListEl
listEl =  (try $ ListEl <$> list)
      <|> (try $ ListEl <$> paragraph)

tryEnd :: Stream Text Identity b => Indent -> Parsec Text s ()
tryEnd i = try $ lookAhead $ endOfList i

endOfList :: Stream Text Identity b => Indent -> Parsec Text s ()
endOfList i = try tryItem
              <|> try checkIndent
              <|> try tryBlanks
              <|> eof
  where
    tryItem     = do
      (Item n _ _ _ _ _ ) <- itemHead i
      case n <= i of
        True -> return ()
        False -> fail "not eol"
    tryBlanks   = count 2 blankLine >> return ()
    checkIndent = do
      newind <- indent
      case newind <= i of
        True  -> return ()
        False -> fail "not eol"
