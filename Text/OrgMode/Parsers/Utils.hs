{-# LANGUAGE FlexibleContexts #-}
module Text.OrgMode.Parsers.Utils where

import Control.Applicative ((<$>))

-- import qualified Data.Text as T
import           Data.Text (Text)

import Data.Char

import           Text.Parsec
import           Text.Parsec.Text()
-- import           Text.OrgMode.Types

indent :: (Stream Text m a) => ParsecT Text u m Int
indent = length <$> blanks

strip :: String -> String
strip = d . d
  where d = dropWhile isSpace . reverse

tryAhead :: (Stream s m a, Show a) => ParsecT s u m a -> ParsecT s u m a
tryAhead = try . lookAhead

blank :: (Stream s m Char) => ParsecT s u m Char
blank = tab <|> char ' '

blanks :: (Stream s m Char) => ParsecT s u m String
blanks = many blank <?> "blanks"

satisfy :: (Stream s m a, Show a) => (a -> Bool) -> ParsecT s u m a
satisfy f = fst <$> satisfyWith (\c -> if f c then Just c else Nothing)

satisfyWith :: (Stream s m a, Show a) => (a -> Maybe b) -> ParsecT s u m (a,b)
satisfyWith f = tokenPrim (\c -> show c)
                          (\pos _c _cs -> pos)
                          (\a -> ((,) a) <$> f a)
