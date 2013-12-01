{- CLOCK: TIMESTAMP DURATION
  DURATION is '=> H{1,2}:MM'
  Timestamps:
    <%%(SEXP)>                                   (diary)
    <DATE TIME REPEATER-OR-DELAY>                                (active)
    [DATE TIME REPEATER-OR-DELAY]                                (inactive)
    <DATE TIME REPEATER-OR-DELAY>--<DATE TIME REPEATER-OR-DELAY> (active range)
    <DATE TIME-TIME REPEATER-OR-DELAY>                           (active range)
    [DATE TIME REPEATER-OR-DELAY]--[DATE TIME REPEATER-OR-DELAY] (inactive range)
    [DATE TIME-TIME REPEATER-OR-DELAY]                           (inactive range)
  DATE: YYYY-MM-DD DAYNAME
  TIME: =H:MM~
  REPEATER-OR-DELAY: MARK VALUE UNIT
  MARK:
    - repeater: +, ++, .+
    - delay: -, --

  VALUE: is a number.
  UNIT: h, d, w, m, y
  MARK, VALUE and UNIT are not separated by whitespace characters.

  There can be two REPEATER-OR-DELAY in the timestamp: one as a
  repeater and one as a warning delay.
-}
module Text.OrgMode.Elements.Clock  where

import           Control.Applicative ((<$>), (*>), (<*>), (<*))

import           Data.Functor.Identity

import           Data.Text (Text)

import           Text.Parsec hiding (tokens)

import           Data.Time.LocalTime
import           Data.Time.Calendar
import           Text.OrgMode.Types
import           Text.OrgMode.Parsers.Utils

data Clock = RangeClock Range (Maybe TimeOfDay) | TimestampClock Timestamp
           deriving Show

instance SectionElement Clock

data Timestamp = Timestamp TState LocalTime (Maybe ROD)
               deriving Show

data Range = ShortRange TState Day TimeOfDay TimeOfDay (Maybe ROD)
           | FullRange  TState Timestamp Timestamp
           deriving Show

data TState = Active | Inactive deriving Show

data ROD   = ROD Mark Int Unit                          deriving Show
data Mark  = Cumulate | CatchUp | Restart | All | First deriving Show
data Unit  = H | D | W | M | Y                          deriving Show

clock :: Stream Text Identity a => Parsec Text u Clock
clock = do
  spaces *> string "CLOCK: "
  clock' <* spaces
  where
    clock' = RangeClock <$> range <*> optionMaybe (try len)
             <|> TimestampClock <$> timestamp
    len = spaces *> string "=>" *> spaces *> time

rod :: Stream Text Identity a => Parsec Text u ROD
rod = ROD <$> mark <*> decimal <*> unit
  where
    mark = zipParsers string
           ["++",    "+",      ".+",    "--", "-"]
           [CatchUp, Cumulate, Restart, All,  First]
    unit = zipParsers char ['h', 'd', 'm', 'w', 'y']
                           [ H,   D,   M,   W,   Y ]
    zipParsers f p t = let ps = map f      p
                           ts = map return t
                       in choice $ zipWith (>>) ps ts

parseWithState :: Stream s m Char =>
                  TState -> ParsecT s u m a -> ParsecT s u m a
parseWithState Active   p = between (char '<') (char '>') p
parseWithState Inactive p = between (char '[') (char ']') p

state :: Stream Text Identity a => Parsec Text u TState
state = (try $ lookAhead active) <|> (try $ lookAhead passive)
-- state = tryAhead active <|> tryAhead passive)
  where
    active  = char '<' >> return Active
    passive = char '[' >> return Inactive

date :: Stream Text Identity a => Parsec Text u Day
date = do
  y <- decimal <* char '-'
  m <- decimal <* char '-'
  d <- decimal
  spaces1
  manyTill anyChar (try $ lookAhead spaces1)
  return $ fromGregorian (fromIntegral y) m d

time :: Stream Text Identity a => Parsec Text u TimeOfDay
time = TimeOfDay <$> decimal <* char ':' <*> decimal <*> return 0

timestamp :: Stream Text Identity a => Parsec Text u Timestamp
timestamp = do
  s <- state
  parseWithState s $ timestamp' s
  where
    timestamp' s = do
      d <- date
      spaces1
      t <- time
      r <- optionMaybe $ spaces1 *> rod
      return $ Timestamp s (LocalTime d t) r

range :: Stream Text Identity a => Parsec Text u Range
range = try short <|> try full
  where
    short = do
      s <- state
      parseWithState s $ do
        d <- date
        space
        t1 <- time
        char '-'
        t2 <- time
        r <- optionMaybe rod
        return $ ShortRange s d t1 t2 r
    full = FullRange <$> state <*> timestamp <* string "--" <*> timestamp
