{-# LANGUAGE FlexibleContexts #-}
-- Simple parser/pretty-printer for emacs org-mode files.

module Text.OrgMode (
  -- * Common types
  -- $types

    Tag, Space, OrgEltRaw(..), OrgElt(..), OrgEltC

  -- * Line-by-line parsing/printing
  -- $linebyline

  , OrgFlat(..)
  , readOrgFlatFile, readOrgFlat
  ) where

import System.FilePath

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Pos

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint hiding (char, empty, space, text)

import Data.Monoid
import Control.Applicative hiding (many, (<|>), optional)
import Control.Arrow
import Data.Traversable (sequenceA)

------------------------------------------------------------
-- Common Types
------------------------------------------------------------

-- $types
-- Some common types used throughout the module.

type Tag = String

type Space = String

-- | A basic element that can occur in an org-mode file.  This is the
--   \"raw\" version which keeps enough information to do exact
--   round-tripping of parsing/printing.
data OrgEltRaw = TextR String
               | SectionHeadR Int Space String Space [Tag] Space
  deriving (Show)

-- | A basic element that can occur in an org-mode file.  This is the
--   \"sanitized\" version that only contains semantic information and
--   throws away the formatting information necessary to do exact
--   round-tripping.
data OrgElt = Text String
            | SectionHead Int String [Tag]
  deriving (Show)

-- | Generic interface for constructing 'OrgElt'-like things.
class OrgEltC e where
  text        :: String -> e
  sectionHead :: Int -> Space -> String -> Space -> [Tag] -> Space -> e

-- | 'OrgEltRaw' is an instance of 'OrgEltC' which retains all
--   information.
instance OrgEltC OrgEltRaw where
  text        = TextR
  sectionHead = SectionHeadR

-- | 'OrgElt' is an instance of 'OrgEltC' which throws away some
--   formatting information.
instance OrgEltC OrgElt where
  text    = Text
  sectionHead n _ title _ tags _ = SectionHead n title tags

------------------------------------------------------------
-- Line-by-line parsing/pretty-printing
------------------------------------------------------------

-- $linebyline
-- It is always possible to parse org-mode files line-by-line.  This
-- section defines a \"flat\" org-mode parser which simply produces a
-- list of elements, one per line.
--
-- It also defines a printer for this flat org-mode structure. If
-- 'OrgEltRaw' is used, enough information is kept in the parsed AST
-- that round-trip parsing/printing is exact.

-- | \"Flat\" org-mode structure, essentially just a (line-by-line) list
--   of elements, along with the name of the file from which the
--   elements were read.
data OrgFlat a = OrgFlat FilePath [a]
  deriving (Show)

-- | Parse the given org-mode file into a flat (line-by-line)
--   'OrgFlat' structure. The type chosen for the elements determines
--   whether formatting information will be retained or discarded.
readOrgFlatFile :: OrgEltC a => FilePath -> IO (Either [ParseError] (OrgFlat a))
readOrgFlatFile f = readOrgFlat f <$> readFile f

-- | @'readOrgFlat' file str@ parses the contents of @str@ into a flat
--   (line-by-line) 'OrgFlat' structure.  The type chosen for the
--   elements determines whether formatting information will be
--   retained or discarded.  @file@ should be the name of the file
--   from which @str@ is taken when such a thing makes sense; it is
--   used only in error messages, so passing @\"\"@ (or anything else)
--   as the file argument is fine.
readOrgFlat :: OrgEltC a => FilePath -> String -> Either [ParseError] (OrgFlat a)
readOrgFlat f s = OrgFlat <$> pure f <*> parseLines s
  where parseLines   = parseCollect f parseOrgLine . lines

-- | Map a single parser over a list of inputs, and return either a
--   list of results or a list of error messages.
parseCollect :: FilePath -> Parser a -> [String] -> Either [ParseError] [a]
parseCollect f p = sequenceA . zipWith (\n -> collectivize . parse (setLine n p) f) [1..]
  where collectivize = (:[]) +++ id
        setLine n p  = setPosition (newPos f n 0) >> p

parseOrgLine :: OrgEltC a => Parser a
parseOrgLine = parseSectionHead <|> parseTextLine  <?> "org line"

parseSectionHead :: OrgEltC a => Parser a
parseSectionHead
  = sectionHead <$> (length <$> many1 (char '*'))
                <*> blanks
                <*> manyTill anyChar (tryAhead (blanks *> parseTags *> blanks *> eof))
                <*> blanks
                <*> parseTags
                <*> blanks
                <*  eof
  <?> "section heading"

tryAhead = try . lookAhead

blank :: Parser Char
blank = tab <|> char ' '

blanks :: Parser String
blanks = many blank <?> "blanks"

word :: Parser String
word = many alphaNum <?> "word"

-- | Parse a sequence of zero or more tags surrounded by colons, like :foo:bar:
parseTags :: Parser [Tag]
parseTags = char ':' *> endBy word (char ':') <|> return []
  <?> "tags"

-- | Treat the entire line as raw text.
parseTextLine :: OrgEltC a => Parser a
parseTextLine = text <$> many anyChar
{-
-- Pretty-printing

formatOrgR :: OrgR -> Doc
formatOrgR (OrgR _ chunks) = vcat (map formatChunkR chunks) <> PP.char '\n'

formatChunkR :: ChunkR -> Doc
formatChunkR (TextR s) = PP.text s
formatChunkR (SectionR n sp1 title sp2 tags sp3) =
    PP.text (replicate n '*') <> PP.text sp1 <> PP.text title <> PP.text sp2 <>
    formatTags tags <> PP.text sp3

formatTags :: [Tag] -> Doc
formatTags [] = PP.empty
formatTags ts = colon <> hcat (punctuate colon (map PP.text ts)) <> colon
-}

-- Converting raw format to more structured format

{-
data Org = Org FilePath [Block]
  deriving (Show)

data Block = PP.Text String
           | Section Int String [Tag] [Block]
  deriving (Show)

orgRToOrg :: OrgR -> Org
orgRToOrg (OrgR file chunks) =
  case runParser (parseBlocks 1) () "" chunks of
    Left err -> error (show err)
    Right blocks -> Org file blocks

type ChunkParser = Parsec [ChunkR] ()

parseBlocks :: Int -> ChunkParser [Block]
parseBlocks n = manyTill parseBlock (eof <|> parent n)

parent :: Int -> ChunkParser ()
parent n = lookAhead (satisfyC (isParent n)) *> return ()
  where isParent n (SectionR m _ _ _ _ _) = m < n
        isParent _ _                      = False

satisfyC :: (Stream s m ChunkR) => (ChunkR -> Bool) -> ParsecT s u m ChunkR
satisfyC f = chunk (\c -> if f c then Just c else Nothing)

chunk :: (Stream s m ChunkR) => (ChunkR -> Maybe a) -> ParsecT s u m a
chunk f = tokenPrim (\c -> show c)
                    (\pos _c _cs -> pos)
                    f

parseBlock :: ChunkParser Block
parseBlock = parseText <|> parseSection

parseText :: ChunkParser Block
parseText = chunk toText
  where toText (TextR t) = Just $ Text t
        toText _         = Nothing

parseSection :: ChunkParser Block
parseSection = do
  (SectionR n _ title _ tags _) <- satisfyC isSectionR
  Section n title tags <$> parseBlocks (n+1)
  where isSectionR (SectionR {}) = True
        isSectionR _             = False
-}

------------------------------------------------------------
-- Miscellaneous/utility
------------------------------------------------------------

-- | An 'Applicative' instance for Either which does monoidal error
--   collection.
instance Monoid w => Applicative (Either w) where
  pure = Right
  (Left w) <*> (Right _)  = Left w
  (Left w1) <*> (Left w2) = Left (w1 `mappend` w2)
  (Right _) <*> (Left w)  = Left w
  (Right f) <*> (Right x) = Right (f x)
