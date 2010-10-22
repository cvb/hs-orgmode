{-# LANGUAGE FlexibleContexts, FlexibleInstances, DeriveFunctor #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.OrgMode
-- Copyright   :  (c) Brent Yorgey 2010
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  byorgey@gmail.com
-- Stability   :  experimental
-- Portability :  FlexibleContexts, FlexibleInstances, DeriveFunctor
--
-- Tools for working with emacs org-mode files, including parsers,
-- pretty-printers, and an assortment of transformations.
--
-- XXX include some examples here
-----------------------------------------------------------------------------

module Text.OrgMode (
  -- * Common types
  -- $types

  -- ** Org-mode elements
    Tag, Space, OrgEltRaw(..), OrgElt(..), OrgEltC

  -- ** Pretty-printing
  , Pretty(..), toString

  -- * Flat org-mode documents
  -- $linebyline

  , OrgFlat(..)

  -- ** Parsing
  , readOrgFlatFile, readOrgFlat

  -- ** Printing
  -- $printflat
  , writeOrgFlatFile

  -- * Structured org-mode documents
  -- $struct

  , Org(..), OrgBlock(..)

  -- ** Parsing
  , readOrgFile, readOrg

  -- ** Printing


  ) where

import System.FilePath

import Text.Parsec hiding (satisfy)
import Text.Parsec.String
import Text.Parsec.Pos

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint hiding (char, empty, space, text)

import Data.Monoid
import Control.Applicative hiding (many, (<|>), optional)
import Control.Arrow ((+++))
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
  text          :: String -> e
  sectionHead   :: Int -> Space -> String -> Space -> [Tag] -> Space -> e

  isText        :: e -> Bool
  isSectionHead :: e -> Maybe Int

-- | 'OrgEltRaw' is an instance of 'OrgEltC' which retains all
--   information.
instance OrgEltC OrgEltRaw where
  text        = TextR
  sectionHead = SectionHeadR

  isText (TextR {}) = True
  isText _          = False

  isSectionHead (SectionHeadR n _ _ _ _ _) = Just n
  isSectionHead _                          = Nothing

-- | 'OrgElt' is an instance of 'OrgEltC' which throws away some
--   formatting information.
instance OrgEltC OrgElt where
  text    = Text
  sectionHead n _ title _ tags _ = SectionHead n title tags

  isText (Text {}) = True
  isText _         = False

  isSectionHead (SectionHead n _ _) = Just n
  isSectionHead _                   = Nothing

-- | Class for pretty-printable things.
class Pretty a where
  ppr :: a -> Doc

-- | Convert pretty-printable things to 'String's.
toString :: Pretty a => a -> String
toString = render . ppr

-- | 'OrgElt's can be pretty-printed, inserting nice-looking default
--   amounts of whitespace as necessary.
instance Pretty OrgElt where
  ppr (Text s) = PP.text s
  ppr (SectionHead n title tags) =
      PP.text (replicate n '*') <+> PP.text title <> tagSpace <> pprTags tags
    where tagSpace
            | null tags = PP.empty
            | otherwise = PP.text (replicate count ' ')
            where count = max 2 $ 77 - (n + 1 + length title + tagLen)
          tagLen = 1 + (sum $ map ((+1) . length) tags)

-- | Pretty-print a list of tags, like @:tag1:tag2:tag3:@.
pprTags :: [Tag] -> Doc
pprTags [] = PP.empty
pprTags ts = colon <> hcat (punctuate colon (map PP.text ts)) <> colon

-- | 'OrgEltRaw's can be pretty-printed straightfowardly, since they
--   retain all formatting information from the original source.
instance Pretty OrgEltRaw where
  ppr (TextR s) = PP.text s
  ppr (SectionHeadR n sp1 title sp2 tags sp3) =
      PP.text (replicate n '*') <> PP.text sp1
   <> PP.text title <> PP.text sp2
   <> pprTags tags <> PP.text sp3

------------------------------------------------------------
-- Flat org-mode documents
------------------------------------------------------------

-- $linebyline
-- It is always possible to parse org-mode files line-by-line:
-- meaningful syntactic units (e.g. links) may not span multiple
-- lines.  This section defines a \"flat\" org-mode parser which
-- simply produces a list of elements, one per line.  This may be
-- sufficient for some applications that don't care about nested
-- structure, and is simpler to deal with (using 'map', 'filter', and
-- so on).
--
-- It also defines a printer for this flat org-mode structure. If
-- 'OrgEltRaw' is used, enough information is kept in the parsed AST
-- that round-trip parsing/printing is exact.

-- | \"Flat\" org-mode structure, essentially just a (line-by-line) list
--   of elements, along with the name of the file from which the
--   elements were read.
data OrgFlat a = OrgFlat FilePath [a]
  deriving (Show)

instance Functor OrgFlat where
  fmap f (OrgFlat file elts) = OrgFlat file (fmap f elts)

----------------------------------------
-- Parsing

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

----------------------------------------
-- Actual parser guts (not exported)

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

-- | Parse a sequence of zero or more tags surrounded by colons, like :foo:bar:
parseTags :: Parser [Tag]
parseTags = char ':' *> endBy1 parseTag (char ':') <|> return []
  <?> "tags"

parseTag :: Parser Tag
parseTag = many1 (noneOf ":")

-- | Treat the entire line as raw text.
parseTextLine :: OrgEltC a => Parser a
parseTextLine = text <$> many anyChar

----------------------------------------
-- Printing

-- $printflat
-- To convert an 'OrgFlat' structure to a 'Doc' or 'String', use 'ppr'
-- and 'toString', respectively.  For writing to a file,
-- 'writeOrgFlatFile' is provided.

-- | An 'OrgFlat' structure can be pretty-printed simply by
--   pretty-printing each of its elements in turn.
instance Pretty (OrgFlat OrgEltRaw) where
  ppr (OrgFlat _ elts) = vcat (map ppr elts)

-- | Write out an 'OrgFlat' structure to its corresponding file.  The
--   inability to write an @'OrgFlat' 'OrgElt'@ structure is
--   intentional, as this is almost certainly not what you want to do:
--   reading in such a structure and writing it back out will probably
--   destroy important formatting information (such as list
--   indentation).
writeOrgFlatFile :: OrgFlat OrgEltRaw -> IO ()
writeOrgFlatFile o@(OrgFlat f _) = writeFile f (toString o)

------------------------------------------------------------
-- Structured org-mode documents
------------------------------------------------------------

-- $struct
-- For some applications, it is useful to be able to observe and work
-- with the nested hierarchical structure of org-mode documents
-- directly.  This section defines types, parsers, and pretty-printers
-- for working with such structured views of org-mode documents.

-- | A structured org-mode document.
data Org a = Org FilePath [OrgBlock a]
  deriving (Show, Functor)

-- | A block of content in a structured org-mode document.  A block
--   can either be a single element, or a nested structure with an
--   optional header element and a list of sub-blocks.
data OrgBlock a = BElt a
                  -- ^ A single element
                | BNest (Maybe a) [OrgBlock a]
                  -- ^ A nested section: an optional element (header)
                  --   followed by a list of blocks (section body)
  deriving (Show, Functor)

----------------------------------------
-- Parsing

-- | Convert a flat org-mode document to a structured one.
orgFlatToOrg :: (OrgEltC a, Show a) => OrgFlat a -> Org a
orgFlatToOrg (OrgFlat file elts) =
  case parse (parseBlocks 1) "" elts of
    Left err     -> error (show err)
    Right blocks -> Org file blocks

-- | Parse the given org-mode file into a structured 'Org'
--   document. The type chosen for the elements determines whether
--   formatting information will be retained or discarded.
readOrgFile :: (OrgEltC a, Show a) => FilePath -> IO (Either [ParseError] (Org a))
readOrgFile f = (fmap . fmap) orgFlatToOrg (readOrgFlatFile f)

-- | @'readOrg' file str@ parses the contents of @str@ into a
--   structured 'Org' document.  The type chosen for the elements
--   determines whether formatting information will be retained or
--   discarded.  @file@ should be the name of the file from which
--   @str@ is taken when such a thing makes sense; it is used only in
--   error messages, so passing @\"\"@ (or anything else) as the file
--   argument is fine.
readOrg :: (OrgEltC a, Show a) => FilePath -> String -> Either [ParseError] (Org a)
readOrg f s = orgFlatToOrg <$> readOrgFlat f s

----------------------------------------
-- Actual parser guts (not exported)

-- Note that we parse flat lists of org elements, not Strings!

type EltParser a = Parsec [a] ()

-- | Parse a single content block.
parseBlock :: (OrgEltC a, Show a) => EltParser a (OrgBlock a)
parseBlock = parseText <|> parseSection

-- TODO: structure Text a bit better, i.e. coalesce adjacent ones etc.
-- | Parse a line of text.
parseText :: (OrgEltC a, Show a) => EltParser a (OrgBlock a)
parseText = BElt <$> satisfy isText

-- | Parse a section header followed by some content.
parseSection :: (OrgEltC a, Show a) => EltParser a (OrgBlock a)
parseSection = do
  (secHd, n) <- satisfyWith isSectionHead
  BNest (Just secHd) <$> parseBlocks (n+1)

-- | Parse blocks at section level n (i.e. stop as soon as we see a
--   section at a level less than n).
parseBlocks :: (OrgEltC a, Show a) => Int -> EltParser a [OrgBlock a]
parseBlocks n = manyTill parseBlock (eof <|> parent n)

parent :: (OrgEltC a, Show a) => Int -> EltParser a ()
parent n = lookAhead (satisfy (isParent n)) *> return ()
  where isParent n elt = case isSectionHead elt of
                           Just m  -> m < n
                           Nothing -> False

----------------------------------------
-- Printing

instance Pretty (Org OrgEltRaw) where
  ppr (Org _ bs) = vcat (map ppr bs) $+$ PP.text ""

instance Pretty (OrgBlock OrgEltRaw) where
  ppr (BElt e) = ppr e
  ppr (BNest hdr blocks) = maybe PP.empty ppr hdr $+$ vcat (map ppr blocks)

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

-- Parsing

tryAhead = try . lookAhead

blank :: Parser Char
blank = tab <|> char ' '

blanks :: Parser String
blanks = many blank <?> "blanks"

satisfy :: (Stream s m a, Show a) => (a -> Bool) -> ParsecT s u m a
satisfy f = fst <$> satisfyWith (\c -> if f c then Just c else Nothing)

satisfyWith :: (Stream s m a, Show a) => (a -> Maybe b) -> ParsecT s u m (a,b)
satisfyWith f = tokenPrim (\c -> show c)
                          (\pos _c _cs -> pos)
                          (\a -> ((,) a) <$> f a)
