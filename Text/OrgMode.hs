{-# LANGUAGE FlexibleContexts #-}
-- Simple parser/pretty-printer for emacs org-mode files.

module Text.OrgMode where

import System.FilePath

import Text.Parsec
import Text.Parsec.String

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint hiding (char, empty, space)

import Control.Applicative hiding (many, (<|>), optional)

-- Raw, unstructured parsing

type Tag = String

data OrgR = OrgR FilePath [ChunkR]
  deriving (Show)

type Space = String

data ChunkR = TextR String
            | SectionR Int Space String Space [Tag] Space
  deriving (Show)

parseOrgR :: FilePath -> Parser OrgR
parseOrgR f = OrgR <$> pure f <*> many parseChunkR

parseChunkR :: Parser ChunkR
parseChunkR = parseSectionR <|> parseTextR  <?> "chunk"

parseSectionR :: Parser ChunkR
parseSectionR = SectionR <$> (length <$> many1 (char '*'))
                         <*> many space
                         <*> manyTill anyChar
                               (try (lookAhead (string "\n" <|> count 2 blank)))
                         <*> blanks
                         <*> parseTagsR
                         <*> manyTill blank (string "\n")
  <?> "section"

blank :: Parser Char
blank = tab <|> char ' '

blanks :: Parser String
blanks = many blank

word :: Parser String
word = many alphaNum <?> "word"

parseTagsR :: Parser [Tag]
parseTagsR = optional (char ':') *> endBy word (char ':') <|> return []
  <?> "tags"

parseTextR :: Parser ChunkR
parseTextR = TextR <$> manyTill anyChar (try (lookAhead (string "\n*"))) <* newline
  <?> "block"

-- Pretty-printing

formatOrgR :: OrgR -> Doc
formatOrgR (OrgR _ chunks) = vcat (map formatChunkR chunks) <> PP.char '\n'

formatChunkR :: ChunkR -> Doc
formatChunkR (TextR s) = text s
formatChunkR (SectionR n sp1 title sp2 tags sp3) =
    text (replicate n '*') <> text sp1 <> text title <> text sp2 <>
    formatTags tags <> text sp3

formatTags :: [Tag] -> Doc
formatTags [] = PP.empty
formatTags ts = colon <> hcat (punctuate colon (map text ts)) <> colon


-- Converting raw format to more structured format

data Org = Org FilePath [Block]
  deriving (Show)

data Block = Text String
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
satisfyC f = tokenPrim (\c -> show c)
                       (\pos _c _cs -> pos)
                       (\c -> if f c then Just c else Nothing)

parseBlock :: ChunkParser Block
parseBlock = parseText <|> parseSection

parseText :: ChunkParser Block
parseText = toText <$> satisfyC isTextR
  where isTextR (TextR _) = True
        isTextR _         = False
        toText (TextR t)  = Text t

parseSection :: ChunkParser Block
parseSection = do
  (SectionR n _ title _ tags _) <- satisfyC isSectionR
  Section n title tags <$> parseBlocks (n+1)
  where isSectionR (SectionR {}) = True
        isSectionR _             = False
