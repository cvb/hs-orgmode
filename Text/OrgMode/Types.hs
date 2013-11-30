{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Text.OrgMode.Types where

import Data.Text


type Indent = Int
type Name   = Text
type Value  = Text

data Element = forall e. (SectionElement e, Show e) => Element e
instance Show Element where show (Element e) = show e

class SectionElement e where
  -- parseSElement :: (Stream Text m a) => ParsecT Text u m e
