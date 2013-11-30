{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Text.OrgMode.Types where

import Data.Text


type Indent = Int
type Name   = Text
type Value  = Text

data SElement = forall e. (SectionElement e, Show e) => SElement e
instance Show SElement where show (SElement e) = show e

class SectionElement e where
  -- parseSElement :: (Stream Text m a) => ParsecT Text u m e
