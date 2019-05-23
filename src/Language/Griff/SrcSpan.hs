{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Griff.SrcSpan where


import           Data.Data

import           Data.Outputable
import           Data.Text (Text)
import           Data.Text.Prettyprint.Doc
import           GHC.Generics

type Line = Int
type Column = Int

data SrcSpan = SrcSpan
  { srcSpanFilename    :: Text
  , srcSpanStartLine   :: Line
  , srcSpanStartColumn :: Column
  , srcSpanEndLine     :: Line
  , srcSpanEndColumn   :: Column
  } deriving (Show, Ord, Generic, Data)

instance Eq SrcSpan where
  _ == _ = True

instance Outputable SrcSpan

instance Semigroup SrcSpan where
  (<>) x y = SrcSpan (srcSpanFilename x) (startLine x y) (startColumn x y) (endLine x y) (endColumn x y)
    where
      startLine x y = min (srcSpanStartLine x) (srcSpanStartLine y)
      startColumn x y | srcSpanStartLine x < srcSpanStartLine y = srcSpanStartColumn x
                      | srcSpanStartLine x == srcSpanStartLine y = min (srcSpanStartColumn x) (srcSpanStartColumn y)
                      | otherwise = srcSpanStartColumn y
      endLine x y = max (srcSpanEndLine x) (srcSpanEndLine y)
      endColumn x y | srcSpanEndLine x < srcSpanEndLine y = srcSpanEndColumn y
                    | srcSpanEndLine x == srcSpanEndLine y = max (srcSpanEndColumn x) (srcSpanEndColumn y)
                    | otherwise = srcSpanEndColumn x


instance Pretty SrcSpan where
  pretty (SrcSpan filename startLine startColumn endLine endColumn) =
    parens $ align $ sep [ pretty filename
                         , pretty (startLine, startColumn) <> "-" <> pretty (endLine, endColumn) ]

class HasSrcSpan a where
  srcSpan :: a -> SrcSpan

instance HasSrcSpan SrcSpan where
  srcSpan = id

instance HasSrcSpan a => HasSrcSpan [a] where
  srcSpan xs = foldl1 (<>) (map srcSpan xs)

noSrcSpan :: SrcSpan
noSrcSpan = SrcSpan "no info" 0 0 0 0
