{-# LANGUAGE FlexibleInstances
           , OverloadedStrings
           , TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Rope.Highlighted
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Rope.Highlighted
  ( HighlightedRope, Highlights
  ) where

import Text.Trifecta.IntervalMap
import Text.Trifecta.Marked.Prim
import Text.Trifecta.Marked.Rope
import Text.Trifecta.Highlight.Effects
import Text.Trifecta.Highlight.Prim
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Prim

import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Foldable as F
import Data.Key (mapWithKey)
import Data.List (sort)
import Data.Semigroup
import Data.Semigroup.Union
import System.Console.Terminfo.PrettyPrint
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Html5 hiding (b,i)
import Text.Blaze.Html5.Attributes hiding (title)

type Highlights = Marks Highlight
type HighlightedRope = MarkedRope Highlight

instance ToHtml HighlightedRope where
  toHtml (MarkedRope intervals r) = pre $ go 0 lbs effects where 
    lbs = L.fromChunks [bs | Strand bs _ <- F.toList (strands r)]
    ln no = a ! name (toValue $ "line-" ++ show no) $ Empty
    effects = sort $ [ i | (Interval lo hi, tok) <- intersections mempty (delta r) intervals
                     , i <- [ (Leaf "span" "<span" ">" ! class_ (toValue $ show tok)) :@ bytes lo
                            , preEscapedString "</span>" :@ bytes hi
                            ]
                     ] ++ mapWithKey (\k i -> ln k :@ i) (L.elemIndices '\n' lbs)
    go _ cs [] = unsafeLazyByteString cs
    go b cs ((eff :@ eb) : es) 
      | eb <= b = eff >> go b cs es 
      | otherwise = unsafeLazyByteString om >> go eb nom es
         where (om,nom) = L.splitAt (fromIntegral (eb - b)) cs
 
instance PrettyTerm HighlightedRope where
  prettyTerm (MarkedRope intervals r) = go 0 lbs effects where
    lbs = L.fromChunks [bs | Strand bs _ <- F.toList (strands r)]
    effects = sort $ [ i | (Interval lo hi, tok) <- intersections mempty (delta r) intervals
                     , i <- [ pushToken tok :@ bytes lo
                            , popToken tok  :@ bytes hi
                            ]
                     ]
    go _ cs [] = prettyTerm (LazyUTF8.toString cs)
    go b cs ((eff :@ eb) : es) 
      | eb <= b = eff <> go b cs es 
      | otherwise = prettyTerm (LazyUTF8.toString om) <> go eb nom es
         where (om,nom) = L.splitAt (fromIntegral (eb - b)) cs
