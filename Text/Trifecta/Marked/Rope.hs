{-# LANGUAGE MultiParamTypeClasses
           , OverloadedStrings
           , TypeFamilies
           , TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Marked.Rope
-- Copyright   :  (C) 2012 Michael Sloan
--                (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Marked.Rope
  ( MarkedRope(..), Located(..)
  ) where

--TODO:remove unecessary

import qualified Data.ByteString.Lazy.Char8 as L
-- What about this one?
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8
import Data.Foldable as F
import Data.Int (Int64)
import Text.Trifecta.IntervalMap as IM
import Data.Key hiding ((!))
import Data.List (sort)
import Data.Semigroup
import Data.Semigroup.Union
import Prelude hiding (head, take, drop, splitAt)
import System.Console.Terminfo.PrettyPrint
import Text.Blaze
import Text.Blaze.Internal
import Text.Trifecta.Marked.Class
import Text.Trifecta.Rope.Delta
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Prim
import Text.Trifecta.Slice
import Text.PrettyPrint.Free

data MarkedRope a = MarkedRope 
  { ropeMarks   :: !(IntervalMap Delta a)
  , ropeContent :: {-# UNPACK #-} !Rope
  }

mapR f g (MarkedRope m c) = MarkedRope (f m) (g c)

instance HasDelta (MarkedRope a) where
  delta = delta . ropeContent

instance HasBytes (MarkedRope a) where
  bytes = bytes . ropeContent

instance Semigroup (MarkedRope a) where
  MarkedRope ms bs <> b = mapR (union ms . IM.offset (delta bs)) (bs <>) b

instance Monoid (MarkedRope a) where
  mappend = (<>) 
  mempty = MarkedRope mempty mempty

instance Markable (MarkedRope a) where
  type MarkType (MarkedRope a) = a
  addMarks ms = mapR (union ms) id

data Located a = a :@ {-# UNPACK #-} !Int64
infix 5 :@
instance Eq (Located a) where
  _ :@ m == _ :@ n = m == n
instance Ord (Located a) where
  compare (_ :@ m) (_ :@ n) = compare m n

instance Pretty (MarkedRope a) where
  pretty (MarkedRope _ r)
    = hsep $ [ pretty bs | Strand bs _ <- F.toList (strands r)]

type instance Index (MarkedRope a) = Delta
instance BoundedIndex (MarkedRope a) where
  minIndex = const mempty
  maxIndex = delta

instance Reducer (MarkedRope a) (MarkedRope a) where
  unit = id

instance Splittable (MarkedRope a) where
  splitAt i (MarkedRope m c) = (MarkedRope m1 c1, MarkedRope m2 c2)
   where
    m1 = fromList $ intersections (minIndex c) i m
    m2 = fromList . map (first $ fmap (i <>))
       $ intersections i (maxIndex c) m
    (c1, c2) = splitAt i c