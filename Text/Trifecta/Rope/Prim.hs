{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, BangPatterns, PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Rope.Prim
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Rope.Prim
  ( Rope(..)
  , rope
  , Strand(..)
  , strand
  , strands
  , grabRest
  , grabLine
  ) where

import Prelude hiding (splitAt)
import Control.Arrow ((***), second)
import Data.Semigroup
import Data.Semigroup.Reducer
import Data.ByteString (ByteString)
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.UTF8 as UTF8
import Data.FingerTree as FingerTree
import Data.Foldable (toList)
import Data.Hashable
import Data.Int (Int64)
import Text.Trifecta.Slice
import Text.Trifecta.Util.Combinators as Util
import Text.Trifecta.Rope.Bytes
import Text.Trifecta.Rope.Delta

data Strand
  = Strand        {-# UNPACK #-} !ByteString !Delta
  | LineDirective {-# UNPACK #-} !ByteString {-# UNPACK #-} !Int64
  deriving Show

strand :: ByteString -> Strand
strand bs = Strand bs (delta bs)

instance Hashable Strand where
  hash (Strand h _) = hashWithSalt 0 h
  hash (LineDirective p l) = hash l `hashWithSalt` p

instance HasBytes Strand where
  bytes (Strand _ d) = bytes d
  bytes _            = 0

instance HasDelta Strand where
  delta = measure

instance Measured Delta Strand where
  measure (Strand _ s) = delta s
  measure (LineDirective p l) = delta (Directed p l 0 0 0)

-- Monoid / Semigroup instances that disregard LineDirectives

instance Monoid Strand where
  mempty = Strand Strict.empty mempty
  mappend = (<>)

instance Semigroup Strand where
  Strand b1 d1   <>   Strand b2 d2 = Strand (b1 <> b2) (d1 <> d2)
  _              <> s@(Strand _ _) = s
  s@(Strand _ _) <> _              = s

instance Reducer Strand Strand where
  unit = id

{-
type instance Index Strand = Int
instance Splittable Strand where
  type Slice Strand = Strand
  take i (Strand b d) = 
-}

data Rope = Rope !Delta !(FingerTree Delta Strand) deriving Show

rope :: FingerTree Delta Strand -> Rope
rope r = Rope (measure r) r

strands :: Rope -> FingerTree Delta Strand
strands (Rope _ r) = r

grabRest :: Delta -> Rope -> r -> (Delta -> Lazy.ByteString -> r) -> r
grabRest i t kf ks = trim (delta l) (bytes i - bytes l) (toList r) where
  trim j 0 (Strand h _ : xs) = go j h xs
  trim _ k (Strand h _ : xs) = go i (Strict.drop (fromIntegral k) h) xs
  trim j k (p          : xs) = trim (j <> delta p) k xs 
  trim _ _ []                = kf
  go j h s = ks j $ Lazy.fromChunks $ h : [ a | Strand a _ <- s ]
  (l, r) = FingerTree.split (\b -> bytes b > bytes i) $ strands t

-- | Grab the contents of a rope from a given location up to a newline.
grabLine :: Delta -> Rope -> r -> (Delta -> Strict.ByteString -> r) -> r
grabLine i t kf ks = grabRest i t kf $ \c -> ks c . Util.fromLazy . Util.takeLine

instance HasBytes Rope where
  bytes = bytes . measure

instance HasDelta Rope where
  delta = measure

instance Measured Delta Rope where
  measure (Rope s _) = s

instance Monoid Rope where
  mempty = Rope mempty mempty
  mappend = (<>)

instance Semigroup Rope where
  Rope mx x <> Rope my y = Rope (mx <> my) (x `mappend` y)

instance Reducer Rope Rope where
  unit = id

instance Reducer Strand Rope where
  unit s = rope (FingerTree.singleton s)
  cons s (Rope mt t) = Rope (delta s `mappend` mt) (s <| t)
  snoc (Rope mt t) !s = Rope (mt `mappend` delta s) (t |> s)

instance Reducer Strict.ByteString Rope where
  unit = unit . strand
  cons = cons . strand
  snoc r = snoc r . strand

instance Reducer [Char] Rope where
  unit = unit . strand . UTF8.fromString
  cons = cons . strand . UTF8.fromString
  snoc r = snoc r . strand . UTF8.fromString

type instance Index Strand = Int
instance Splittable Strand where
--  splitAt i (Strand b d) = (Strand l i) ()
  splitAt i (Strand b d) = strand *** strand $ splitAt i b
  -- TODO: does this make sense?
  splitAt i x = (x, x)

-- | Grab the contents of a rope from a given location.
type instance Index Rope = Delta
instance BoundedIndex Rope where
  minIndex = const mempty
  maxIndex = measure

instance Splittable Rope where
  splitAt i t
    = rope *** rope 
    $ case viewl r of
        EmptyL -> (l, FingerTree.empty)
        (s :< r') -> (l |>) *** (<| r) 
                   $ splitAt (fromIntegral $ bytes i - bytes l) s
   where
    (l, r) = FingerTree.split (\b -> bytes b > bytes i) $ strands t
