{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , FlexibleInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
  #-}
module Text.Trifecta.Slice
  ( Index, BoundedIndex(..), Splittable(..)
  , take, drop) where

import Prelude hiding (take, drop, splitAt)

import Control.Arrow ((***))

import qualified Data.List            as List
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.UTF8 as UTF8

import Data.Int (Int64)
import Data.Semigroup
import Data.Semigroup.Reducer

{-
class BoundedIndex a where
  type Index   a :: *
  type Element a :: *

  indexGet :: Index a -> a -> Maybe (Element a)
  indexSet :: Index a  -> Maybe (Element a) -> a -> a
  unsafeIndexGet :: Index a  -> a -> Element a
  unsafeIndexGet i a = maybe notFound id $ indexGet i a
      where notFound = error "unsafeIndexGet: element not found"

etc etc
-}

type family Index a :: *

class Ord (Index a) => BoundedIndex a where
  -- TODO: explain maxIndex - 1
  minIndex, maxIndex :: a -> Index a

-- | Minimal definition is splitAt, or take and drop.
class Splittable a where
  splitAt :: Index a -> a -> (a, a)

take :: Splittable a => Index a -> a -> a
take i = fst . splitAt i

drop :: Splittable a => Index a -> a -> a
drop i = snd . splitAt i

-- slice

type instance Index [a] = Int
instance BoundedIndex [a] where
  minIndex = const 0
  maxIndex = length

instance Splittable [a] where
  splitAt = List.splitAt

-- Strict instances

instance Semigroup Strict.ByteString where
  (<>) = mappend

type instance Index Strict.ByteString = Int
instance BoundedIndex Strict.ByteString where
  minIndex = const 0
  maxIndex = Strict.length

instance Splittable Strict.ByteString where
  splitAt = Strict.splitAt

-- Lazy instances

instance Semigroup Lazy.ByteString where
  (<>) = mappend

instance Reducer Lazy.ByteString Lazy.ByteString where
  unit = id

type instance Index Lazy.ByteString = Int64
instance BoundedIndex Lazy.ByteString where
  minIndex = const 0
  maxIndex = Lazy.length

instance Splittable Lazy.ByteString where
  splitAt = Lazy.splitAt


-- UTF8 instances

newtype UTF8B = UTF8B UTF8.ByteString

instance Semigroup UTF8B where
  UTF8B x <> UTF8B y = UTF8B $ x <> y

type instance Index UTF8B = Int
instance BoundedIndex UTF8B where
  minIndex = const 0
  maxIndex (UTF8B x) = UTF8.length x

instance Splittable UTF8B where
  splitAt i (UTF8B x) = (UTF8B *** UTF8B) $ UTF8.splitAt i x