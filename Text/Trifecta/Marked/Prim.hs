{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Marked.Prim
-- Copyright   :  (C) 2012 Michael Sloan
--                (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Marked.Prim
  ( Marks(..)
  ) where

import Text.Trifecta.IntervalMap
import Text.Trifecta.Rope.Delta

type Marks a = IntervalMap Delta a