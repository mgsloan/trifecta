{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Highlight.Class
-- Copyright   :  (C) 2011 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Text.Trifecta.Marked.Class 
  ( Markable(..)
  ) where

import Text.Trifecta.Marked.Prim

class Markable a where
  type MarkType a
  addMarks :: Marks (MarkType a) -> a -> a
