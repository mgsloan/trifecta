{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Trifecta.Diagnostic.Class
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Provides a class for logging and throwing expressive diagnostics.
----------------------------------------------------------------------------
module Text.Trifecta.Diagnostic.Class
  ( MonadDiagnostic(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Identity
import Data.Monoid
import Text.Trifecta.Diagnostic.Prim

class Monad m => MonadDiagnostic e m | m -> e where
  throwDiagnostic :: Diagnostic e -> m a
  logDiagnostic   :: Diagnostic e -> m ()

instance MonadDiagnostic e m => MonadDiagnostic e (Lazy.StateT s m) where
  throwDiagnostic = lift . throwDiagnostic
  logDiagnostic = lift . logDiagnostic

instance MonadDiagnostic e m => MonadDiagnostic e (Strict.StateT s m) where
  throwDiagnostic = lift . throwDiagnostic
  logDiagnostic = lift . logDiagnostic

instance MonadDiagnostic e m => MonadDiagnostic e (ReaderT r m) where
  throwDiagnostic = lift . throwDiagnostic
  logDiagnostic = lift . logDiagnostic

instance (MonadDiagnostic e m, Monoid w) => MonadDiagnostic e (Lazy.WriterT w m) where
  throwDiagnostic = lift . throwDiagnostic
  logDiagnostic = lift . logDiagnostic

instance (MonadDiagnostic e m, Monoid w) => MonadDiagnostic e (Strict.WriterT w m) where
  throwDiagnostic = lift . throwDiagnostic
  logDiagnostic = lift . logDiagnostic

instance (MonadDiagnostic e m, Monoid w) => MonadDiagnostic e (Lazy.RWST r w s m) where
  throwDiagnostic = lift . throwDiagnostic
  logDiagnostic = lift . logDiagnostic

instance (MonadDiagnostic e m, Monoid w) => MonadDiagnostic e (Strict.RWST r w s m) where
  throwDiagnostic = lift . throwDiagnostic
  logDiagnostic = lift . logDiagnostic

instance MonadDiagnostic e m => MonadDiagnostic e (IdentityT m) where
  throwDiagnostic = lift . throwDiagnostic
  logDiagnostic = lift . logDiagnostic
