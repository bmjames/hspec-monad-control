{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Test.Hspec.Control where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Writer
import Data.Functor (($>))
import Test.Hspec.Core.Spec

instance MonadBase IO (SpecM a)

instance MonadBaseControl IO (SpecM a) where
  type StM (SpecM a) r = (r, [SpecTree a])
  liftBaseWith f = runIO (f runSpecM')
    where runSpecM' (SpecM specs) = runWriterT specs
  restoreM (r, specs) = fromSpecList specs $> r

