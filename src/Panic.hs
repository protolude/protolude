{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Panic (
  FatalError(..),
  panic,
) where

import Base (Show)
import Data.Text (Text)
import Data.Typeable (Typeable)
import CallStack (HasCallStack)
import Control.Exception as X

-- | Uncatchable exceptions thrown and never caught.
data FatalError = FatalError { fatalErrorMessage :: Text }
  deriving (Show, Typeable)

instance Exception FatalError

panic :: HasCallStack => Text -> a
panic a = throw (FatalError a)
