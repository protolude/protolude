{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Error
( error
) where

import GHC.Prim
import Data.Text (Text, unpack)

#if MIN_VERSION_base(4,9,0)
-- Full stack trace.

import GHC.Types (RuntimeRep)
import CallStack (HasCallStack)
import GHC.Exception (errorCallWithCallStackException)

error :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => Text -> a
error s = raise# (errorCallWithCallStackException (unpack s) ?callstack)

#elif MIN_VERSION_base(4,7,0)
-- Basic Call Stack with callsite.

import GHC.Exception (errorCallException)

error :: Text -> a
error s = raise# (errorCallException (unpack s))

#else

-- No exception tracing.
import GHC.Types
import GHC.Exception

error :: Text -> a
error s = throw (ErrorCall s)

#endif
