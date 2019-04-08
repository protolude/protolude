{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
#if ( __GLASGOW_HASKELL__ >= 800 )
{-# LANGUAGE TypeInType #-}
#endif

#if MIN_VERSION_base(4,9,0)
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Protolude.Error
( error
) where

import GHC.Prim
import Data.Text (Text, unpack)

#if MIN_VERSION_base(4,9,0)
-- Full stack trace.

import GHC.Types (RuntimeRep)
import Protolude.CallStack (HasCallStack)
import GHC.Exception (errorCallWithCallStackException)

{-# WARNING error "'error' remains in code" #-}
error :: forall (r :: RuntimeRep) . forall (a :: TYPE r) . HasCallStack => Text -> a
error s = raise# (errorCallWithCallStackException (unpack s) ?callStack)

#elif MIN_VERSION_base(4,7,0)
-- Basic Call Stack with callsite.

import GHC.Exception (errorCallException)

{-# WARNING error "'error' remains in code" #-}
error :: Text -> a
error s = raise# (errorCallException (unpack s))

#else

-- No exception tracing.
import GHC.Types
import GHC.Exception

{-# WARNING error "'error' remains in code" #-}
error :: Text -> a
error s = throw (ErrorCall (unpack s))

#endif
