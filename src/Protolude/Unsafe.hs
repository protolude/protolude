{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protolude.Unsafe (
  unsafeHead,
  unsafeTail,
  unsafeInit,
  unsafeLast,
  unsafeFromJust,
  unsafeIndex,
  unsafeThrow,
  unsafeRead,
) where

import Protolude.Base (Int)
import Data.Char (Char)
import Text.Read (Read, read)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Control.Exception as Exc

unsafeHead :: [a] -> a
unsafeHead = List.head

unsafeTail :: [a] -> [a]
unsafeTail = List.tail

unsafeInit :: [a] -> [a]
unsafeInit = List.init

unsafeLast :: [a] -> a
unsafeLast = List.last

unsafeFromJust :: Maybe.Maybe a -> a
unsafeFromJust = Maybe.fromJust

unsafeIndex :: [a] -> Int -> a
unsafeIndex = (List.!!)

unsafeThrow :: Exc.Exception e => e -> a
unsafeThrow = Exc.throw

unsafeRead :: Read a => [Char] -> a
unsafeRead = Text.Read.read
