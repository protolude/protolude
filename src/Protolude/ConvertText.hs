{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}

-- | Non-partial text conversion typeclass and functions.
-- For an alternative with partial conversions import 'Protolude.Conv'.
module Protolude.ConvertText (
  ConvertText (..)
, toUtf8
, toUtf8Lazy
) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as LT

import Data.Function (id, (.))
import Data.String (String)
import Data.Text.Encoding (encodeUtf8)

-- | Convert from one Unicode textual type to another. Not for serialization/deserialization,
-- so doesn't have instances for bytestrings.
class ConvertText a b where
  convertText :: a -> b

instance ConvertText String String where convertText = id
instance ConvertText String T.Text where convertText = T.pack
instance ConvertText String LT.Text where convertText = LT.pack

instance ConvertText T.Text String where convertText = T.unpack
instance ConvertText T.Text LT.Text where convertText = LT.fromStrict
instance ConvertText T.Text T.Text where convertText = id

instance ConvertText LT.Text String where convertText = LT.unpack
instance ConvertText LT.Text T.Text where convertText = LT.toStrict
instance ConvertText LT.Text LT.Text where convertText = id

toUtf8 :: ConvertText a T.Text => a -> B.ByteString
toUtf8 =
  encodeUtf8 . convertText

toUtf8Lazy :: ConvertText a T.Text => a -> LB.ByteString
toUtf8Lazy =
  LB.fromStrict . encodeUtf8 . convertText
