{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}


module Conv (
  StringConv (..)
, toS
, toSL
, Leniency (..)
) where

import Data.ByteString.Char8      as B
import Data.ByteString.Lazy.Char8 as LB
import Data.Text                  as T
import Data.Text.Encoding         as T
import Data.Text.Encoding.Error   as T
import Data.Text.Lazy             as LT
import Data.Text.Lazy.Encoding    as LT
#ifdef __GHCJS__
import Data.JSString              as J
import Data.JSString.Text         as JT
#endif

import Base
import Data.Eq (Eq(..))
import Data.Ord (Ord(..))
import Data.Function ((.), id)
import Data.String (String)
import Control.Applicative (pure)

data Leniency = Lenient | Strict
  deriving (Eq,Show,Ord,Enum,Bounded)

class StringConv a b where
  strConv :: Leniency -> a -> b

toS :: StringConv a b => a -> b
toS = strConv Strict

toSL :: StringConv a b => a -> b
toSL = strConv Lenient

instance StringConv String String where strConv _ = id
instance StringConv String B.ByteString where strConv _ = B.pack
instance StringConv String LB.ByteString where strConv _ = LB.pack
instance StringConv String T.Text where strConv _ = T.pack
instance StringConv String LT.Text where strConv _ = LT.pack
#ifdef __GHCJS__
instance StringConv String J.JSString where strConv _ = J.pack
#endif

instance StringConv B.ByteString String where strConv _ = B.unpack
instance StringConv B.ByteString B.ByteString where strConv _ = id
instance StringConv B.ByteString LB.ByteString where strConv _ = LB.fromChunks . pure
instance StringConv B.ByteString T.Text where strConv = decodeUtf8T
instance StringConv B.ByteString LT.Text where strConv l = strConv l . LB.fromChunks . pure
#ifdef __GHCJS__
-- instance StringConv B.ByteString J.JSString where strConv _ = ??
#endif

instance StringConv LB.ByteString String where strConv _ = LB.unpack
instance StringConv LB.ByteString B.ByteString where strConv _ = B.concat . LB.toChunks
instance StringConv LB.ByteString LB.ByteString where strConv _ = id
instance StringConv LB.ByteString T.Text where strConv l = decodeUtf8T l . strConv l
instance StringConv LB.ByteString LT.Text where strConv = decodeUtf8LT
#ifdef __GHCJS__
-- instance StringConv LB.ByteString J.JSString where strConv _ = ??
#endif

instance StringConv T.Text String where strConv _ = T.unpack
instance StringConv T.Text B.ByteString where strConv _ = T.encodeUtf8
instance StringConv T.Text LB.ByteString where strConv l = strConv l . T.encodeUtf8
instance StringConv T.Text LT.Text where strConv _ = LT.fromStrict
instance StringConv T.Text T.Text where strConv _ = id
#ifdef __GHCJS__
instance StringConv T.Text J.JSString where strConv _ = JT.textToJSString
#endif

instance StringConv LT.Text String where strConv _ = LT.unpack
instance StringConv LT.Text T.Text where strConv _ = LT.toStrict
instance StringConv LT.Text LT.Text where strConv _ = id
instance StringConv LT.Text LB.ByteString where strConv _ = LT.encodeUtf8
instance StringConv LT.Text B.ByteString where strConv l = strConv l . LT.encodeUtf8
#ifdef __GHCJS__
instance StringConv LT.Text J.JSString where strConv _ = JT.lazyTextToJSString
#endif

#ifdef __GHCJS__
instance StringConv J.JSString String where strConv _ = J.unpack
instance StringConv J.JSString T.Text where strConv _ = JT.textFromJSString
instance StringConv J.JSString LT.Text where strConv _ = JT.lazyTextFromJSString
-- instance StringConv J.JSString LB.ByteString where strConv _ = ??
-- instance StringConv J.JSString B.ByteString where strConv l = ??
instance StringConv J.JSString J.JSString where strConv _ = id
#endif


decodeUtf8T :: Leniency -> B.ByteString -> T.Text
decodeUtf8T Lenient = T.decodeUtf8With T.lenientDecode
decodeUtf8T Strict = T.decodeUtf8With T.strictDecode

decodeUtf8LT :: Leniency -> LB.ByteString -> LT.Text
decodeUtf8LT Lenient = LT.decodeUtf8With T.lenientDecode
decodeUtf8LT Strict = LT.decodeUtf8With T.strictDecode
