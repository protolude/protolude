{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Protolude (
  module X,
  module Base,
  identity,
  map,
  uncons,
  unsnoc,
  applyN,
  print,
  throwIO,
  throwTo,
  show,
  pass,
  guarded,
  guardedA,
  LText,
  LByteString,
  liftIO1,
  liftIO2,
#if !MIN_VERSION_base(4,8,0)
  (&),
  scanl',
#endif
  die,
) where

-- Protolude module exports.
import Debug as X
import Protolude.List as X
import Protolude.Show as X
import Protolude.Bool as X
import Protolude.Monad as X
import Protolude.Functor as X
import Protolude.Either as X
import Protolude.Applicative as X
import Protolude.Conv as X
import Protolude.Panic as X
import Protolude.Exceptions as X
import Protolude.Semiring as X

import Protolude.Base as Base hiding (
    putStr           -- Overriden by Show.putStr
  , putStrLn         -- Overriden by Show.putStrLn
  , print            -- Overriden by Protolude.print
  , show             -- Overriden by Protolude.show
  , showFloat        -- Custom Show instances deprecated.
  , showList         -- Custom Show instances deprecated.
  , showSigned       -- Custom Show instances deprecated.
  , showSignedFloat  -- Custom Show instances deprecated.
  , showsPrec        -- Custom Show instances deprecated.
  )
import qualified Protolude.Base as PBase

-- Used for 'show', not exported.
import Data.String (String)
import Data.String as X (IsString)

-- Maybe'ized version of partial functions
import Protolude.Safe as X (
    headMay
  , headDef
  , initMay
  , initDef
  , initSafe
  , tailMay
  , tailDef
  , tailSafe
  , lastDef
  , lastMay
  , foldr1May
  , foldl1May
  , foldl1May'
  , maximumMay
  , minimumMay
  , maximumDef
  , minimumDef
  , atMay
  , atDef
  )

-- Applicatives
import Control.Applicative as X (
    Applicative(..)
  , Alternative(..)
  , Const(..)
  , ZipList(..)
  , (<**>)
  , liftA
  , liftA2
  , liftA3
  , optional
  )

-- Base typeclasses
import Data.Eq as X (
    Eq(..)
  )
import Data.Ord as X (
    Ord(..)
  , Ordering(..)
  , Down(..)
  , comparing
  )
import Data.Traversable as X
import Data.Foldable as X hiding (
    foldr1
  , foldl1
  , product
  , sum
  )
import Data.Functor.Identity as X (
    Identity(..)
  )

#if MIN_VERSION_base(4,9,0)
import Data.List.NonEmpty as X (
    NonEmpty(..)
  , nonEmpty
  )
import Data.Semigroup as X (
    Semigroup(sconcat, stimes)
  , WrappedMonoid
  , Option(..)
  , option
  , diff
  , cycle1
  , stimesMonoid
  , stimesIdempotent
  , stimesIdempotentMonoid
  , mtimesDefault
  )
#endif

import Data.Monoid as X

#if !MIN_VERSION_base(4,8,0)
import Protolude.Bifunctor as X (Bifunctor(..))
#else
import Data.Bifunctor as X (Bifunctor(..))
#endif

-- Deepseq
import Control.DeepSeq as X (
    NFData(..)
  , ($!!)
  , deepseq
  , force
  )

-- Data structures
import Data.Tuple as X (
    fst
  , snd
  , curry
  , uncurry
  , swap
  )

import Data.List as X (
    splitAt
  , break
  , intercalate
  , isPrefixOf
  , drop
  , filter
  , reverse
  , replicate
  , take
  , sortBy
  , sort
  , intersperse
  , transpose
  , subsequences
  , permutations
  , scanl
#if MIN_VERSION_base(4,8,0)
  , scanl'
#endif
  , scanr
  , iterate
  , repeat
  , cycle
  , unfoldr
  , takeWhile
  , dropWhile
  , group
  , inits
  , tails
  , zipWith
  , zip
  , unzip
  , genericLength
  , genericTake
  , genericDrop
  , genericSplitAt
  , genericReplicate
  )

#if !MIN_VERSION_base(4,8,0)
-- These imports are required for the scanl' rewrite rules
import GHC.Exts (build)
import Data.List (tail)
#endif

-- Hashing
import Data.Hashable as X (
    Hashable
  , hash
  , hashWithSalt
  , hashUsing
  )

import Data.Map as X (Map)
import Data.Set as X (Set)
import Data.Sequence as X (Seq)
import Data.IntMap as X (IntMap)
import Data.IntSet as X (IntSet)

#if MIN_VERSION_base(4,7,0)
import Data.Proxy as X (
    Proxy(..)
  )

import Data.Typeable as X (
    TypeRep
  , Typeable
  , typeRep
  , cast
  , eqT
  )

import Data.Type.Coercion as X (
    Coercion(..)
  , coerceWith
  , repr
  )

import Data.Type.Equality as X (
    (:~:)(..)
  , type (==)
  , sym
  , trans
  , castWith
  , gcastWith
  )

#endif

#if MIN_VERSION_base(4,8,0)
import Data.Void as X (
    Void
  , absurd
  , vacuous
  )
#endif

-- Monad transformers
import Control.Monad.State as X (
    MonadState
  , State
  , StateT(StateT)
  , put
  , get
  , gets
  , modify
  , state
  , withState

  , runState
  , execState
  , evalState

  , runStateT
  , execStateT
  , evalStateT
  )

import Control.Monad.Reader as X (
    MonadReader
  , Reader
  , ReaderT(ReaderT)
  , ask
  , asks
  , local
  , reader
  , runReader
  , runReaderT
  )

import Control.Monad.Trans.Except as X (
    throwE
  , catchE
  )

import Control.Monad.Except as X (
    MonadError
  , Except
  , ExceptT(ExceptT)
  , throwError
  , catchError
  , runExcept
  , runExceptT
  , mapExcept
  , mapExceptT
  , withExcept
  , withExceptT
  )

import Control.Monad.Trans as X (
    MonadIO
  , lift
  , liftIO
  )

-- Base types
import Data.Int as X (
    Int
  , Int8
  , Int16
  , Int32
  , Int64
  )
import Data.Bits as X hiding (
    unsafeShiftL
  , unsafeShiftR
  )
import Data.Word as X (
    Word
  , Word8
  , Word16
  , Word32
  , Word64
#if MIN_VERSION_base(4,7,0)
  , byteSwap16
  , byteSwap32
  , byteSwap64
#endif
  )

import Data.Either as X (
    Either(..)
  , either
  , lefts
  , rights
  , partitionEithers
#if MIN_VERSION_base(4,7,0)
  , isLeft
  , isRight
#endif
  )

import Data.Complex as X (
    Complex(..)
  , realPart
  , imagPart
  , mkPolar
  , cis
  , polar
  , magnitude
  , phase
  , conjugate
  )
import Data.Char as X (chr)
import Data.Bool as X hiding (bool)
import Data.Maybe as X hiding (fromJust)

import Data.Function as X (
    const
  , (.)
  , ($)
  , flip
  , fix
  , on
#if MIN_VERSION_base(4,8,0)
  , (&)
#endif
  )

-- Genericss
import GHC.Generics as X (
    Generic(..)
  , Generic1
  , Rep
  , K1(..)
  , M1(..)
  , U1(..)
  , V1
  , D1
  , C1
  , S1
  , (:+:)(..)
  , (:*:)(..)
  , (:.:)(..)
  , Rec0
  , Constructor(..)
  , Datatype(..)
  , Selector(..)
  , Fixity(..)
  , Associativity(..)
#if ( __GLASGOW_HASKELL__ >= 800 )
  , Meta(..)
  , FixityI(..)
  , URec
#endif
  )

-- ByteString
import qualified Data.ByteString.Lazy
import Data.ByteString as X (ByteString)

-- Text
import Data.Text as X (Text)
import qualified Data.Text.Lazy

import Data.Text.IO as X (
    getLine
  , getContents
  , interact
  , readFile
  , writeFile
  , appendFile
  )

import Data.Text.Lazy as X (
    toStrict
  , fromStrict
  )

import Data.Text.Encoding as X (
    encodeUtf8
  , decodeUtf8
  , decodeUtf8'
  , decodeUtf8With
  )

import Data.Text.Encoding.Error as X (
    OnDecodeError
  , OnError
  , UnicodeException
  , lenientDecode
  , strictDecode
  , ignore
  , replace
  )

-- IO
import System.Environment as X (getArgs)
import qualified System.Exit
import System.Exit as X (
    ExitCode(..)
  , exitWith
  , exitFailure
  , exitSuccess
  )
import System.IO as X (
    Handle
  , FilePath
  , IOMode(..)
  , stdin
  , stdout
  , stderr
  , withFile
  , openFile
  )

-- ST
import Control.Monad.ST as X (
    ST
  , runST
  , fixST
  )

-- Concurrency and Parallelism
import Control.Exception as X hiding (
    throw    -- Impure throw is forbidden.
  , throwIO
  , throwTo
  , assert
  , Handler(..)
  )

import qualified Control.Exception

import Control.Monad.STM as X (
    STM
  , atomically
#if !(MIN_VERSION_stm(2,5,0))
  , always
  , alwaysSucceeds
#endif
  , retry
  , orElse
  , check
  , throwSTM
  , catchSTM
  )
import Control.Concurrent as X hiding (
    throwTo
  , yield
  )
import Control.Concurrent.Async as X (
    Async(..)
  , Concurrently(..)
  , async
  , asyncBound
  , asyncOn
  , withAsync
  , withAsyncBound
  , withAsyncOn
  , wait
  , poll
  , waitCatch
  , cancel
  , cancelWith
  , asyncThreadId
  , waitAny
  , waitAnyCatch
  , waitAnyCancel
  , waitAnyCatchCancel
  , waitEither
  , waitEitherCatch
  , waitEitherCancel
  , waitEitherCatchCancel
  , waitEither_
  , waitBoth
  , link
  , link2
  , race
  , race_
  , concurrently
  )

import Foreign.Ptr as X (IntPtr, WordPtr)
import Foreign.Storable as X (Storable)
import Foreign.StablePtr as X (StablePtr)

-- Read instances hiding unsafe builtins (read)
import Text.Read as X (
    Read
  , reads
  , readMaybe
  , readEither
  )

-- Type synonymss for lazy texts
type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString


#if !MIN_VERSION_base(4,8,0)
infixl 1 &

(&) :: a -> (a -> b) -> b
x & f = f x
#endif

identity :: a -> a
identity x = x

map :: Functor f => (a -> b) -> f a -> f b
map = fmap

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)

unsnoc :: [x] -> Maybe ([x],x)
unsnoc = foldr go Nothing
  where
    go x mxs = Just (case mxs of
       Nothing -> ([], x)
       Just (xs, e) -> (x:xs, e))

applyN :: Int -> (a -> a) -> a -> a
applyN n f = X.foldr (.) identity (X.replicate n f)

print :: (X.MonadIO m, PBase.Show a) => a -> m ()
print = liftIO . PBase.print

throwIO :: (X.MonadIO m, Exception e) => e -> m a
throwIO = liftIO . Control.Exception.throwIO

throwTo :: (X.MonadIO m, Exception e) => ThreadId -> e -> m ()
throwTo tid e = liftIO (Control.Exception.throwTo tid e)

-- | Do nothing returning unit inside applicative.
pass :: Applicative f => f ()
pass = pure ()

guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded p x = X.bool empty (pure x) (p x)

guardedA :: (Functor f, Alternative t) => (a -> f Bool) -> a -> f (t a)
guardedA p x = X.bool empty (pure x) <$> p x

-- | Lift an 'IO' operation with 1 argument into another monad
liftIO1 :: MonadIO m => (a -> IO b) -> a -> m b
liftIO1 = (.) liftIO

-- | Lift an 'IO' operation with 2 arguments into another monad
liftIO2 :: MonadIO m => (a -> b -> IO c) -> a -> b -> m c
liftIO2 = ((.).(.)) liftIO

show :: (Show a, StringConv String b) => a -> b
show x = toS (PBase.show x)
{-# SPECIALIZE show :: Show  a => a -> Text  #-}
{-# SPECIALIZE show :: Show  a => a -> LText  #-}
{-# SPECIALIZE show :: Show  a => a -> ByteString  #-}
{-# SPECIALIZE show :: Show  a => a -> LByteString  #-}
{-# SPECIALIZE show :: Show  a => a -> String  #-}

#if MIN_VERSION_base(4,8,0)
die :: Text -> IO a
die err = System.Exit.die (toS err)
#else
die :: Text -> IO a
die err = hPutStrLn stderr err >> exitFailure
#endif

#if !MIN_VERSION_base(4,8,0)
-- This is a literal copy of the implementation in GHC.List in base-4.10.1.0.

-- | A strictly accumulating version of 'scanl'
{-# NOINLINE [1] scanl' #-}
scanl'           :: (b -> a -> b) -> b -> [a] -> [b]
scanl' = scanlGo'
  where
    scanlGo'           :: (b -> a -> b) -> b -> [a] -> [b]
    scanlGo' f !q ls    = q : (case ls of
                            []   -> []
                            x:xs -> scanlGo' f (f q x) xs)

{-# RULES
"scanl'"  [~1] forall f a bs . scanl' f a bs =
  build (\c n -> a `c` foldr (scanlFB' f c) (flipSeqScanl' n) bs a)
"scanlList'" [1] forall f a bs .
    foldr (scanlFB' f (:)) (flipSeqScanl' []) bs a = tail (scanl' f a bs)
 #-}

{-# INLINE [0] scanlFB' #-}
scanlFB' :: (b -> a -> b) -> (b -> c -> c) -> a -> (b -> c) -> b -> c
scanlFB' f c = \b g -> \x -> let !b' = f x b in b' `c` g b'

{-# INLINE [0] flipSeqScanl' #-}
flipSeqScanl' :: a -> b -> a
flipSeqScanl' a !_b = a
#endif
