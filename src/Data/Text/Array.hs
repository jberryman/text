{-# LANGUAGE BangPatterns, CPP, MagicHash, Rank2Types,
    RecordWildCards, UnboxedTuples, UnliftedFFITypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
-- |
-- Module      : Data.Text.Array
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : portable
--
-- Packed, unboxed, heap-resident arrays.  Suitable for performance
-- critical use, both in terms of large data quantities and high
-- speed.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions, e.g.
--
-- > import qualified Data.Text.Array as A
--
-- The names in this module resemble those in the 'Data.Array' family
-- of modules, but are shorter due to the assumption of qualified
-- naming.
module Data.Text.Array
    (
    -- * Types
      Array(..)
    , MArray(..)
    -- * Functions
    , resizeM
    , shrinkM
    , copyM
    , copyI
    , empty
    , equal
    , run
    , run2
    , toList
    , unsafeFreeze
    , unsafeIndex
    , new
    , newPinned
    , newFilled
    , unsafeWrite
    , tile
    ) where

#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Text.Internal.Unsafe (inlinePerformIO)
#endif
import Foreign.C.Types (CInt(..))
import GHC.Exts hiding (toList)
import GHC.ST (ST(..), runST)
import GHC.Word (Word8(..))
import Prelude hiding (length, read)
import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH

-- | Immutable array type.
--
-- The 'Array' constructor is exposed since @text-1.1.1.3@
data Array = Array { aBA :: ByteArray# }

instance TH.Lift Array where
  lift a@(Array a#) = TH.appE (TH.appE (TH.varE 'fromAddr) addr) (TH.lift l)
    where
      l = I# (sizeofByteArray# a#)
      addr = TH.litE $ TH.StringPrimL $ toList a 0 l
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

fromAddr :: Addr# -> Int -> Array
fromAddr addr (I# len) = runST $ ST $ \s1# ->
  case newByteArray# len s1# of
    (# s2#, marr #) -> case copyAddrToByteArray# addr marr 0# len s2# of
      s3# -> case unsafeFreezeByteArray# marr s3# of
        (# s4#, arr #) -> (# s4#, Array arr #)

-- | Mutable array type, for use in the ST monad.
--
-- The 'MArray' constructor is exposed since @text-1.1.1.3@
data MArray s = MArray { maBA :: MutableByteArray# s }

-- | Create an uninitialized mutable array.
new :: forall s. Int -> ST s (MArray s)
new (I# len#)
#if defined(ASSERTS)
  | I# len# < 0 = error "Data.Text.Array.new: size overflow"
#endif
  | otherwise = ST $ \s1# ->
    case newByteArray# len# s1# of
      (# s2#, marr# #) -> (# s2#, MArray marr# #)
{-# INLINE new #-}

-- | Create an uninitialized mutable pinned array.
newPinned :: forall s. Int -> ST s (MArray s)
newPinned (I# len#)
#if defined(ASSERTS)
  | I# len# < 0 = error "Data.Text.Array.newPinned: size overflow"
#endif
  | otherwise = ST $ \s1# ->
    case newPinnedByteArray# len# s1# of
      (# s2#, marr# #) -> (# s2#, MArray marr# #)
{-# INLINE newPinned #-}

newFilled :: Int -> Int -> ST s (MArray s)
newFilled (I# len#) (I# c#) = ST $ \s1# ->
  case newByteArray# len# s1# of
    (# s2#, marr# #) -> case setByteArray# marr# 0# len# c# s2# of
      s3# -> (# s3#, MArray marr# #)
{-# INLINE newFilled #-}

tile :: MArray s -> Int -> ST s ()
tile marr tileLen = do
  totalLen <- getSizeofMArray marr
  let go l
        | 2 * l > totalLen = copyM marr l marr 0 (totalLen - l)
        | otherwise = copyM marr l marr 0 l >> go (2 * l)
  go tileLen
{-# INLINE tile #-}

-- | Freeze a mutable array. Do not mutate the 'MArray' afterwards!
unsafeFreeze :: MArray s -> ST s Array
unsafeFreeze MArray{..} = ST $ \s1# ->
    case unsafeFreezeByteArray# maBA s1# of
        (# s2#, ba# #) -> (# s2#, Array ba# #)
{-# INLINE unsafeFreeze #-}

-- | Unchecked read of an immutable array.  May return garbage or
-- crash on an out-of-bounds access.
unsafeIndex ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Array -> Int -> Word8
unsafeIndex a@Array{..} i@(I# i#) =
#if defined(ASSERTS)
  let word8len = I# (sizeofByteArray# aBA) in
  if i < 0 || i >= word8len
  then error ("Data.Text.Array.unsafeIndex: bounds error, offset " ++ show i ++ ", length " ++ show word8len)
  else
#endif
  case indexWord8Array# aBA i# of r# -> (W8# r#)
{-# INLINE unsafeIndex #-}

-- sizeofMutableByteArray# is deprecated, because it is unsafe in the presence of
-- shrinkMutableByteArray# and resizeMutableByteArray#.
getSizeofMArray :: MArray s -> ST s Int
getSizeofMArray ma@MArray{..} = ST $ \s0# ->
  case getSizeofMutableByteArray# maBA s0# of
    (# s1#, word8len# #) -> (# s1#, I# word8len# #)

#if defined(ASSERTS)
checkBoundsM :: HasCallStack => MArray s -> Int -> Int -> ST s ()
checkBoundsM ma i elSize = do
  len <- getSizeofMArray ma
  if i < 0 || i + elSize > len
    then error ("bounds error, offset " ++ show i ++ ", length " ++ show len)
    else return ()
#endif

-- | Unchecked write of a mutable array.  May return garbage or crash
-- on an out-of-bounds access.
unsafeWrite ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  MArray s -> Int -> Word8 -> ST s ()
unsafeWrite ma@MArray{..} i@(I# i#) (W8# e#) =
#if defined(ASSERTS)
  checkBoundsM ma i 1 >>
#endif
  (ST $ \s1# -> case writeWord8Array# maBA i# e# s1# of
    s2# -> (# s2#, () #))
{-# INLINE unsafeWrite #-}

-- | Convert an immutable array to a list.
toList :: Array -> Int -> Int -> [Word8]
toList ary off len = loop 0
    where loop i | i < len   = unsafeIndex ary (off+i) : loop (i+1)
                 | otherwise = []

-- | An empty immutable array.
empty :: Array
empty = runST (new 0 >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result.
run :: (forall s. ST s (MArray s)) -> Array
run k = runST (k >>= unsafeFreeze)

-- | Run an action in the ST monad and return an immutable array of
-- its result paired with whatever else the action returns.
run2 :: (forall s. ST s (MArray s, a)) -> (Array, a)
run2 k = runST (do
                 (marr,b) <- k
                 arr <- unsafeFreeze marr
                 return (arr,b))
{-# INLINE run2 #-}

resizeM :: MArray s -> Int -> ST s (MArray s)
resizeM ma@MArray{..} i@(I# i#) = ST $ \s1# ->
  case resizeMutableByteArray# maBA i# s1# of
    (# s2#, newArr #) -> (# s2#, MArray newArr #)
{-# INLINE resizeM #-}

shrinkM ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  MArray s -> Int -> ST s ()
shrinkM (MArray marr) i@(I# newSize) = do
#if defined(ASSERTS)
  oldSize <- getSizeofMArray (MArray marr)
  if I# newSize > oldSize
    then error $ "shrinkM: shrink cannot grow " ++ show oldSize ++ " to " ++ show (I# newSize)
    else return ()
#endif
  ST $ \s1# ->
    case shrinkMutableByteArray# marr newSize s1# of
      s2# -> (# s2#, () #)
{-# INLINE shrinkM #-}

-- | Copy some elements of a mutable array.
copyM :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> MArray s               -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyM dst@(MArray dst#) dstOff@(I# dstOff#) src@(MArray src#) srcOff@(I# srcOff#) count@(I# count#)
#if defined(ASSERTS)
  | count < 0 = error $
    "copyM: count must be >= 0, but got " ++ show count
#endif
    | otherwise = do
#if defined(ASSERTS)
    srcLen <- getSizeofMArray src
    dstLen <- getSizeofMArray dst
    if srcOff + count > srcLen
      then error "copyM: source is too short"
      else return ()
    if dstOff + count > dstLen
      then error "copyM: destination is too short"
      else return ()
#endif
    ST $ \s1# -> case copyMutableByteArray# src# srcOff# dst# dstOff# count# s1# of
      s2# -> (# s2#, () #)
{-# INLINE copyM #-}

-- | Copy some elements of an immutable array.
copyI :: MArray s               -- ^ Destination
      -> Int                    -- ^ Destination offset
      -> Array                  -- ^ Source
      -> Int                    -- ^ Source offset
      -> Int                    -- ^ Count
      -> ST s ()
copyI (MArray dst#) dstOff@(I# dstOff#) (Array src#) (I# srcOff#) count@(I# count#)
#if defined(ASSERTS)
  | count < 0 = error $
    "copyI: count must be >= 0, but got " ++ show count
#endif
  | otherwise = ST $ \s1# ->
    case copyByteArray# src# srcOff# dst# dstOff# count# s1# of
      s2# -> (# s2#, () #)
{-# INLINE copyI #-}

-- | Compare portions of two arrays for equality.  No bounds checking
-- is performed.
equal :: Array                  -- ^ First
      -> Int                    -- ^ Offset into first
      -> Array                  -- ^ Second
      -> Int                    -- ^ Offset into second
      -> Int                    -- ^ Count
      -> Bool
equal (Array src1#) (I# off1#) (Array src2#) (I# off2#) (I# count#) = i == 0
  where
#if MIN_VERSION_base(4,11,0)
    i = I# (compareByteArrays# src1# off1# src2# off2# count#)
#else
    i = fromIntegral (inlinePerformIO (memcmp src1# off1# src2# off2# count#))

foreign import ccall unsafe "_hs_text_memcmp" memcmp
    :: ByteArray# -> Int# -> ByteArray# -> Int# -> Int# -> IO CInt
#endif
{-# INLINE equal #-}
