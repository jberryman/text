{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

-- |
-- Module      : Data.Text.Lazy.Search
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- /Warning/: this is an internal module, and does not have a stable
-- API or name. Functions in this module may not check or enforce
-- preconditions expected by public modules. Use at your own risk!
--
-- Fast substring search for lazy 'Text', based on work by Boyer,
-- Moore, Horspool, Sunday, and Lundh.  Adapted from the strict
-- implementation.

module Data.Text.Internal.Lazy.Search
    (
      indices
    ) where

import Data.Bits (unsafeShiftL)
import qualified Data.Text.Array as A
import Data.Int (Int64)
import Data.Word (Word8, Word64)
import qualified Data.Text.Internal as T
import qualified Data.Text as T (concat)
import Data.Text.Internal.Fusion.Types (PairS(..))
import Data.Text.Internal.Lazy (Text(..), foldrChunks, equal)
import Data.Bits ((.|.), (.&.))

-- | /O(n+m)/ Find the offsets of all non-overlapping indices of
-- @needle@ within @haystack@.
--
-- This function is strict in @needle@, and lazy (as far as possible)
-- in the chunks of @haystack@.
--
-- In (unlikely) bad cases, this algorithm's complexity degrades
-- towards /O(n*m)/.
indices :: Text              -- ^ Substring to search for (@needle@)
        -> Text              -- ^ Text to search in (@haystack@)
        -> [Int64]
indices needle haystack
    | nlen <= 0  = []
    | nlen == 1  = indicesOne (nindex 0) 0 haystack
    | otherwise  = advance haystack 0 0
  where
    T.Text narr noff nlen = T.concat (foldrChunks (:) [] needle)

    advance Empty !_ !_ = []
    advance xxs@(Chunk x@(T.Text xarr xoff l) xs) !(g :: Int64) !(i :: Int)
         | i >= l = advance xs g (i - l)
         | lackingHay (i + nlen) x xs  = []
         | c == z && candidateMatch    = g : advance xxs (g + intToInt64 nlen) (i + nlen)
         | otherwise                   = advance xxs (g + intToInt64 delta) (i + delta)
       where
         c = index xxs (i + nlast)
         delta | nextInPattern = nlen + 1
               | c == z        = skip + 1
               | otherwise     = 1
         nextInPattern         = mask .&. swizzle (index xxs (i + nlen)) == 0

         candidateMatch
          | i + nlen <= l = A.equal narr noff xarr (xoff + i) nlen
          | otherwise     = A.equal narr noff xarr (xoff + i) (l - i) &&
            Chunk (T.Text narr (noff + l - i) (nlen - l + i)) Empty `equal` xs

    nlast     = nlen - 1
    nindex i  = A.unsafeIndex narr (noff + i)
    z         = A.unsafeIndex narr (noff + nlen - 1)
    (mask :: Word64) :*: skip = buildTable 0 0 0 (nlen-2)

    swizzle :: Word8 -> Word64
    swizzle w = 1 `unsafeShiftL` (word8ToInt w .&. 0x3f)

    buildTable !g !i !msk !skp
            | i >= nlast = (msk .|. swizzle z) :*: skp
            | otherwise = buildTable (g+1) (i+1) msk' skp'
            where c                = A.unsafeIndex narr (noff+i)
                  msk'             = msk .|. swizzle c
                  skp' | c == z    = nlen - g - 2
                       | otherwise = skp

    -- | Check whether an attempt to index into the haystack at the
    -- given offset would fail.
    lackingHay :: Int -> T.Text -> Text -> Bool
    lackingHay q (T.Text _ _ l) ps = l < q && case ps of
      Empty -> True
      Chunk r rs -> lackingHay (q - l) r rs

-- | Fast index into a partly unpacked 'Text'.  We take into account
-- the possibility that the caller might try to access one element
-- past the end.
index :: Text -> Int -> Word8
index Empty !_ = 0
index (Chunk (T.Text arr off len) xs) !i
    | i < len   = A.unsafeIndex arr (off + i)
    | otherwise = index xs (i - len)

-- | A variant of 'indices' that scans linearly for a single 'Word8'.
indicesOne :: Word8 -> Int64 -> Text -> [Int64]
indicesOne c = chunk
  where
    chunk :: Int64 -> Text -> [Int64]
    chunk !_ Empty = []
    chunk !i (Chunk (T.Text oarr ooff olen) os) = go 0
      where
        go h | h >= olen = chunk (i+intToInt64 olen) os
             | on == c = i + intToInt64 h : go (h+1)
             | otherwise = go (h+1)
             where on = A.unsafeIndex oarr (ooff+h)

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
