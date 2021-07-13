-- | Miscellaneous testing utilities
--
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

module Tests.Utils
    (
      (=^=)
    , withRedirect
    , withTempFile
    , QEq(..)
    ) where

import Control.Exception (SomeException, bracket, bracket_, evaluate, try)
import Control.Monad (when)
import Data.Int (Int32, Int64)
import Data.Word (Word8)
import GHC.IO.Handle.Internals (withHandle)
import System.Directory (removeFile)
import System.IO (Handle, hClose, hFlush, hIsOpen, hIsWritable, openTempFile)
import Test.QuickCheck (Property, ioProperty, property, counterexample, (===), (.&&.), conjoin)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Encoding.Error (UnicodeException(..))

class (Eq a, Show a) => QEq a where
  infix 4 ====
  (====) :: a -> a -> Property
  default (====) :: a -> a -> Property
  (====) = (===)

instance QEq Bool
instance QEq Ordering
instance QEq Char
instance QEq Int
instance QEq Int32
instance QEq Int64
instance QEq Word8

instance QEq a => QEq (Maybe a) where
  x ==== y = counterexample (show x ++ " /= " ++ show y ++ " because") $
    case (x, y) of
      (Just x', Just y') -> x' ==== y'
      (Just{}, Nothing)  -> counterexample ("Just /= Nothing") (property False)
      (Nothing, Just{})  -> counterexample ("Nothing /= Just") (property False)
      (Nothing, Nothing) -> property True

instance QEq UnicodeException where
  x ==== y = counterexample (show x ++ " /= " ++ show y ++ " because") $
    case (x, y) of
      (DecodeError x' x'', DecodeError y' y'') -> x' ==== y' .&&. x'' ==== y''
      (DecodeError{}, EncodeError{})  -> counterexample ("DecodeError /= Right") (property False)
      (EncodeError{}, DecodeError{})  -> counterexample ("Right /= DecodeError") (property False)
      (EncodeError x' x'', EncodeError y' y'') -> x' ==== y' .&&. x'' ==== y''

instance (QEq a, QEq b) => QEq (Either a b) where
  x ==== y = counterexample (show x ++ " /= " ++ show y ++ " because") $
    case (x, y) of
      (Left x', Left y')   -> x' ==== y'
      (Left{}, Right{})    -> counterexample ("Left /= Right") (property False)
      (Right{}, Left{})    -> counterexample ("Right /= Left") (property False)
      (Right x', Right y') -> x' ==== y'

instance (QEq a, QEq b) => QEq (a, b) where
  x@(x', x'') ==== y@(y', y'') =
    counterexample (show x ++ " /= " ++ show y ++ " because") $
      counterexample ("at the first component") (x' ==== y')
      .&&.
      counterexample ("at the second component") (x'' ==== y'')

instance QEq a => QEq [a] where
  xs ==== ys
    | xs == ys = property True
    | otherwise = counterexample (show xs ++ " /= " ++ show ys ++ " because") $
      counterexample "lengths do not match" (length xs ==== length ys)
      .&&.
      conjoin (zipWith3 (\i x y -> counterexample ("at index " ++ show i) (x ==== y)) [(0::Int)..] xs ys)

instance QEq B.ByteString where
  xs ==== ys
    | xs == ys = property True
    | otherwise = B.unpack xs ==== B.unpack ys .&&. xs === ys

instance QEq T.Text where
  xs ==== ys
    | xs == ys = property True
    | otherwise = T.unpack xs ==== T.unpack ys .&&. xs === ys

instance QEq TL.Text where
  xs ==== ys
    | xs == ys = property True
    | otherwise = TL.unpack xs ==== TL.unpack ys .&&. xs === ys

-- Ensure that two potentially bottom values (in the sense of crashing
-- for some inputs, not looping infinitely) either both crash, or both
-- give comparable results for some input.
(=^=) :: QEq a => (Eq a, Show a) => a -> a -> Property
i =^= j = ioProperty $ do
  x <- try (evaluate i)
  y <- try (evaluate j)
  return $ case (x, y) of
    (Left (_ :: SomeException), Left (_ :: SomeException))
                       -> property True
    (Right a, Right b) -> a ==== b
    e                  -> counterexample ("Divergence: " ++ show e) $ property False
infix 4 =^=
{-# NOINLINE (=^=) #-}

withTempFile :: (FilePath -> Handle -> IO a) -> IO a
withTempFile = bracket (openTempFile "." "crashy.txt") cleanupTemp . uncurry
  where
    cleanupTemp (path,h) = do
      open <- hIsOpen h
      when open (hClose h)
      removeFile path

withRedirect :: Handle -> Handle -> IO a -> IO a
withRedirect tmp h = bracket_ swap swap
  where
    whenM p a = p >>= (`when` a)
    swap = do
      whenM (hIsOpen tmp) $ whenM (hIsWritable tmp) $ hFlush tmp
      whenM (hIsOpen h) $ whenM (hIsWritable h) $ hFlush h
      withHandle "spam" tmp $ \tmph -> do
        hh <- withHandle "spam" h $ \hh ->
          return (tmph,hh)
        return (hh,())
