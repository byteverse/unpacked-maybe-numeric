{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Numeric.Word
  ( Maybe(..)
  , just
  , nothing
  , maybe
  , fromMaybe
  , toBaseMaybe
  , fromBaseMaybe
  ) where

import Prelude hiding (Maybe,maybe)

import GHC.Exts (Word#,(*#),(+#),and#,indexWordArray#,readWordArray#)
import GHC.Word (Word(W#))
import Data.Primitive.Types (Prim(..))

import qualified Prelude as P

data Maybe = Maybe (# (# #) | Word# #)

instance Eq Maybe where
  a == b = toBaseMaybe a == toBaseMaybe b
  a /= b = toBaseMaybe a /= toBaseMaybe b

instance Ord Maybe where
  compare a b = compare (toBaseMaybe a) (toBaseMaybe b)
  a > b = toBaseMaybe a > toBaseMaybe b
  a < b = toBaseMaybe a < toBaseMaybe b
  a >= b = toBaseMaybe a >= toBaseMaybe b
  a <= b = toBaseMaybe a <= toBaseMaybe b
  min a b = fromBaseMaybe (min (toBaseMaybe a) (toBaseMaybe b))
  max a b = fromBaseMaybe (max (toBaseMaybe a) (toBaseMaybe b))

instance Show Maybe where
  showsPrec p (Maybe m) = case m of
    (# (# #) | #) -> showString "nothing"
    (# | w #) -> showParen (p > 10)
      $ showString "just "
      . showsPrec 11 (W# w)

instance Prim Maybe where
  sizeOf# _ = sizeOf# (undefined :: Word) *# 2#
  alignment# _ = alignment# (undefined :: Word)
  indexByteArray# arr ix = Maybe
    (case indexWordArray# arr (2# *# ix) of
      0## -> (# (# #) | #)
      _ -> (# | (indexWordArray# arr ((2# *# ix) +# 1#)) #)
    ) 
  readByteArray# arr ix s0 = case readWordArray# arr (2# *# ix) s0 of
    (# s1, x #) -> case x of
      0## -> (# s1, nothing #)
      _ -> case readWordArray# arr ((2# *# ix) +# 1#) s1 of
        (# s2, y #) -> (# s2, Maybe (# | y #) #)

nothing :: Maybe
nothing = Maybe (# (# #) | #)

just :: Word -> Maybe
just (W# w) = Maybe (# | w #)

fromMaybe :: Word -> Maybe -> Word
fromMaybe a (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | w #) -> W# w

maybe :: a -> (Word -> a) -> Maybe -> a
maybe a f (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | w #) -> f (W# w)

toBaseMaybe :: Maybe -> P.Maybe Word
toBaseMaybe = maybe P.Nothing P.Just

fromBaseMaybe :: P.Maybe Word -> Maybe
fromBaseMaybe = P.maybe nothing just

