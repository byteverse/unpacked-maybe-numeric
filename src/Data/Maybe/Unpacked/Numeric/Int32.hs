{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Maybe.Unpacked.Numeric.Int32
  ( Maybe (..)
  , just
  , nothing
  , maybe
  , isJust
  , isNothing
  , fromMaybe
  , listToMaybe
  , maybeToList
  , catMaybes
  , mapMaybe
  , toBaseMaybe
  , fromBaseMaybe
    -- * Patterns
  , pattern Nothing
  , pattern Just
  ) where

import Prelude hiding (Just, Maybe, Nothing, maybe)

import GHC.Exts
import GHC.Int (Int32)
import GHC.Int.Compat (pattern I32#)

import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadPrec (prec, step)
import Text.Read (Lexeme (Ident), lexP, parens, (+++))

import qualified Prelude as P

data Maybe = M Int#

instance Eq Maybe where
  ma == mb =
    maybe
      (isNothing mb)
      (\a -> maybe False (\b -> a == b) mb)
      ma
  {-# INLINE (==) #-}

instance Ord Maybe where
  compare ma mb = case ma of
    Just a -> case mb of
      Just b -> compare a b
      _ -> GT
    _ -> case mb of
      Just{} -> LT
      _ -> EQ

instance Show Maybe where
  showsPrec p m =
    maybe
      (showString "nothing")
      ( \i ->
          showParen (p > 10) $
            showString "just "
              . showsPrec 11 i
      )
      m

instance Read Maybe where
  readPrec = parens $ nothingP +++ justP
   where
    nothingP = do
      Ident "nothing" <- lexP
      return nothing
    justP = prec 10 $ do
      Ident "just" <- lexP
      a <- step readPrec
      return (just a)

listToMaybe :: [Int32] -> Maybe
{-# INLINE listToMaybe #-}
listToMaybe [] = nothing
listToMaybe (x : _) = just x

maybeToList :: Maybe -> [Int32]
maybeToList m = maybe [] (: []) m

catMaybes :: [Maybe] -> [Int32]
catMaybes ms = mapMaybe id ms

mapMaybe :: (a -> Maybe) -> [a] -> [Int32]
mapMaybe _ [] = []
mapMaybe f (a : as) =
  let ws = mapMaybe f as
   in maybe ws (: ws) (f a)
{-# NOINLINE [1] mapMaybe #-}

{-# RULES
"mapMaybe" [~1] forall f xs.
  mapMaybe f xs =
    build (\c n -> foldr (mapMaybeFB c f) n xs)
"mapMaybeList" [1] forall f. foldr (mapMaybeFB (:) f) [] = mapMaybe f
  #-}

{-# NOINLINE [0] mapMaybeFB #-}
mapMaybeFB :: (Int32 -> r -> r) -> (a -> Maybe) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: Maybe -> Bool
{-# INLINE isNothing #-}
isNothing m = maybe True (const False) m

isJust :: Maybe -> Bool
{-# INLINE isJust #-}
isJust m = maybe False (const True) m

nothing :: Maybe
{-# INLINE nothing #-}
nothing = M 2147483648#

just :: Int32 -> Maybe
{-# INLINE just #-}
just (I32# i) = M i

fromMaybe :: Int32 -> Maybe -> Int32
{-# INLINE fromMaybe #-}
fromMaybe a m = maybe a id m

maybe :: a -> (Int32 -> a) -> Maybe -> a
{-# INLINE maybe #-}
maybe a f (M m) = case m ># 2147483647# of
  1# -> a
  _ -> case m <# -2147483648# of
    1# -> a
    _ -> f (I32# m)

toBaseMaybe :: Maybe -> P.Maybe Int32
{-# INLINE toBaseMaybe #-}
toBaseMaybe m = maybe P.Nothing P.Just m

fromBaseMaybe :: P.Maybe Int32 -> Maybe
{-# INLINE fromBaseMaybe #-}
fromBaseMaybe m = P.maybe nothing just m

pattern Nothing :: Maybe
pattern Nothing = M 2147483648#

pattern Just :: Int32 -> Maybe
pattern Just i <- (maybeInt32ToInt32 -> (# | i #))
  where
    Just (I32# i) = M i

maybeInt32ToInt32 :: Maybe -> (# (# #) | Int32 #)
{-# inline maybeInt32ToInt32 #-}
maybeInt32ToInt32 (M i) = case i of
  2147483648# -> (# (# #) | #)
  _ -> (# | I32# i #)
