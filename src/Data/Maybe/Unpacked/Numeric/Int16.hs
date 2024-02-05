{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Maybe.Unpacked.Numeric.Int16
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

import GHC.Base (build)
import GHC.Exts (Int#, (<#), (>#))
import GHC.Int (Int16)
import GHC.Int.Compat (pattern I16#)

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

listToMaybe :: [Int16] -> Maybe
{-# INLINE listToMaybe #-}
listToMaybe [] = nothing
listToMaybe (x : _) = just x

maybeToList :: Maybe -> [Int16]
maybeToList m = maybe [] (: []) m

catMaybes :: [Maybe] -> [Int16]
catMaybes ms = mapMaybe id ms

mapMaybe :: (a -> Maybe) -> [a] -> [Int16]
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
mapMaybeFB :: (Int16 -> r -> r) -> (a -> Maybe) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: Maybe -> Bool
{-# INLINE isNothing #-}
isNothing m = maybe True (const False) m

isJust :: Maybe -> Bool
{-# INLINE isJust #-}
isJust m = maybe False (const True) m

nothing :: Maybe
{-# INLINE nothing #-}
nothing = M 32768#

just :: Int16 -> Maybe
{-# INLINE just #-}
just (I16# i) = M i

fromMaybe :: Int16 -> Maybe -> Int16
{-# INLINE fromMaybe #-}
fromMaybe a m = maybe a id m

maybe :: a -> (Int16 -> a) -> Maybe -> a
{-# INLINE maybe #-}
maybe a f (M m) = case m ># 32767# of
  1# -> a
  _ -> case m <# -32768# of
    1# -> a
    _ -> f (I16# m)

toBaseMaybe :: Maybe -> P.Maybe Int16
{-# INLINE toBaseMaybe #-}
toBaseMaybe m = maybe P.Nothing P.Just m

fromBaseMaybe :: P.Maybe Int16 -> Maybe
{-# INLINE fromBaseMaybe #-}
fromBaseMaybe m = P.maybe nothing just m

pattern Nothing :: Maybe
pattern Nothing = M 32768#

pattern Just :: Int16 -> Maybe
pattern Just i <- (maybeInt16ToInt16 -> (# | i #))
  where
    Just (I16# i) = M i

maybeInt16ToInt16 :: Maybe -> (# (# #) | Int16 #)
{-# inline maybeInt16ToInt16 #-}
maybeInt16ToInt16 (M i) = case i of
  32768# -> (# (# #) | #)
  _ -> (# | I16# i #)
