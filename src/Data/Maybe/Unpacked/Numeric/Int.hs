{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Numeric.Int
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
  ) where

import Prelude hiding (Maybe, maybe)

import GHC.Base (build)
import GHC.Exts (Int#)
import GHC.Int (Int (I#))

import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadPrec (prec, step)
import Text.Read (Lexeme (Ident), lexP, parens, (+++))

import qualified Prelude as P

data Maybe = Maybe (# (# #) | Int# #)

instance Eq Maybe where
  ma == mb =
    maybe
      (isNothing mb)
      (\a -> maybe False (\b -> a == b) mb)
      ma

instance Ord Maybe where
  compare ma mb = maybe LT (\a -> maybe GT (compare a) mb) ma

instance Show Maybe where
  showsPrec p (Maybe m) = case m of
    (# (# #) | #) -> showString "nothing"
    (# | i #) ->
      showParen (p > 10) $
        showString "just "
          . showsPrec 11 (I# i)

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

listToMaybe :: [Int] -> Maybe
listToMaybe [] = nothing
listToMaybe (x : _) = just x

maybeToList :: Maybe -> [Int]
maybeToList = maybe [] (: [])

catMaybes :: [Maybe] -> [Int]
catMaybes = mapMaybe id

mapMaybe :: (a -> Maybe) -> [a] -> [Int]
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
mapMaybeFB :: (Int -> r -> r) -> (a -> Maybe) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: Maybe -> Bool
isNothing = maybe True (const False)

isJust :: Maybe -> Bool
isJust = maybe False (const True)

nothing :: Maybe
nothing = Maybe (# (# #) | #)

just :: Int -> Maybe
just (I# i) = Maybe (# | i #)

fromMaybe :: Int -> Maybe -> Int
fromMaybe a (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | i #) -> I# i

maybe :: a -> (Int -> a) -> Maybe -> a
maybe a f (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | i #) -> f (I# i)

toBaseMaybe :: Maybe -> P.Maybe Int
toBaseMaybe = maybe P.Nothing P.Just

fromBaseMaybe :: P.Maybe Int -> Maybe
fromBaseMaybe = P.maybe nothing just
