{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Numeric.Double
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
import GHC.Exts (Double (D#), Double#)

import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadPrec (prec, step)
import Text.Read (Lexeme (Ident), lexP, parens, (+++))
import qualified Prelude as P

data Maybe = Maybe (# (# #) | Double# #)

instance Eq Maybe where
  ma == mb =
    maybe
      (isNothing mb)
      (\a -> maybe False (\b -> a == b) mb)
      ma

instance Ord Maybe where
  compare (Maybe ma) (Maybe mb) = case ma of
    (# (# #) | #) -> case mb of
      (# (# #) | #) -> EQ
      (# | _ #) -> LT
    (# | a #) -> case mb of
      (# (# #) | #) -> GT
      (# | b #) -> compare (D# a) (D# b)

instance Show Maybe where
  showsPrec p (Maybe m) = case m of
    (# (# #) | #) -> showString "nothing"
    (# | d #) ->
      showParen (p > 10) $
        showString "just "
          . showsPrec 11 (D# d)

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

listToMaybe :: [Double] -> Maybe
listToMaybe [] = nothing
listToMaybe (x : _) = just x

maybeToList :: Maybe -> [Double]
maybeToList = maybe [] (: [])

catMaybes :: [Maybe] -> [Double]
catMaybes = mapMaybe id

mapMaybe :: (a -> Maybe) -> [a] -> [Double]
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
mapMaybeFB :: (Double -> r -> r) -> (a -> Maybe) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: Maybe -> Bool
isNothing = maybe True (const False)

isJust :: Maybe -> Bool
isJust = maybe False (const True)

nothing :: Maybe
nothing = Maybe (# (# #) | #)

just :: Double -> Maybe
just (D# d) = Maybe (# | d #)

fromMaybe :: Double -> Maybe -> Double
fromMaybe a (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | d #) -> D# d

maybe :: a -> (Double -> a) -> Maybe -> a
maybe a f (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | d #) -> f (D# d)

toBaseMaybe :: Maybe -> P.Maybe Double
toBaseMaybe = maybe P.Nothing P.Just

fromBaseMaybe :: P.Maybe Double -> Maybe
fromBaseMaybe = P.maybe nothing just
