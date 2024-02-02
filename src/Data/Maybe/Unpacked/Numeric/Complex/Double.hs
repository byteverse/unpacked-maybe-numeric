{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Numeric.Complex.Double
  ( Complex (..)
  , toBaseComplex
  , fromBaseComplex
  , Maybe (..)
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

import qualified Data.Complex as C
import GHC.Base (build)
import GHC.Exts (Double (D#), Double#, (==##))

import GHC.Read (Read (readPrec), expectP)
import Text.ParserCombinators.ReadPrec (prec, step)
import Text.Read (Lexeme (Ident), lexP, parens, (+++))
import qualified Prelude as P

data Complex = Complex Double# Double#

toBaseComplex :: Complex -> C.Complex Double
toBaseComplex (Complex d1# d2#) = (D# d1#) C.:+ (D# d2#)

fromBaseComplex :: C.Complex Double -> Complex
fromBaseComplex ((D# d1#) C.:+ (D# d2#)) = Complex d1# d2#

instance Eq Complex where
  Complex a b == Complex c d =
    case a ==## c of
      1# -> case b ==## d of
        1# -> True
        _ -> False
      _ -> False

instance Show Complex where
  showsPrec p (Complex a b) =
    showParen (p >= 11) $
      showString "Complex "
        . showsPrec 11 (D# a)
        . showString " "
        . showsPrec 11 (D# b)

instance Read Complex where
  readPrec = parens $ prec 10 $ do
    expectP (Ident "Complex")
    (D# a) <- step readPrec
    (D# b) <- step readPrec
    return (Complex a b)

data Maybe = Maybe (# (# #) | Complex #)

instance Eq Maybe where
  ma == mb =
    maybe
      (isNothing mb)
      (\a -> maybe False (\b -> a == b) mb)
      ma

instance Show Maybe where
  showsPrec p (Maybe m) = case m of
    (# (# #) | #) -> showString "nothing"
    (# | c #) ->
      showParen (p > 10) $
        showString "just "
          . showsPrec 11 c

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

listToMaybe :: [Complex] -> Maybe
listToMaybe [] = nothing
listToMaybe (x : _) = just x

maybeToList :: Maybe -> [Complex]
maybeToList = maybe [] (: [])

catMaybes :: [Maybe] -> [Complex]
catMaybes = mapMaybe id

mapMaybe :: (a -> Maybe) -> [a] -> [Complex]
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
mapMaybeFB :: (Complex -> r -> r) -> (a -> Maybe) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: Maybe -> Bool
isNothing = maybe True (const False)

isJust :: Maybe -> Bool
isJust = maybe False (const True)

nothing :: Maybe
nothing = Maybe (# (# #) | #)

just :: Complex -> Maybe
just c = Maybe (# | c #)

fromMaybe :: Complex -> Maybe -> Complex
fromMaybe a (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | c #) -> c

maybe :: a -> (Complex -> a) -> Maybe -> a
maybe a f (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | c #) -> f c

toBaseMaybe :: Maybe -> P.Maybe Complex
toBaseMaybe = maybe P.Nothing P.Just

fromBaseMaybe :: P.Maybe Complex -> Maybe
fromBaseMaybe = P.maybe nothing just
