{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Numeric.Word128
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

import Data.WideWord (Word128 (..))
import GHC.Base (build)
import GHC.Exts (Word64#)
import GHC.Word (Word64 (W64#))

import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadPrec (prec, step)
import Text.Read (Lexeme (Ident), lexP, parens, (+++))

import qualified Prelude as P

data Maybe = Maybe (# (# #) | (# Word64#, Word64# #) #)

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
    (# | (# wa, wb #) #) ->
      showParen (p > 10) $
        showString "just "
          . showsPrec 11 (Word128 (W64# wa) (W64# wb))

instance Read Maybe where
  readPrec = parens $ nothingP +++ justP
   where
    nothingP = prec 10 $ do
      Ident "nothing" <- lexP
      return nothing
    justP = prec 10 $ do
      Ident "just" <- lexP
      a <- step readPrec
      return (just a)

listToMaybe :: [Word128] -> Maybe
listToMaybe [] = nothing
listToMaybe (x : _) = just x

maybeToList :: Maybe -> [Word128]
maybeToList = maybe [] (: [])

catMaybes :: [Maybe] -> [Word128]
catMaybes = mapMaybe id

mapMaybe :: (a -> Maybe) -> [a] -> [Word128]
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
mapMaybeFB :: (Word128 -> r -> r) -> (a -> Maybe) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: Maybe -> Bool
isNothing = maybe True (const False)

isJust :: Maybe -> Bool
isJust = maybe False (const True)

nothing :: Maybe
nothing = Maybe (# (# #) | #)

just :: Word128 -> Maybe
just (Word128 (W64# wa) (W64# wb)) = Maybe (# | (# wa, wb #) #)

fromMaybe :: Word128 -> Maybe -> Word128
fromMaybe a (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | (# wa, wb #) #) -> Word128 (W64# wa) (W64# wb)

maybe :: a -> (Word128 -> a) -> Maybe -> a
maybe a f (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | (# wa, wb #) #) -> f (Word128 (W64# wa) (W64# wb))

toBaseMaybe :: Maybe -> P.Maybe Word128
toBaseMaybe = maybe P.Nothing P.Just

fromBaseMaybe :: P.Maybe Word128 -> Maybe
fromBaseMaybe = P.maybe nothing just
