{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Maybe.Unpacked.Numeric.Word16
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
import GHC.Exts (Word#)
import GHC.Word (Word16)
import GHC.Word.Compat (pattern W16#)

import GHC.Read (Read (readPrec))
import Text.ParserCombinators.ReadPrec (prec, step)
import Text.Read (Lexeme (Ident), lexP, parens, (+++))

import qualified Prelude as P

data Maybe = Maybe (# (# #) | Word# #)

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
  showsPrec p (Maybe m) = case m of
    (# (# #) | #) -> showString "nothing"
    (# | w #) ->
      showParen (p > 10) $
        showString "just "
          . showsPrec 11 (W16# w)

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

listToMaybe :: [Word16] -> Maybe
listToMaybe [] = nothing
listToMaybe (x : _) = just x

maybeToList :: Maybe -> [Word16]
maybeToList = maybe [] (: [])

catMaybes :: [Maybe] -> [Word16]
catMaybes = mapMaybe id

mapMaybe :: (a -> Maybe) -> [a] -> [Word16]
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
mapMaybeFB :: (Word16 -> r -> r) -> (a -> Maybe) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: Maybe -> Bool
isNothing = maybe True (const False)

isJust :: Maybe -> Bool
isJust = maybe False (const True)

nothing :: Maybe
nothing = Maybe (# (# #) | #)

just :: Word16 -> Maybe
just (W16# w) = Maybe (# | w #)

fromMaybe :: Word16 -> Maybe -> Word16
fromMaybe a (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | w #) -> W16# w

maybe :: a -> (Word16 -> a) -> Maybe -> a
maybe a f (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | w #) -> f (W16# w)

toBaseMaybe :: Maybe -> P.Maybe Word16
toBaseMaybe = maybe P.Nothing P.Just

fromBaseMaybe :: P.Maybe Word16 -> Maybe
fromBaseMaybe = P.maybe nothing just

pattern Nothing :: Maybe
pattern Nothing = Maybe (# (# #) | #)

pattern Just :: Word16 -> Maybe
pattern Just i <- Maybe (# | (W16# -> i) #)
  where
    Just (W16# i) = Maybe (# | i #)

{-# COMPLETE Nothing, Just #-}
