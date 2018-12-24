{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Numeric.Word16
  ( Maybe(..)
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

import Prelude hiding (Maybe,maybe)

import GHC.Base (build)
import GHC.Exts (Word#)
import GHC.Word (Word16(W16#))

import GHC.Read (Read(readPrec))
import Text.Read (parens, Lexeme(Ident), lexP, (+++))
import Text.ParserCombinators.ReadPrec (prec, step)

import qualified Prelude as P

data Maybe = Maybe (# (# #) | Word# #)

instance Eq Maybe where
  ma == mb =
    maybe (isNothing mb)
          (\a -> maybe False (\b -> a == b) mb) ma
    
instance Ord Maybe where
  compare ma mb = maybe LT (\a -> maybe GT (compare a) mb) ma  

instance Show Maybe where
  showsPrec p (Maybe m) = case m of
    (# (# #) | #) -> showString "nothing"
    (# | w #) -> showParen (p > 10)
      $ showString "just "
      . showsPrec 11 (W16# w)

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

listToMaybe :: [Word16] -> Maybe
listToMaybe [] = nothing
listToMaybe (x:_) = just x

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
"mapMaybe"     [~1] forall f xs. mapMaybe f xs
                    = build (\c n -> foldr (mapMaybeFB c f) n xs)
"mapMaybeList" [1]  forall f. foldr (mapMaybeFB (:) f) [] = mapMaybe f
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

