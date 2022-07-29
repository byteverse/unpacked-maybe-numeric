{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Numeric.Int8
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

import GHC.Exts 
import GHC.Int (Int8)
import GHC.Int.Compat (pattern I8#)

import GHC.Read (Read(readPrec))
import Text.Read (parens, Lexeme(Ident), lexP, (+++))
import Text.ParserCombinators.ReadPrec (prec, step)

import qualified Prelude as P

data Maybe = M Int#

instance Eq Maybe where
  ma == mb =
    maybe (isNothing mb)
          (\a -> maybe False (\b -> a == b) mb) ma
    
instance Ord Maybe where
  compare ma mb = maybe LT (\a -> maybe GT (compare a) mb) ma  

instance Show Maybe where
  showsPrec p m =
    maybe (showString "nothing")
      (\i -> showParen (p > 10)
        $ showString "just "
        . showsPrec 11 i
      ) m

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

listToMaybe :: [Int8] -> Maybe
{-# INLINE listToMaybe #-}
listToMaybe [] = nothing
listToMaybe (x:_) = just x

maybeToList :: Maybe -> [Int8]
maybeToList m = maybe [] (: []) m

catMaybes :: [Maybe] -> [Int8]
catMaybes ms = mapMaybe id ms

mapMaybe :: (a -> Maybe) -> [a] -> [Int8]
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
mapMaybeFB :: (Int8 -> r -> r) -> (a -> Maybe) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: Maybe -> Bool
{-# INLINE isNothing #-}
isNothing m = maybe True (const False) m

isJust :: Maybe -> Bool
{-# INLINE isJust #-}
isJust m = maybe False (const True) m

nothing :: Maybe
{-# INLINE nothing #-}
nothing = M 128#

just :: Int8 -> Maybe
{-# INLINE just #-}
just (I8# i) = M i

fromMaybe :: Int8 -> Maybe -> Int8
{-# INLINE fromMaybe #-}
fromMaybe a m = maybe a id m

maybe :: a -> (Int8 -> a) -> Maybe -> a
{-# INLINE maybe #-}
maybe a f (M m) = case m ># 127# of
  1# -> a
  _  -> case m <# -128# of
    1# -> a
    _  -> f (I8# m)

toBaseMaybe :: Maybe -> P.Maybe Int8
{-# INLINE toBaseMaybe #-}
toBaseMaybe m = maybe P.Nothing P.Just m

fromBaseMaybe :: P.Maybe Int8 -> Maybe
{-# INLINE fromBaseMaybe #-}
fromBaseMaybe m = P.maybe nothing just m

