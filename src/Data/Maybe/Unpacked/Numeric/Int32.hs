{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Numeric.Int32
  ( Maybe(..)
  , just
  , nothing
  , maybe
  , fromMaybe
  , toBaseMaybe
  , fromBaseMaybe
  ) where

import Prelude hiding (Maybe,maybe)

import GHC.Base (build)
import GHC.Exts 
import GHC.Int (Int32(I32#))
import Data.Primitive.Types (Prim(..))

import GHC.Read (Read(readPrec), expectP)
import Text.Read (parens, Lexeme(Ident), lexP, (+++))
import Text.ParserCombinators.ReadPrec (prec, step)

import qualified Prelude as P

data Maybe = M Int#

instance Eq Maybe where
  ma == mb =
    maybe (isNothing mb)
          (\a -> maybe False (\b -> a == b) mb) ma
  {-# INLINE (==) #-}

instance Ord Maybe where
  compare ma mb = maybe LT (\a -> maybe GT (compare a) mb) ma  
  {-# INLINE compare #-}

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
      nothingP = prec 10 $ do
        Ident "nothing" <- lexP
        return nothing
      justP = prec 10 $ do
        Ident "just" <- lexP
        a <- step readPrec
        return (just a)

listToMaybe :: [Int32] -> Maybe
{-# INLINE listToMaybe #-}
listToMaybe [] = nothing
listToMaybe (x:_) = just x

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
"mapMaybe"     [~1] forall f xs. mapMaybe f xs
                    = build (\c n -> foldr (mapMaybeFB c f) n xs)
"mapMaybeList" [1]  forall f. foldr (mapMaybeFB (:) f) [] = mapMaybe f
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
  _  -> case m <# -2147483648# of
    1# -> a
    0# -> f (I32# m)

toBaseMaybe :: Maybe -> P.Maybe Int32
{-# INLINE toBaseMaybe #-}
toBaseMaybe m = maybe P.Nothing P.Just m

fromBaseMaybe :: P.Maybe Int32 -> Maybe
{-# INLINE fromBaseMaybe #-}
fromBaseMaybe m = P.maybe nothing just m

