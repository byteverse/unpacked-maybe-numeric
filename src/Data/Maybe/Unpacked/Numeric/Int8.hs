{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Numeric.Int8
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
import GHC.Exts (Int#,(*#),(+#),and#,(==#))
import GHC.Int (Int8(I8#))
import Data.Primitive.Types (Prim(..))

import GHC.Read (Read(readPrec), expectP)
import Text.Read (parens, Lexeme(Ident), lexP, (+++))
import Text.ParserCombinators.ReadPrec (prec, step)

import qualified Prelude as P

data Maybe = Maybe (# (# #) | Int# #)

instance Eq Maybe where
  ma == mb =
    maybe (isNothing mb)
          (\a -> maybe False (\b -> a == b) mb) ma
    
instance Ord Maybe where
  compare ma mb = maybe LT (\a -> maybe GT (compare a) mb) ma  

instance Show Maybe where
  showsPrec p (Maybe m) = case m of
    (# (# #) | #) -> showString "nothing"
    (# | i #) -> showParen (p > 10)
      $ showString "just "
      . showsPrec 11 (I8# i)

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

listToMaybe :: [Int8] -> Maybe
listToMaybe [] = nothing
listToMaybe (x:_) = just x

maybeToList :: Maybe -> [Int8]
maybeToList = maybe [] (: [])

catMaybes :: [Maybe] -> [Int8]
catMaybes = mapMaybe id

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
isNothing = maybe True (const False)

isJust :: Maybe -> Bool
isJust = maybe False (const True)

nothing :: Maybe
nothing = Maybe (# (# #) | #)

just :: Int8 -> Maybe
just (I8# i) = Maybe (# | i #)

fromMaybe :: Int8 -> Maybe -> Int8
fromMaybe a (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | i #) -> I8# i

maybe :: a -> (Int8 -> a) -> Maybe -> a
maybe a f (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | i #) -> f (I8# i)

toBaseMaybe :: Maybe -> P.Maybe Int8
toBaseMaybe = maybe P.Nothing P.Just

fromBaseMaybe :: P.Maybe Int8 -> Maybe
fromBaseMaybe = P.maybe nothing just

