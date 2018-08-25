{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Maybe.Unpacked.Numeric.Word
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
import GHC.Exts (Word#,(*#),(+#),and#,indexWordArray#,readWordArray#,word2Int#, (==#))
import GHC.Word (Word(W#))
import Data.Primitive.Types (Prim(..))

import GHC.Read (Read(readPrec), expectP)
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
      . showsPrec 11 (W# w)

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

instance Prim Maybe where
  sizeOf# _ = sizeOf# (undefined :: Word) *# 2#
  alignment# _ = alignment# (undefined :: Word)
  indexByteArray# arr ix = Maybe
    (case indexWordArray# arr (2# *# ix) of
      0## -> (# (# #) | #)
      _ -> (# | (indexWordArray# arr ((2# *# ix) +# 1#)) #)
    ) 
  readByteArray# arr ix s0 = case readWordArray# arr (2# *# ix) s0 of
    (# s1, x #) -> case x of
      0## -> (# s1, nothing #)
      _ -> case readWordArray# arr ((2# *# ix) +# 1#) s1 of
        (# s2, y #) -> (# s2, Maybe (# | y #) #)

listToMaybe :: [Word] -> Maybe
listToMaybe [] = nothing
listToMaybe (x:_) = just x

maybeToList :: Maybe -> [Word]
maybeToList = maybe [] (: [])

catMaybes :: [Maybe] -> [Word]
catMaybes = mapMaybe P.id

mapMaybe :: (a -> Maybe) -> [a] -> [Word]
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
mapMaybeFB :: (Word -> r -> r) -> (a -> Maybe) -> a -> r -> r
mapMaybeFB cons f x next = maybe next (flip cons next) (f x)

isNothing :: Maybe -> Bool
isNothing = maybe True (const False)

isJust :: Maybe -> Bool
isJust = maybe False (const True)

nothing :: Maybe
nothing = Maybe (# (# #) | #)

just :: Word -> Maybe
just (W# w) = Maybe (# | w #)

fromMaybe :: Word -> Maybe -> Word
fromMaybe a (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | w #) -> W# w

maybe :: a -> (Word -> a) -> Maybe -> a
maybe a f (Maybe m) = case m of
  (# (# #) | #) -> a
  (# | w #) -> f (W# w)

toBaseMaybe :: Maybe -> P.Maybe Word
toBaseMaybe = maybe P.Nothing P.Just

fromBaseMaybe :: P.Maybe Word -> Maybe
fromBaseMaybe = P.maybe nothing just

