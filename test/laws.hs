{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeApplications #-}

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck.Classes
import Data.Proxy (Proxy (Proxy))
import GHC.Exts

import qualified Data.Maybe.Unpacked.Numeric.Complex.Float  as MComplexFloat
import qualified Data.Maybe.Unpacked.Numeric.Complex.Double as MComplexDouble

import qualified Data.Maybe.Unpacked.Numeric.Float  as MFloat
import qualified Data.Maybe.Unpacked.Numeric.Double as MDouble

import qualified Data.Maybe.Unpacked.Numeric.Int    as MInt
import qualified Data.Maybe.Unpacked.Numeric.Int8   as MInt8
import qualified Data.Maybe.Unpacked.Numeric.Int16  as MInt16
import qualified Data.Maybe.Unpacked.Numeric.Int32  as MInt32
import qualified Data.Maybe.Unpacked.Numeric.Int64  as MInt64

import qualified Data.Maybe.Unpacked.Numeric.Word   as MWord
import qualified Data.Maybe.Unpacked.Numeric.Word8  as MWord8
import qualified Data.Maybe.Unpacked.Numeric.Word16 as MWord16
import qualified Data.Maybe.Unpacked.Numeric.Word32 as MWord32
import qualified Data.Maybe.Unpacked.Numeric.Word64 as MWord64

main :: IO ()
main = lawsCheckMany allPropsApplied

allLaws ::
  ( Arbitrary a
  , Eq a
  , Ord a
  , Show a
  , Read a
  ) => Proxy a -> [Laws]
allLaws p = map ($ p)
  [ eqLaws, ordLaws, showReadLaws
  ]


allPropsApplied :: [(String, [Laws])]
allPropsApplied =
  [ ("Maybe (Complex Float)", map ($ (Proxy :: Proxy MComplexFloat.Maybe)) [eqLaws, showReadLaws])
  , ("Maybe (Complex Double)", map ($ (Proxy :: Proxy MComplexDouble.Maybe)) [eqLaws, showReadLaws])

  , ("Maybe Float", allLaws (Proxy :: Proxy MFloat.Maybe))
  , ("Maybe Double", allLaws (Proxy :: Proxy MDouble.Maybe))
  , ("Maybe Int8", allLaws (Proxy :: Proxy MInt8.Maybe))
  , ("Maybe Int16", allLaws (Proxy :: Proxy MInt16.Maybe))
  , ("Maybe Int32", allLaws (Proxy :: Proxy MInt32.Maybe))
  , ("Maybe Int64", allLaws (Proxy :: Proxy MInt64.Maybe))
  , ("Maybe Int", allLaws (Proxy :: Proxy MInt.Maybe))
  , ("Maybe Word8", allLaws (Proxy :: Proxy MWord8.Maybe))
  , ("Maybe Word16", allLaws (Proxy :: Proxy MWord16.Maybe))
  , ("Maybe Word32", allLaws (Proxy :: Proxy MWord32.Maybe))
  , ("Maybe Word64", allLaws (Proxy :: Proxy MWord64.Maybe))
  , ("Maybe Word", allLaws (Proxy :: Proxy MWord.Maybe))
  ]

unFloat :: Float -> Float#
unFloat (F# f#) = f#

unDouble :: Double -> Double#
unDouble (D# d#) = d#

instance Arbitrary MComplexFloat.Complex where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    pure (MComplexFloat.Complex (unFloat x) (unFloat y))

instance Arbitrary MComplexDouble.Complex where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    pure (MComplexDouble.Complex (unDouble x) (unDouble y))

instance Arbitrary MComplexFloat.Maybe where
  arbitrary = frequency [(1,pure MComplexFloat.nothing), (1, MComplexFloat.just <$> arbitrary)]

instance Arbitrary MComplexDouble.Maybe where
  arbitrary = frequency [(1,pure MComplexDouble.nothing), (1, MComplexDouble.just <$> arbitrary)]

instance Arbitrary MFloat.Maybe where
  arbitrary = frequency [(1,pure MFloat.nothing), (1, MFloat.just <$> arbitrary)]

instance Arbitrary MDouble.Maybe where
  arbitrary = frequency [(1,pure MDouble.nothing), (1, MDouble.just <$> arbitrary)]

instance Arbitrary MWord8.Maybe where
  arbitrary = frequency [(1, pure MWord8.nothing), (1, MWord8.just <$> arbitrary)]

instance Arbitrary MWord16.Maybe where
  arbitrary = frequency [(1, pure MWord16.nothing), (1, MWord16.just <$> arbitrary)]

instance Arbitrary MWord32.Maybe where
  arbitrary = frequency [(1, pure MWord32.nothing), (1, MWord32.just <$> arbitrary)]

instance Arbitrary MWord64.Maybe where
  arbitrary = frequency [(1, pure MWord64.nothing), (1, MWord64.just <$> arbitrary)]

instance Arbitrary MWord.Maybe where
  arbitrary = frequency [(1, pure MWord.nothing), (1, MWord.just <$> arbitrary)]

instance Arbitrary MInt8.Maybe where
  arbitrary = frequency [(1, pure MInt8.nothing), (1, MInt8.just <$> arbitrary)]

instance Arbitrary MInt16.Maybe where
  arbitrary = frequency [(1, pure MInt16.nothing), (1, MInt16.just <$> arbitrary)]

instance Arbitrary MInt32.Maybe where
  arbitrary = frequency [(1, pure MInt32.nothing), (1, MInt32.just <$> arbitrary)]

instance Arbitrary MInt64.Maybe where
  arbitrary = frequency [(1, pure MInt64.nothing), (1, MInt64.just <$> arbitrary)]

instance Arbitrary MInt.Maybe where
  arbitrary = frequency [(1, pure MInt.nothing), (1, MInt.just <$> arbitrary)]