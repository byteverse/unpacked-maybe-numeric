import Control.Monad (when)

import qualified Data.Maybe.Unpacked.Numeric.Word as Word

main :: IO ()
main = do
  putStrLn "A"
  when (showsPrec 12 Word.nothing "" /= "nothing") $ do
    fail "Failed test A"
  putStrLn "B"
  when (showsPrec 6 Word.nothing "" /= "nothing") $ do
    fail "Failed test B"
  putStrLn "C"
  when (showsPrec 11 Word.nothing "" /= "nothing") $ do
    fail "Failed test C"
