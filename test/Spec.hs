import Data.List (genericLength, genericIndex)

import Test.Hspec (hspec, describe, it, shouldBe, SpecWith, pending)

import Problems (problems, ioProblems)

safeIndex :: (Integral a) => [b] -> a -> Maybe b
safeIndex xs i
  | i >= 0 && i < genericLength xs = Just $ xs `genericIndex` i
  | otherwise = Nothing

expect :: Int -> Integer -> SpecWith ()
expect question answer = it (show question) $
                            problems `safeIndex` (question - 1) `shouldBe`
                            Just answer

main :: IO ()
main = do
  p22data <- readFile "./data/names.txt"

  hspec $ do
    describe "Project Euler problems" $ do
      expect 1 233168
      expect 2 4613732
      expect 3 6857
      expect 4 906609
      expect 5 232792560
      expect 6 25164150
      expect 7 104743
      expect 8 23514624000
      expect 9 31875000
      expect 10 142913828922

      expect 11 70600674
      expect 12 76576500
      expect 13 5537376230
      expect 14 837799
      expect 15 137846528820
      expect 16 1366
      expect 17 21124
      expect 18 1074
      expect 19 171
      expect 20 648

      expect 21 31626
      it "22" $ ioProblems 22 p22data `shouldBe` 871198282
      expect 23 4179871
      expect 24 2783915460
      expect 25 4782
      expect 26 983
      expect 27 (-59231)
      expect 28 669171001
      expect 29 9183
      expect 30 443839

      expect 31 73682
      expect 32 45228
      expect 33 100
      expect 34 40730
      expect 35 55
      expect 36 872187
      expect 37 748317
      expect 38 932718654
      expect 39 840

    describe "Utility functions" $
      it "NO TESTS IMPLEMENTED" pending
