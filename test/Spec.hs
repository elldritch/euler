import Data.List (genericLength, genericIndex)

import Test.Hspec (hspec, describe, it, shouldBe, SpecWith)

import Problems (problems)

safeIndex :: (Integral a) => [b] -> a -> Maybe b
safeIndex xs i
  | i >= 0 && i < genericLength xs = Just $ xs `genericIndex` i
  | otherwise = Nothing

expect :: (Integral a, Show a, Integral b, Show b) => a -> b -> SpecWith ()
expect question answer = it (show question) $
                            problems `safeIndex` (question - 1) `shouldBe`
                            Just answer

main :: IO ()
main = hspec $
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
