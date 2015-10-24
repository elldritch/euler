module Group3 (p21, p22, p23, p24, p25, p26, p27, p28, p29, p30) where

import Data.Char (ord, digitToInt)
import qualified Data.Map as Map (Map, empty, member, lookup, insert)
import Data.Maybe (fromMaybe)
import Data.List (sort, nub, permutations, maximumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

import Util

{-
Let d(n) be defined as the sum of proper divisors of n (numbers less than n
which divide evenly into n).
If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
each of a and b are called amicable numbers.

For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55
and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and
142; so d(284) = 220.

Evaluate the sum of all the amicable numbers under 10000.
-}

isAmicable :: Int -> Bool
isAmicable n = partner /= n && amicability partner == n
  where partner = amicability n
        amicability = sum . nub . divisors

p21 :: (Integral a) => a
p21 = fromIntegral $ sum $ filter isAmicable [1..10000 - 1]

{-
Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
containing over five-thousand first names, begin by sorting it into alphabetical
order. Then working out the alphabetical value for each name, multiply this
value by its alphabetical position in the list to obtain a name score.

For example, when the list is sorted into alphabetical order, COLIN, which is
worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
obtain a score of 938 × 53 = 49714.

What is the total of all the name scores in the file?
-}

hasNoneOf :: (Eq a) => [a] -> [a] -> [a]
hasNoneOf (x:xs) = filter (/= x) . hasNoneOf xs
hasNoneOf _ = id

parseNames :: String -> [String]
parseNames = sort . splitOn "," . hasNoneOf "\\\"\n"

charValue :: Char -> Int
charValue c = ord c - 64

nameValue :: String -> Int
nameValue n = sum $ map charValue n

productT :: (Int, Int) -> Int
productT (a, b) = a * b

p22 :: (Integral a) => String -> a
p22 names = fromIntegral $ sum $ zipWith (curry productT) [1..] (map nameValue $ parseNames names)

{-
A perfect number is a number for which the sum of its proper divisors is exactly
equal to the number. For example, the sum of the proper divisors of 28 would be
1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123 can
be written as the sum of two abundant numbers. However, this upper limit cannot
be reduced any further by analysis even though it is known that the greatest
number that cannot be expressed as the sum of two abundant numbers is less than
this limit.

Find the sum of all the positive integers which cannot be written as the sum of
two abundant numbers.
-}

isAbundant :: Int -> Bool
isAbundant n = sum (nub $ divisors n) > n

abundantNumbers :: [Int]
abundantNumbers = [x | x <- [1..28123], isAbundant x]

abundantTwoSums :: [Int]
abundantTwoSums = [x + y | x <- abundantNumbers,
                           y <- abundantNumbers,
                           x + y <= 28123]

sortedNub :: [Int] -> [Int]
sortedNub (f:s:xs)
  | f == s = sortedNub (s:xs)
  | f /= s = f : sortedNub (s:xs)
sortedNub xs = xs

p23 :: (Integral a) => a
p23 = fromIntegral $ totalSum - abundantSum
  where abundantSum = sum $ sortedNub $ sort abundantTwoSums
        totalSum = sum [1..28123]

{-
 A permutation is an ordered arrangement of objects. For example, 3124 is one
possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
are listed numerically or alphabetically, we call it lexicographic order. The
lexicographic permutations of 0, 1 and 2 are:

012   021   102   120   201   210

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5,
6, 7, 8 and 9? -}

p24 :: (Integral a) => a
p24 = fromIntegral $ read $ last $ take 1000000 $ sort $ permutations "0123456789"

{-
The Fibonacci sequence is defined by the recurrence relation:

    F_{n} = F_{n−1} + F_{n−2}, where F_{1} = 1 and F_{2} = 1.

Hence the first 12 terms will be:

    F_{1} = 1
    F_{2} = 1
    F_{3} = 2
    F_{4} = 3
    F_{5} = 5
    F_{6} = 8
    F_{7} = 13
    F_{8} = 21
    F_{9} = 34
    F_{10} = 55
    F_{11} = 89
    F_{12} = 144

The 12th term, F_{12}, is the first term to contain three digits.

What is the index of the first term in the Fibonacci sequence to contain 1000
digits?
-}

p25 :: (Integral a) => a
p25 = fromIntegral $ fst $ head $ filter (\n -> (snd n `div` 10 ^ 999) >= 1) $ zip [1..] fibs

{-
A unit fraction contains 1 in the numerator. The decimal representation of the
unit fractions with denominators 2 to 10 are given:

    1/2 	= 	0.5
    1/3 	= 	0.(3)
    1/4 	= 	0.25
    1/5 	= 	0.2
    1/6 	= 	0.1(6)
    1/7 	= 	0.(142857)
    1/8 	= 	0.125
    1/9 	= 	0.(1)
    1/10	= 	0.1

Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in
its decimal fraction part.
-}

-- See http://mathworld.wolfram.com/DecimalExpansion.html
computeDecimalPeriod :: Integer -> Integer
computeDecimalPeriod n = _computeDecimalPeriod Map.empty n 0

_computeDecimalPeriod :: Map.Map Integer Integer -> Integer -> Integer -> Integer
_computeDecimalPeriod seen denom index
  | Map.member multOrder seen = index - fromMaybe 0 (Map.lookup multOrder seen)
  | otherwise = _computeDecimalPeriod (Map.insert multOrder index seen) denom (index + 1)
  where multOrder = (10 ^ index) `mod` denom

p26 :: (Integral a) => a
p26 = fromIntegral $ fst $ maximumBy (comparing snd) $ zip [1..] $ map computeDecimalPeriod [1..1000]

{-
Euler discovered the remarkable quadratic formula:

n^2 + n + 41

It turns out that the formula will produce 40 primes for the consecutive values
n = 0 to 39. However, when n = 40, 40^2 + 40 + 41 = 40(40 + 1) + 41 is divisible
by 41, and certainly when n = 41, 41^2 + 41 + 41 is clearly divisible by 41.

The incredible formula  n^2 − 79n + 1601 was discovered, which produces 80
primes for the consecutive values n = 0 to 79. The product of the coefficients,
−79 and 1601, is −126479.

Considering quadratics of the form:

    n^2 + an + b, where |a| < 1000 and |b| < 1000

    where |n| is the modulus/absolute value of n e.g. |11| = 11 and |−4| = 4

Find the product of the coefficients, a and b, for the quadratic expression that
produces the maximum number of primes for consecutive values of n, starting with
n = 0. -}

primes'' :: [Integer]
primes'' = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `rem` p /= 0]

primeFactors'' :: Integer -> [Integer]
primeFactors'' n = factor n primes''
  where
    factor x (p:ps)
      | p * p > x = [x]
      | x `mod` p == 0 = p : factor (x `div` p ) (p:ps)
      | otherwise = factor x ps

isPrime'' :: Integer -> Bool
isPrime'' n
  | n < 0 = False
  | otherwise = length (primeFactors'' n) <= 1

makeQuadratic :: Integer -> Integer -> Integer -> Integer
makeQuadratic a b n = n * n + a * n + b

consecutivePrimes :: (Integer -> Integer) -> Integer
consecutivePrimes f = snd $ until (not . isPrime'' . fst) f' (0, 0)
  where f' (_, count) = (f count, count + 1)

p27 :: (Integral a) => a
p27 = fromIntegral $ prodT $ fstTwo $ maximumBy (comparing third) [(a, b, consecutivePrimes $ makeQuadratic a b) | a <- [-999..999], b <- [-999..999]]
  where third (_, _, t) = t
        fstTwo (a, b, _) = (a, b)
        prodT (a, b) = a * b

{-
Starting with the number 1 and moving to the right in a clockwise direction a 5
by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed
in the same way?
-}

_diagonalSpiral :: Int -> Int -> Int -> [Int]
_diagonalSpiral 1 _ _ = 1 : _diagonalSpiral 3 2 2
_diagonalSpiral index gap 0 = index : _diagonalSpiral (index + gap) (gap + 2) 3
_diagonalSpiral index gap remaining = index : _diagonalSpiral (index + gap) gap (remaining - 1)

diagonalSpiral :: [Int]
diagonalSpiral = _diagonalSpiral 1 0 0

diagonalSpiralTo :: Int -> [Int]
diagonalSpiralTo size = takeWhile (<= size * size) diagonalSpiral

p28 :: (Integral a) => a
p28 = fromIntegral $ sum $ diagonalSpiralTo 1001

{-
Consider all integer combinations of a^b for 2 <= a <= 5 and 2 <= b <= 5:

    2^2=4, 2^3=8, 2^4=16, 2^5=32
    3^2=9, 3^3=27, 3^4=81, 3^5=243
    4^2=16, 4^3=64, 4^4=256, 4^5=1024
    5^2=25, 5^3=125, 5^4=625, 5^5=3125

If they are then placed in numerical order, with any repeats removed, we get the
following sequence of 15 distinct terms:

4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125

How many distinct terms are in the sequence generated by a^b for 2 <= a <= 100
and 2 <= b <= 100?
-}

p29 :: (Integral a) => a
p29 = fromIntegral $ length $ nub [a ^ b | a <- [2..100], b <- [2..100]]

{-
Surprisingly there are only three numbers that can be written as the sum of
fourth powers of their digits:

    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4

As 1 = 14 is not a sum it is not included.

The sum of these numbers is 1634 + 8208 + 9474 = 19316.

Find the sum of all the numbers that can be written as the sum of fifth powers
of their digits.
-}

-- 999999 > 9^6; therefore, the ceiling of these numbers is 9^6 == 354294

isSumOfFifthPowerOfDigits :: (Integral a) => a -> Bool
isSumOfFifthPowerOfDigits n
  | length (toDigits n) > 1 = sum (map (^ 5) $ toDigits n) == n
  | otherwise = False

p30 :: (Integral a) => a
p30 = fromIntegral $ sum $ filter isSumOfFifthPowerOfDigits [1..9 ^ 6]
