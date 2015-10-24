-- module Group4 (p31, p32, p33, p34, p35, p36, p37, p38, p39, p40) where
module Group4 where

import Numeric (showIntAtBase)
import Data.Array.Unboxed (UArray, assocs, accumArray)
import Data.Char (intToDigit)
import Data.Function (on)
import Data.List (permutations, nub, intersect, groupBy, maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Ratio ((%), denominator)
import Data.Set (Set, fromDistinctAscList, member, fromList)

import Util

{-
In England the currency is made up of pound, £, and pence, p, and there are
eight coins in general circulation:

    1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

It is possible to make £2 in the following way:

    1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p

How many different ways can £2 be made using any number of coins?
-}

coins :: (Integral a) => [a]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

waysToMake :: (Integral a) => a -> [a] -> a
waysToMake amount with
  | null with = 0
  | amount < 0 = 0
  | amount == 0 = 1
  | otherwise = waysToMake (amount - coin) with + waysToMake amount without
    where without = tail with
          coin = head with

p31 :: (Integral a) => a
p31 = waysToMake 200 coins

{-
We shall say that an n-digit number is pandigital if it makes use of all the
digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through
5 pandigital.

The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing
multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can
be written as a 1 through 9 pandigital. HINT: Some products can be obtained in
more than one way so be sure to only include it once in your sum.
-}

digits :: [Int]
digits = [1..9]

splitAt2 :: Int -> Int -> [Int] -> ([Int], [Int], [Int])
splitAt2 i i' xs = (take i xs, drop i (take i' xs), drop i' xs)

uncurry2 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry2 f (a, b, c) = f a b c

thd :: (a, b, c) -> c
thd (_, _, c) = c

digitsToNum :: [Int] -> Int
digitsToNum xs = reverseDigitsToNum $ reverse xs

digitsToNum3 :: ([Int], [Int], [Int]) -> (Int, Int, Int)
digitsToNum3 (a, b, c) = (digitsToNum a, digitsToNum b, digitsToNum c)

reverseDigitsToNum :: [Int] -> Int
reverseDigitsToNum [] = 0
reverseDigitsToNum xs = head xs + 10 * reverseDigitsToNum (tail xs)

mmpIdentity :: [Int] -> [Int] -> [Int] -> Bool
mmpIdentity a b c = digitsToNum a * digitsToNum b == digitsToNum c

p32 :: (Integral a) => a
p32 = fromIntegral $ sum $ nub $ map (thd . digitsToNum3) $ filter (uncurry2 mmpIdentity) splits'
  where splits' = [splitAt2 i i' xs | i <- [1..7], i' <- [(i + 1)..8], xs <- permutations digits]

{-
The fraction 49/98 is a curious fraction, as an inexperienced mathematician in
attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is
correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than
one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find
the value of the denominator.
-}

isCurious :: (Integral a) => a -> a -> Bool
isCurious numer denom
  | a == c && d /= 0 = (numer % denom) == (b % d)
  | a == d && c /= 0 = (numer % denom) == (b % c)
  | b == c && d /= 0 = (numer % denom) == (a % d)
  | b == d && c /= 0 = (numer % denom) == (a % c)
  | otherwise = False
  where [a, b] = toDigits numer
        [c, d] = toDigits denom

tens :: [Integer]
tens = [10..99]

-- Answer: 100
p33 :: (Integral a) => a
p33 = fromIntegral $ denominator $ product $ map (uncurry (%)) $ filter (\(a, b) ->
    (a `mod` 10 /= 0 || b `mod` 10 /= 0) &&
    a % b < 1 &&
    isCurious a b)
  [(a, b) | a <- tens, b <- tens]

{-
145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

Find the sum of all numbers which are equal to the sum of the factorial of their digits.

Note: as 1! = 1 and 2! = 2 are not sums they are not included.
-}

-- 9! == 362880

isCurious' :: (Integral a) => a -> Bool
isCurious' 1 = False
isCurious' 2 = False
isCurious' n = sum (map factorial $ toDigits n) == n

p34 :: (Integral a) => a
p34 = fromIntegral $ sum $ filter isCurious' [1..factorial 9]

{-
The number, 197, is called a circular prime because all rotations of the digits:
197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
73, 79, and 97.

How many circular primes are there below one million?
-}

primesTo :: Int -> [Int]
primesTo top = takeWhile (<= top) primes'

primesToSet :: Int -> Set Int
primesToSet top = fromDistinctAscList $ primesTo top

rotations :: [a] -> [[a]]
rotations xs = [rotate n xs | n <- [1..length xs]]

rotate :: Int -> [b] -> [b]
rotate _ [] = []
rotate n xs = take (length xs) (drop n (cycle xs))

rotationsNum :: (Integral a) => a -> [a]
rotationsNum n = map fromDigits $ rotations $ toDigitsOrd n

-- Answer: 55
p35 :: (Integral a) => a
p35 = fromIntegral $ length $ filter (\n ->
    and [member n' (primesToSet 1000000) | n' <- rotationsNum n])
  $ primesTo 1000000

{-
The decimal number, 585 = 1001001001_2 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in
base 10 and base 2.

(Please note that the palindromic number, in either base, may not include
leading zeros.)
-}

isDoublePalindrome :: (Integral a, Show a) => a -> Bool
isDoublePalindrome n = isPalindrome n && isPalindrome (showIntAtBase 2 intToDigit n "")

p36 :: (Integral a) => a
p36 = fromIntegral $ sum $ filter isDoublePalindrome [1..1000000]

{-
The number 3797 has an interesting property. Being prime itself, it is possible
to continuously remove digits from left to right, and remain prime at each
stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797,
379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to
right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
-}

isRTLTruncPrime :: Integer -> Bool
isRTLTruncPrime n = _isContinuouslyPrime n _RTLTrunc

isLTRTruncPrime :: Integer -> Bool
isLTRTruncPrime n = _isContinuouslyPrime n _LTRTrunc

isTruncPrime :: Integer -> Bool
isTruncPrime n = isRTLTruncPrime n && isLTRTruncPrime n

commonLog :: (Floating b) => Rational -> b
commonLog n = logBase 10 (fromRational n)

_LTRTrunc :: (Integral a) => a -> a
_LTRTrunc n = n `mod` 10 ^ floor (commonLog $ fromIntegral n)

_RTLTrunc :: (Integral a) => a -> a
_RTLTrunc n = n `div` 10

_isContinuouslyPrime :: (Integral a) => a -> (a -> a) -> Bool
_isContinuouslyPrime n f
  | n == 2 || n == 3 || n == 5 || n == 7 = False
  | otherwise = isPrime n && _isContinuouslyPrime' (f n) f

_isContinuouslyPrime' :: (Integral a) => a -> (a -> a) -> Bool
_isContinuouslyPrime' n f
  | n == 0 = True
  | otherwise = isPrime n && _isContinuouslyPrime' (f n) f

p37 :: (Integral a) => a
p37 = fromIntegral $ sum $ take 11 $ filter isTruncPrime [1..]

{-
Take the number 192 and multiply it by each of 1, 2, and 3:

    192 × 1 = 192
    192 × 2 = 384
    192 × 3 = 576

By concatenating each product we get the 1 to 9 pandigital, 192384576. We will
call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and
5, giving the pandigital, 918273645, which is the concatenated product of 9 and
(1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
concatenated product of an integer with (1,2, ... , n) where n > 1?
-}

pandigitals :: (Integral a) => Set [a]
pandigitals = fromList $ permutations [1..9]

concatProduct :: (Integral a) => a -> [a] -> a
concatProduct n xs = foldr1 concatNums $ map (* n) xs

concatNums :: (Integral a) => a -> a -> a
concatNums a b = a * 10 ^ numDigits b + b

numDigits :: (Integral a, Integral b) => a -> b
numDigits n
  | n < 10 = 1
  | otherwise = 1 + numDigits (n `div` 10)

panConcatProducts :: (Integral a) => a -> [a]
panConcatProducts n = filter (\x -> member (toDigits x) pandigitals) $
  filter (\x -> numDigits x < 10) $
  map (concatProduct n) [[1..x] | x <- [2..9]]

upperLimit :: Integer
upperLimit = last $ takeWhile (\x -> (numDigits x + numDigits (x * 2)) < 10) [1..]

p38 :: (Integral a) => a
p38 = fromIntegral $ maximum $ foldr1 (++) [panConcatProducts n | n <- [1..upperLimit]]

{-
If p is the perimeter of a right angle triangle with integral length sides,
{a,b,c}, there are exactly three solutions for p = 120.

{20,48,52}, {24,45,51}, {30,40,50}

For which value of p ≤ 1000, is the number of solutions maximised?
-}

isCoprime :: (Integral a) => a -> a -> Bool
isCoprime a b = null $ primeFactors a `intersect` primeFactors b

-- Euclid's formula for generating Pythagorean triples:
-- a = k * (m ^ 2 - n ^ 2), b = k * (2 * m * n), c = k * (m ^ 2 + n ^ 2)
-- where m, n, and k are positive integers with m > n, m − n odd, and with m and n coprime.
generateTriple :: (Integral a) => a -> a -> a -> (a, a, a)
generateTriple m n k = (k * (m * m - n * n), k * (2 * m * n), k * (m * m + n * n))

tripleSum :: (Integral a) => (a, a, a) -> a
tripleSum (a, b, c) = a + b + c

p39 :: (Integral a) => a
p39 = fromIntegral $ tripleSum $ head $ maximumBy (comparing length) triplesByPerim
  where maxPerim = 1000
        candidates = [
          (m, n, k) |
            m <- [1..maxPerim], n <- [1..maxPerim], k <- [1..maxPerim],
            m > n,
            odd (m - n),
            isCoprime m n]
        triples = filter (\sides -> tripleSum sides < maxPerim) $
          map (uncurry2 generateTriple) candidates
        triplesByPerim = filter (not . null) $
          groupBy ((==) `on` tripleSum) $
          sortBy (comparing tripleSum) triples
