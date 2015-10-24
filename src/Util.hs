{-
Utility functions for Project Euler.
-}

module Util where

import Data.Array.Unboxed (UArray, assocs, accumArray)
import Data.List (group, genericLength)

-- Mathematical sequences
-- Primes
primes :: (Integral a) => [a]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `rem` p /= 0]

-- Fast primes up to Int width
-- 32 bit machines: 30 bits, which is up to 2^29 - 1 == 536 870 911
-- 64 bit machines: 61 bits, which is up to 2^60 - 1 == 1 152 921 504 606 846 975
primes' :: [Int]
primes' = 2 : oddprimes ()
  where
    oddprimes () = 3 : sieve (oddprimes ()) 3 []
    sieve (p:ps) x fs = [i * 2 + x | (i, True) <- assocs a]
                        ++ sieve ps (p * p)
                            ((p, 0) : [(s, (y - q) `rem` s) | (s, y) <- fs])
      where
        q = (p * p - x) `div` 2
        a :: UArray Int Bool
        a = accumArray (\_ _ -> False) True (1, q - 1)
                       [(i, ()) | (s, y) <- fs, i <- [y + s, y + s + s..q]]

-- Prime factorisation
_primeFactors :: (Integral a) => a -> [a] -> [a]
_primeFactors x (p:ps)
  | p * p > x = [x]
  | x `mod` p == 0 = p : _primeFactors (x `div` p) (p:ps)
  | otherwise = _primeFactors x ps

primeFactors :: (Integral a) => a -> [a]
primeFactors n = _primeFactors n primes

primeFactors' :: Int -> [Int]
primeFactors' n = _primeFactors n primes'

-- Fibonacci series
fibs :: (Integral a) => [a]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Collatz sequence
collatz :: (Integral a) => a -> a
collatz n
  | even n = n `div` 2
  | odd n = 3 * n + 1

collatz' :: Int -> Int
collatz' n
  | even n = n `div` 2
  | odd n = 3 * n + 1

collatzSeq :: (Integral a) => a -> [a]
collatzSeq = iterate collatz

collatzSeq' :: Int -> [Int]
collatzSeq' = iterate collatz'

collatzLen :: (Integral a) => a -> a
collatzLen n = (+ 1) . genericLength $ takeWhile (/= 1) $ collatzSeq n

collatzLen' :: Int -> Int
collatzLen' n = (+ 1) . length $ takeWhile (/= 1) $ collatzSeq' n

-- Commonly used filters
-- Palindromes
isPalindrome :: (Show a) => a -> Bool
isPalindrome n = s == reverse s where s = show n

-- Right triangles
isPythagoreanTriple :: (Integral a) => a -> a -> a -> Bool
isPythagoreanTriple a b c = square a + square b == square c

-- Primality tests
isPrime :: (Integral a) => a -> Bool
isPrime n
  | n < 2 = False
  | otherwise = length (primeFactors n) <= 1

isPrime' :: Int -> Bool
isPrime' n
  | n < 2 = False
  | otherwise = length (primeFactors' n) <= 1

-- Commonly used functions on numbers
-- Squaring a number
square :: (Integral a) => a -> a
square n = n * n

-- Factorial of a number
factorial :: (Integral a) => a -> a
factorial 1 = 1
factorial n = n * factorial (n - 1)

-- Turn a number into a list of its digits in right-to-left order
toDigits :: (Integral a) => a -> [a]
toDigits 0 = []
toDigits n = (n `mod` 10) : toDigits (n `div` 10)

toDigitsOrd :: (Integral a) => a -> [a]
toDigitsOrd = reverse . toDigits

-- Turn a list of digits into a number
fromDigits :: (Integral a) => [a] -> a
fromDigits digits = _fromDigits $ reverse digits

_fromDigits :: (Integral a) => [a] -> a
_fromDigits [] = 0
_fromDigits (x:xs) = x + 10 * _fromDigits xs

-- Compute the number of unique divisors for a number
numDivisors :: (Integral a) => a -> a
numDivisors n = product $ map ((+ 1) . genericLength) $ group (primeFactors n)

numDivisors' :: Int -> Int
numDivisors' n = product $ map ((+ 1) . length) $ group (primeFactors' n)

-- Permutations
choose :: (Integral a) => a -> a -> a
choose _ 0 = 1
choose 0 _ = 0
choose n k = choose (n - 1) (k - 1) * n `div` k

-- Commonly used functions on sets
-- Power set of a set
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)

-- Commonly used functions on lists
-- Find the index of the maximum element in a list
maxIndex :: (Ord a, Integral b) => [a] -> b
maxIndex xs = snd . maximum $ zip xs [0..]
