{-
Utility functions for Project Euler.
-}

module Util where

import Data.Array.Unboxed (UArray, assocs, accumArray)

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

-- Turn a number into a list of its digits
toDigits :: (Integral a) => a -> [a]
toDigits 0 = []
toDigits n = (n `mod` 10) : toDigits (n `div` 10)

-- Set functions
-- Power set of a set
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)
