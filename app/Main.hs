{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Data (Data)
import Data.Typeable (Typeable)

import System.Console.CmdArgs (cmdArgs, def)

import Problems (problems)

{-# ANN module "HLint: ignore Use camelCase" #-}
data Euler = Euler { problem :: Int
                   , run_all :: Bool}
             deriving (Show, Data, Typeable)

data Action = RunAll | RunOne Int

-- Parse options into a valid Action or Nothing
parseOptions :: Euler -> Maybe Action
parseOptions (Euler problem runAll)
  | runAll && problem /= -1 = Nothing
  | runAll = Just RunAll
  | problem > 0 && problem < length problems + 1 = Just $ RunOne problem
  | otherwise = Nothing

-- Pretty-print lists
prettyPrint :: (Show a) => [a] -> String
prettyPrint xs = init $ _prettyPrint xs 1

_prettyPrint :: (Show a, Show b, Integral b) => [a] -> b -> String
_prettyPrint (x:xs) i = show i ++ ": " ++ show x ++ "\n"
                        ++ _prettyPrint xs (i + 1)
_prettyPrint [] _ = ""

-- CLI for interacting with answers
main :: IO ()
main = do
  options <- cmdArgs Euler{problem = -1, run_all = def}
  putStrLn $ case parseOptions options of
    Nothing -> "Invalid options. Use --help for usage"
    Just RunAll -> "Running all problems..." ++ "\n"
                   ++ prettyPrint problems
    Just (RunOne problem) -> "Running problem " ++ show problem ++ "..." ++ "\n"
                             ++ show (problems !! (problem - 1))
