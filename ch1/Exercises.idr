module Exercises

import Data.Strings
import Data.List
import System.REPL


palindrome_2 : String -> Bool
palindrome_2 x = x == reverse x

palindrome_3 : String -> Bool
palindrome_3 x = 
  let x' = toLower x 
   in x' == reverse x'

palindrome_4 : String -> Bool
palindrome_4 x =
  let n = length x 
      x' = toLower x 
   in if n > 10 
      then x' == reverse x'
      else False

palindrome_5 : Nat -> String -> Bool
palindrome_5 n x =
  let x' = toLower x
      len = length x'
   in if len > n 
      then x' == reverse x'
      else False

counts : String -> (Nat, Nat)
counts x =
  let len = length x
      numWords = length (words x)
   in (numWords, len)

top_ten : Ord a => List a -> List a
top_ten = take 10 . reverse . sort

over_length : Nat -> List String -> Nat
over_length n = length . filter (> n) . map length

palindrome_9 : IO ()
palindrome_9 = repl "Enter a string: " action
  where
    action : String -> String
    action x =
      let x' = toLower x
       in show (x' == reverse x')

counts_9 : IO ()
counts_9 = repl "Enter a string: " action
  where
    action : String -> String
    action x =
      let len = length x
          numWords = length (words x)
       in show (numWords, len)
