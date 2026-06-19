{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Editor

import qualified MyLib (someFunc)
import Test.QuickCheck

-- represents abc|def
example1 :: Editor 
example1 = Editor {left="abc",right= "def"}

example2 :: Editor 
example2 = run
  [ MoveLeft
  , MoveLeft
  , Insert 'A'
  , MoveRight
  , Delete
  , MoveRight
  ]
  example1

prop_reverse :: [Int] -> Bool
prop_reverse xs =
    reverse (reverse xs) == xs


main :: IO ()
main =
  do putStrLn "Hello, Haskell!"
     quickCheck prop_reverse

