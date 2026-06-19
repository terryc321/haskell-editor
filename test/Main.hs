module Main (main) where

import Test.QuickCheck

import Editor

main :: IO ()
main = do
  quickCheck prop_run

prop_run :: [Char] -> [Char] -> Bool
prop_run xs ys =
  let e = Editor {left = xs , right = ys}
  in run [] e == e

