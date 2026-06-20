{-# OPTIONS_GHC -Wall #-}

module Main (main) where

-- import qualified Editor as G
-- import qualified GapEditor as G
import qualified RefEditor as G

-- import Editor
-- import qualified MyLib (someFunc)
-- import Test.QuickCheck

-- -- represents abc|def
-- example1 :: Editor 
-- example1 = Editor {left="abc",right= "def"}

-- example2 :: Editor 
-- example2 = run
--   [ MoveLeft
--   , MoveLeft
--   , Insert 'A'
--   , MoveRight
--   , Delete
--   , MoveRight
--   ]
--   example1

-- prop_reverse :: [Int] -> Bool
-- prop_reverse xs =
--     reverse (reverse xs) == xs


main :: IO ()
main =
  do -- putStrLn "Hello, Haskell!"
     putStrLn "enter size of replicate : "
     line <- getLine 
     --quickCheck prop_reverse
     -- putStrLn $ "you entered " ++ line
     let n = read line :: Int
       in do -- putStrLn $ "int value = " ++ show n
             let len = length $ G.contents $ G.run (replicate n (G.Insert 'x')) G.empty
             putStrLn $ "length = " ++ show len


     -- 
