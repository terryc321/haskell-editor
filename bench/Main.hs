module Main(main) where

import Criterion

import qualified Editor as E
import qualified RefEditor as R
import qualified GapEditor as G


import System.Random (randomRIO)
import Control.Monad (replicateM)


benchEditor :: IO ()
benchEditor = do let commands = replicate 100000 (E.Insert 'x')
                 let eout = E.run commands E.empty
                 print (length (E.contents eout))

benchRefEditor :: IO ()
benchRefEditor = do let commands = replicate 100000 (R.Insert 'x')
                    let eout = R.run commands R.empty
                    print (length (R.contents eout))

benchGapEditor :: IO ()
benchGapEditor = do let commands = replicate 100000 (G.Insert 'x')
                    let eout = G.run commands G.empty
                    print (length (G.contents eout))
                 

main :: IO ()
main = do putStrLn $ "Hello from benchmark!"
          -- benchEditor
          -- benchRefEditor
          benchGapEditor
          putStrLn $ "Finished the first benchmark!"

          
          --- chars <- replicateM 100000 (randomRIO ('a', 'z'))
          --print (E.contents E.empty)
          
          

{--
Editor 
real	1m48.067s
user	1m47.947s
sys	0m0.243s

RefEditor
real	0m0.347s
user	0m0.210s
sys	0m0.099s


GapEditor


--}
