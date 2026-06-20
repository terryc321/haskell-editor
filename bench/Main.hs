{-# LANGUAGE NumericUnderscores #-}

module Main(main) where

import Criterion.Main

import qualified Editor as E
import qualified RefEditor as R
import qualified GapEditor as G

import System.Random (randomRIO)
import Control.Monad (replicateM)

benchEditor :: Int
benchEditor = let commands = replicate 100000 (E.Insert 'x')
                  eout = E.run commands E.empty
              in length (E.contents eout)

benchRefEditor :: Int
benchRefEditor = let commands = replicate 100000 (R.Insert 'x')
                     eout = R.run commands R.empty
                 in length (R.contents eout)

benchGapEditor :: Int
benchGapEditor = let commands = replicate 10000 (G.Insert 'x')
                     eout = G.run commands G.empty
                 in length (G.contents eout)


gapCommands :: Int -> [G.Command]
gapCommands n = replicate n (G.Insert 'x')

gapResult :: Int -> Int
gapResult n =  length (G.contents (G.run (gapCommands n) G.empty))
             --G.charCount $ G.run gapCommands G.empty


refCommands :: Int -> [R.Command]
refCommands n = replicate n (R.Insert 'x')

refResult :: Int -> Int
refResult n =  length (R.contents (R.run (refCommands n) R.empty))
             --R.charCount $ R.run refCommands R.empty

editCommands :: Int -> [E.Command]
editCommands n = replicate n (E.Insert 'x')

editResult :: Int -> Int
editResult n =  length (E.contents (E.run (editCommands n) E.empty))



main :: IO ()
main = defaultMain
  [ bench "Editor    with 1000 inserts"    $ nf editResult  1_000,
    bench "GapEditor with 1000 inserts"    $ nf gapResult   1_000,
    bench "RefEditor with 1000 inserts"    $ nf refResult   1_000,
    
    bench "Editor    with 10000 inserts"    $ nf editResult  10_000,
    bench "RefEditor with 10000 inserts"    $ nf refResult   10_000,
    bench "GapEditor with 10000 inserts"    $ nf gapResult   10_000
  ]


{--

20_000   0.512 second
40_000   3.012 second
60_000   8.012 second
100_000  8.192 second
         24.31 s


main :: IO ()
main = defaultMain
  [ bench "Editor with 100_000 inserts"    $ nf editResult   100_000,


    bench "RefEditor with 1_000 inserts"   $ nf refResult     1_000,
    
    bench "RefEditor with 200_000 inserts"    $ nf refResult   200_000,
    bench "RefEditor with 400_000 inserts"    $ nf refResult   400_000,
    bench "RefEditor with 600_000 inserts"    $ nf refResult   600_000,
    bench "RefEditor with 800_000 inserts"    $ nf refResult   800_000,
    bench "RefEditor with 1_000_000 inserts"  $ nf refResult 1_000_000,
    bench "RefEditor with 2_000_000 inserts"  $ nf refResult 2_000_000
  ]




--}  
-- main :: IO ()
-- main = defaultMain
--   [ bench "GapEditor" $ nf (\n -> n) benchGapEditor
--   ]



-- bench "GapEditor" $ nfIO benchGapEditor

-- main :: IO ()
-- main = do putStrLn $ "Hello from benchmark!"
--           -- benchEditor
--           -- benchRefEditor
--           benchGapEditor
--           putStrLn $ "Finished the first benchmark!"
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
