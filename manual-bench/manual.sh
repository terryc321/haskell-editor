#!/bin/bash

clear 
echo "starting manual bench "

echo 1 | time cabal run

echo 10000 | time cabal run
echo 20000 | time cabal run
echo 30000 | time cabal run
echo 40000 | time cabal run
echo 50000 | time cabal run
echo 60000 | time cabal run
echo 70000 | time cabal run
echo 80000 | time cabal run
echo 90000 | time cabal run
echo 100000 | time cabal run
echo 110000 | time cabal run



