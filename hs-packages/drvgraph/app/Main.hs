module Main (main) where

import DrvGraph (greet)

main :: IO ()
main = putStrLn $ greet "World"
