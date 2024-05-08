module Main where

import qualified BrickMain as BM
import qualified DBController as DBC
main :: IO ()
--main = putStrLn "Hello, Haskell!"
--main = BM.run
main = --do DBC.runThis
        BM.run
