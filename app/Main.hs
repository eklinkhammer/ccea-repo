module Main where

import Lib

main :: IO ()
main = do
  val <- someFunc
  putStrLn $ show val
    
