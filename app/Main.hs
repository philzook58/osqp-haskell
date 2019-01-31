module Main where

import Lib

main :: IO ()
main = do x <- demo
          print x
          print "Done"
