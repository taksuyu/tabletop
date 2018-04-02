module Main where

import System.IO

-- tests
import Test.Backhand.Modules.Ur

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence [
    Test.Backhand.Modules.Ur.tests
    ]

  pure ()
