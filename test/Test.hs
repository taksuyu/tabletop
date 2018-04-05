module Main where

import GHC.IO.Encoding
import System.IO

-- tests
import Test.Backhand.Modules.Ur

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  _results <- sequence [
    Test.Backhand.Modules.Ur.tests
    ]

  pure ()
