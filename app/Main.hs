module Main (main) where

import Oaset (hashIndex, test)

main :: IO ()
main = print $ hashIndex "hellowdas" 8
