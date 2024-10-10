module Oaset (

) where

import Data.Hashable ()
import Data.Maybe ()

data OASet a = OASet
    { table :: [Maybe a]
    , size :: Int
    }
    deriving (Show)

empty :: Int -> OASet a
empty max_size = OASet (replicate max_size Nothing) 0
