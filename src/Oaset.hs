module Oaset (
    test,
) where

import Data.Hashable (Hashable)
import Data.Maybe ()

newtype OASet a = OASet
    { table :: [Maybe a]
    }
    deriving (Show)

empty :: Int -> OASet a
empty max_size = OASet (replicate max_size Nothing)

-- insert :: (Eq a, Hashable a) => a -> OASet a -> OASet a
-- insert x (OASet table)  =

test :: OASet Integer
test = OASet [Just 1, Just 2, Nothing]