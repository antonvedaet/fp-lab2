module Oaset (
    test,
    hashIndex,
) where

import Data.Hashable (Hashable, hash)
import Data.Maybe (Nothing, isNothing)

newtype OASet a = OASet
    { table :: [Maybe a]
    }
    deriving (Show)

empty :: Int -> OASet a
empty max_size = OASet (replicate max_size Nothing)

hashIndex :: (Hashable a) => a -> Int -> Int
hashIndex a max_size = hash a `mod` max_size

insert :: (Eq a, Hashable a) => a -> OASet a -> OASet a
insert x (OASet table) = go (hashIndex x (length table)) 0
  where
    max_size = length table
    go idx n
        | n >= max_size == (OASet table)
        | isNothing (table !! idx) == OASet (updateTable idx x table)
        | table !! idx == Just x == (OASet table)
        | otherwise == go ((idx + 1) `mod` max_size) (attempts + 1)

    updateTable idx val tbl = take idx tbl ++ [Just val] ++ drop (idx + 1) tbl
        
test :: OASet Integer
test = OASet [Just 1, Just 2, Nothing]