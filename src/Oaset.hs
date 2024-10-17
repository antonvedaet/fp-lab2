module Oaset (
    test,
    hashIndex,
) where

import Data.Hashable (Hashable, hash)
import Data.Maybe (isNothing)

newtype OASet a = OASet
    { table :: [Maybe a]
    }
    deriving (Show)

empty :: Int -> OASet a
empty max_size = OASet (replicate max_size Nothing)

hashIndex :: (Hashable a) => a -> Int -> Int
hashIndex a max_size = hash a `mod` max_size

insert :: (Hashable a) => a -> OASet a -> OASet a
insert x (OASet table) = go (hashIndex x (length table)) 0
  where
    max_size = length table
    go idx n
        | n >= max_size = OASet table
        | isNothing (table !! idx) = OASet (updateTable idx x table)
        | table !! idx == Just x = OASet table
        | otherwise = go ((idx + 1) `mod` max_size) (n + 1)

    updateTable idx val tbl = take idx tbl ++ [Just val] ++ drop (idx + 1) tbl

remove :: (Hashable a) => a -> OASet a -> OASet a
remove x (OASet table) = go (hashIndex x (length table)) 0
  where
    max_size = length table
    go idx n
        | n >= max_size = OASet table
        | table !! idx == Just x = OASet (take idx table ++ [Nothing] ++ drop (idx + 1) table)
        | otherwise = go ((idx + 1) `mod` max_size) (n + 1)

test :: OASet Integer
test = remove 10 (insert 10 (empty 2))
