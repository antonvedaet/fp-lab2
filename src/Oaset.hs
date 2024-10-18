module Oaset (
    empty,
    insert,
    remove,
    Oaset.filter,
    Oaset.map,
    Oaset.foldl,
    Oaset.foldr,
    OASet(..)
) where

import Data.Hashable (Hashable, hash)
import Data.Maybe (isNothing)

newtype OASet a = OASet
    { table :: [Maybe a]
    }
    deriving (Show)

instance (Eq a) => Eq (OASet a) where
  (OASet t1) == (OASet t2) = t1 == t2

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

filter :: (a -> Bool) -> OASet a -> OASet a
filter predicate (OASet table) = OASet (go table)
  where
    go [] = []
    go (Nothing:xs) = Nothing : go xs
    go (Just x:xs)
      | predicate x = Just x : go xs
      | otherwise = Nothing : go xs

map :: (a -> b) -> OASet a -> OASet b
map f (OASet table) = OASet (go table)
  where
    go [] = []
    go (Nothing:xs) = Nothing : go xs
    go (Just x:xs) = Just (f x) : go xs

foldl :: (b -> a -> b) -> b -> OASet a -> b
foldl f acc (OASet table) = foldlTable acc table
  where
    foldlTable acc [] = acc
    foldlTable acc (Nothing:xs) = foldlTable acc xs
    foldlTable acc (Just x:xs) = foldlTable (f acc x) xs

foldr :: (a -> b -> b) -> b -> OASet a -> b
foldr f acc (OASet table) = foldrTable table acc
  where
    foldrTable [] acc = acc
    foldrTable (Nothing:xs) acc = foldrTable xs acc
    foldrTable (Just x:xs) acc = f x (foldrTable xs acc)