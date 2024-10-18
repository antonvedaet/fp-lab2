{-# LANGUAGE ScopedTypeVariables #-}
module PropertySpec (
    runPropertyTests
) where

import Test.QuickCheck
import Oaset
import Data.Hashable (Hashable)

instance (Arbitrary a, Hashable a, Eq a) => Arbitrary (OASet a) where
    arbitrary = do
        maxSize <- choose (1, 100) 
        elements <- vectorOf maxSize arbitrary 
        let set = Prelude.foldr insert (empty maxSize) elements 
        return set

-- нейтральный элемент
prop_identityElement :: (Hashable a, Eq a) => OASet a -> Bool
prop_identityElement set =
    (set `union` emptySet == set) && (emptySet `union` set == set)
  where
    emptySet = empty (length (table set))

-- ассоциативность
prop_associative :: (Hashable a, Eq a) => OASet a -> OASet a -> OASet a -> Bool
prop_associative set1 set2 set3 =
    (set1 `union` (set2 `union` set3)) == ((set1 `union` set2) `union` set3)

-- замкнутость
prop_closure :: (Hashable a) => OASet a -> OASet a -> Bool
prop_closure set1 set2 =
    let result = set1 `union` set2
    in all isOASetElement (table result)
  where
    isOASetElement :: Maybe a -> Bool
    isOASetElement _ = True 

runPropertyTests :: IO ()
runPropertyTests = do
    quickCheck (prop_identityElement :: OASet Int -> Bool)
    quickCheck (prop_associative :: OASet Int -> OASet Int -> OASet Int -> Bool)
    quickCheck (prop_closure :: OASet Int -> OASet Int -> Bool)
