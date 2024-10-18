import Test.HUnit
import Oaset

testEmpty :: Test
testEmpty = TestCase $ do
    let set = empty 10 :: OASet Int
    assertEqual "Empty set should have only Nothings" (OASet (replicate 10 Nothing)) set

testInsert :: Test
testInsert = TestCase $ do
    let set = insert 5 (empty 10) :: OASet Int
    assertEqual "5 should be in the set" (OASet [Nothing, Nothing, Nothing, Nothing, Nothing, Just 5, Nothing, Nothing, Nothing, Nothing]) set

testRemove :: Test
testRemove = TestCase $ do
    let set = remove 5 (insert 5 (empty 10)) :: OASet Int
    assertEqual "5 should be removed from the set" (empty 10 :: OASet Int) set

testFilter :: Test
testFilter = TestCase $ do
    let set = insert 3 (insert 2 (insert 1 (empty 10))) :: OASet Int
        filteredSet = Oaset.filter (> 2) set
    assertEqual "Filter should keep elements greater than 2" (OASet [Nothing, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]) filteredSet

testMap :: Test
testMap = TestCase $ do
    let set = insert 3 (insert 2 (insert 1 (empty 10))) :: OASet Int
        mappedSet = Oaset.map (+1) set
    assertEqual "Map should add 1 to all elements" (OASet [Nothing, Just 2, Just 3, Just 4, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]) mappedSet

testFoldl :: Test
testFoldl = TestCase $ do
    let set = insert 3 (insert 2 (insert 1 (empty 10))) :: OASet Int
        sumSet = Oaset.foldl (+) 0 set
    assertEqual "Foldl should sum all elements" 6 sumSet

testFoldr :: Test
testFoldr = TestCase $ do
    let set = insert 3 (insert 2 (insert 1 (empty 10))) :: OASet Int
        sumSet = Oaset.foldr (+) 0 set
    assertEqual "Foldr should sum all elements" 6 sumSet

tests :: Test
tests = TestList [ TestLabel "testEmpty" testEmpty
                 , TestLabel "testInsert" testInsert
                 , TestLabel "testRemove" testRemove
                 , TestLabel "testFilter" testFilter
                 , TestLabel "testMap" testMap
                 , TestLabel "testFoldl" testFoldl
                 , TestLabel "testFoldr" testFoldr
                 ]

main :: IO Counts
main = runTestTT tests
