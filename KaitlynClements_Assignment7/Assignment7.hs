import GHC.ForeignPtr (mallocPlainForeignPtrAlignedBytes)
{-
Author: Kaitlyn Clements
KUID: 3072622
Date: 11/13/2023
Inputs: 
Outputs: 
Description: 5 Haskell Functions
-}


-- replicate' function
{-
In a similar manner to the function length described in the "Haskell List Comprehension" lecture, 
show how the library function:
replicate :: Int -> a -> [a], 
which produces a list of identical elements, can be defined using a list comprehension. 
-}
replicate' :: Int -> a -> [a]
-- declares the replicate' function, takes in an integer and a 'string' type (polymorphic type)
replicate' n x = [x | _ <- [1..n]]
-- takes int n and value x and returns a list
-- Generates a list of length n where all elements are equal to x. X is the "test code" as shown below

--replicate' main in order to test the code 
mainReplicate :: IO ()
mainReplicate = do
    putStrLn "replicate' 5 \"test code\": "
    print $ replicate' 5 "test code"
    -- testing with input 5 "test code"

-- perfects function
{-
A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself. 
Using a list comprehension, define a function:
perfects :: Int->[Int]
that returns the list of all perfect numbers up to a given limit. 
-}
perfects :: Int -> [Int]
-- declares the perfects function, takes in an integer
perfects n = [x | x <- [1..n], x == sum (filter (\y -> x `mod` y == 0) [1..x-1])]
-- takes in a integer m and returns a list
-- for each x in the range 1 to n, it checks if x is equal to the sum of its divisors
-- filter filters the numbers in the range 1 to x-1 for which x is divisible
-- if x equals the sum of its proper divisors, it is a perfect number and gets added to the returned list

-- perfects main to test the code
mainPerfects :: IO ()
mainPerfects = do
    putStrLn "perfects 9000: "
    print $ perfects 9000
    -- testing with input 9000

-- find function
{-
Suppose that we represent a lookup table by a list of pairs of keys and values. Then for any type
of keys that supports equality, define a function as follows: 
find :: Eq a => a -> [(a,b)] -> [b]
that returns the list of all values that are associated with a given key in a table. 
-}
find :: Eq a => a -> [(a, b)] -> [b]
-- declares find function, takes a key a, and a list of pairs where each pair is (a, b). Returns a list of elements of type b
find key table = [v | (k, v) <- table, k == key]
-- takes a key and a table and returns a list
-- generates a list of values v from the pairs in table where teh key k is equal to the specified key
-- (k, v) <- table makes these two values an element in the table, and assigns the value k to the key
-- k == key checks if the specified key is equal to k, if so the corresponding v value is added to the returned list

-- find main to test the code
mainFind :: IO ()
mainFind = do
    putStrLn "find 'c' [('a', 1), ('b', 2), ('c', 3), ('b', 4), ('c', 25)]: "
    print $ find 'c' [('a', 1), ('b', 2), ('c', 3), ('b', 4), ('c', 25)]
    -- testing with input 'c' [('a', 1), ('b', 2), ('c', 3), ('b', 4), ('c', 25)

-- positions function
{-
Redefine the positions functions from the "Haskell List Comprehension" lecture using the find function
-}
positions :: Eq a => a -> [a] -> [Int]
-- declares the positions function, takes a value x of some type a that supports equality, and a list xs of elements of type a. Returns a list of integers
positions x xs = find x (zip xs [0..])
-- takes a value a and a list xs and returns a list of integers
-- zip xs [0..] creates a list of pairs by combining each element of the list xs with its index
-- find x (zip xs [0..]) uses find to find all occurrences of value x in the zipped list of elements and their indices
-- 


-- positions main in order to test the code
mainPositions :: IO ()
mainPositions = do
    putStrLn "positions 1 [1, 0, 0, 1, 0, 1, 1, 0]"
    print $ positions 1 [1, 0, 0, 1, 0, 1, 1, 0]
    -- testing with input 1 [1, 0, 0, 1, 0, 1, 1, 0]

-- scalarproduct function
{- 
The scalar product of two lists of integers xs and ys of length n is given by the sum of the 
products of the corresponding integers 
-}
scalarproduct :: [Int] -> [Int] -> Int
-- declares scalarproduct function, takes two lists of integers xs and ys and returns a single integer
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
-- Takes two lists, xs and ys, and returns their scalar product as an integer
-- zip xs ys combines corresponding elements of the lists into pairs
-- [x * y | (x, y) <- zip xs ys] creates a list of products of corresponding elements from zipped lists
-- sum adds up all the products in the list, and provides the integer which is the scalar product of the two lists. 

-- scalarproduct main to test the code
mainScalarProduct :: IO ()
mainScalarProduct = do
    putStrLn "scalarproduct [-1, 2, 3] [-4, -5, 6]: "
    print $ scalarproduct [-1, 2, 3] [-4, -5, 6]
    -- testing with input [-1, 2, 3] [-4, -5, 6]


-- top layer main function to call the rest of the main functions below
main :: IO ()
main = do
    mainReplicate
    mainPerfects
    mainFind 
    mainPositions
    mainScalarProduct