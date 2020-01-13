subset :: (Eq a) => [a] -> [a] -> Bool
subset [] _ = True
subset (x:xs) ys
    | elem x ys = subset xs ys
    | otherwise = False 

{- https://www.pagedon.com/rsa-explained-simply/programming
READ ME: Change the name of this file to YOURSTUDENTNUMBER.hs. For example, if your
student number were 123456789, then you would rename the file 123456789.hs.

REPLACE the function definitions for each of the questions. 
The names of the functions correspond to the names given in the document set07016_cwk_17_18.pdf. 

DO NOT CHANGE THE TYPE SIGNATURES, OR THE ORDER OF THE ARGUMENTS!

You may add as many extra or helper functions as you wish, but do not make any "import" statements.
-}

-- QUESTION 1: Sets

complement :: (Eq a) => [a] -> [a] -> Maybe [a]
complement [] [] = Nothing
complement xs ys
    | subset xs ys == False = Nothing
complement xs ys = Just (filter (\x -> notElem x xs) ys)

toMultiset :: (Eq a) => [a] -> [(a,Int)]
toMultiset [] = []
toMultiset (x:xs) = (x,1+count x xs) : toMultiset (filter (\y -> y /= x) xs)
  where count x [] = 0
        count x (y:ys) = if (x == y) then 1 + count x ys else count x ys

checker :: (Eq a) => (a,Int) -> (a,Int) -> (a,Int)
checker (x,y) (z,w)
    | x /= z = (x,y)
    | w <= y = (x,y)
    | otherwise = (z,w)


removeSmaller :: (Eq a) => (a,Int) -> [(a,Int)]-> [(a,Int)]
removeSmaller z (x : xs) = checker z x : xs

mUnion :: (Eq a) => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
mUnion [] ys =  []
mUnion [a] ys =  []
mUnion (x:xs) ys = (removeSmaller x ys) ++ mUnion xs ys
--mUnion (x:xs) (y:ys) = error "You've not tried to write toMultiset yet"

-- TEST SET FOR Q1
{-
Your functions should have the following behaviour:
complement [1,2,3] [1..5] = Just [4,5]
complement [1,2,3] [2..5] = Nothing
toMultiset [1,1,1,2,4,1,2] = [(1,4),(2,2),(4,1)]
toMultiset "from each according to his ability, to each according to his needs" = [('f',1),('m',1),('b',1),('l',1),('y',1),(',',1),('a',5),('c',6),('r',3),('g',2),('t',4),('o',6),('h',4),('i',6),(' ',11),('n',3),('e',4),('d',3),('s',3)]
mUnion [(1,6),(2,3)] [(1,2),(2,5),(3,1)] = [(1,6),(2,5),(3,1)]
mUnion [(1,2),(4,1)] [(1,1),(4,2)] = [(1,2),(4,2)]
 
THE ORDER OF ELEMENTS IN THE RESULTS OF mUnion IS NOT IMPORTANT.
-}


-- QUESTION 2: Functions and relations

giveSym :: (a,a) -> (a,a)
giveSym (x,y) = (y,x)

originalClosure :: (Eq a) => [(a,a)] -> [(a,a)]
originalClosure [] = []
originalClosure (x : xs) = x : originalClosure xs

symClosure' :: (Eq a) => [(a,a)] -> [(a,a)]
symClosure' [] = []
symClosure' (x : xs) = (giveSym x) : symClosure' xs

duplicateFree :: (Eq a) => [a] -> [a]
duplicateFree [] = []
duplicateFree (x:xs)
        | elem x xs = duplicateFree xs
        | otherwise = x : duplicateFree xs 

symClosure :: (Eq a) => [(a,a)] -> [(a,a)]
symClosure xs = duplicateFree ((originalClosure xs) ++ (symClosure' xs))

--symClosure' (x : xs) = xs

-- TEST SET FOR Q2
{-
Your functions should have the following behaviour:
symClosure [(1,2),(3,2)] = [(1,2),(3,2),(2,1),(2,3)]
symClosure [(1,1),(3,5)] = [(1,1),(3,5),(5,3)]

DO NOT WORRY ABOUT THE ORDER IN WHICH PAIRS APPEAR IN YOUR LIST
-}



-- QUESTION 3: Combinatorics

choose2 :: [Int] -> [(Int,Int)]
choose2 [] = []
choose2 (x:xs) = map ((,) x) xs ++ choose2 xs

-- TEST SET FOR Q3
{-
Your functions should have the following behaviour:
choose2 [1,2,3] = [(1,2),(1,3),(2,3)]
choose2 [2,6,9,12] = [(2,6),(2,9),(2,12),(6,9),(6,12),(9,12)]
NOTE THAT THE SMALLER ELEMENT IN EACH PAIR APPEARS FIRST. THE ORDERING OF THE PAIRS IN THE LIST DOES NOT MATTER.
-}




-- QUESTION 4: Primes

factors :: Int -> [Int]
factors n
    | n <= 99999999 = [x | x <- [2..n], n `mod` x == 0]
    | otherwise = error "You've not tried to write factors for big numbers yet"

primeFactorisation :: Int -> [Int]
primeFactorisation n
    | factors == []  = [n]
    | n <= 99999999 = factors ++ primeFactorisation (n `div` (head factors))
    | n <= 99999999 = factors ++ primeFactorisation (n `div` (head factors))
        where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

{- 
Leave the error messages in place if you do not want to attempt the parts for the input size. You should remove the guards up to the point you want to attempt. For example, if you were confident of anything up to five digits, the function would look like:

primeFactorisation n
    | n <= 99999 = whatever_your_calculation_is
    | n <= 999999 = error "..."
    | otherwise = error "..."

 -}




-- TEST SET FOR Q4
{-
Your functions should have the following behaviour:
factors 75 = [3,5,15,25,75]
factors 64 = [2,4,8,16,32,64]
primeFactorisation 75 = [3,5,5]
primeFactorisation 64 = [2,2,2,2,2,2]
-}




-- QUESTION 5: RSA

eTotient :: Int -> Int
eTotient n = length [p | p <- [1..n-1], gcd n p == 1]


encodeMsg :: Int -> Int -> Int -> Int -> Maybe Int
encodeMsg p q m e = Just (p*q)

isValid :: Int -> Int -> Int -> Bool
isValid p q e
    | mod (gcd p q) (p*q) /= 1 = False
    | gcd (eTotient(p*q)) e /= 1 = False
    | otherwise = True

encode :: Int -> Int -> Int -> Int -> Maybe Int
encode p q m e 
    | isValid p q e = Just (mod (m^e) (p*q))
    | otherwise = Nothing

-- TEST SET FOR Q5
{-
Your functions should have the following behaviour:
eTotient 54 = 18
eTotient 73 = 72
encode 53 73 151 95 = Just 3689
encode 99 18 108 45 = Nothing
encode 37 17 23 48 = Nothing
-}


