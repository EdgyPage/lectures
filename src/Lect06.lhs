% CS 340: Programming Paradigms and Patterns
% Lect 06 - Recursion
% Michael Lee

\begin{code}
module Lect06 where
import Debug.Trace
import qualified Data.Set as Set
\end{code}

Recursion
=========

Agenda:

  - Some common patterns of recursion:
     A. Iteration & Reduction
     B. Filtering
     C. Combinations & Permutations
     D. Divide & Conquer
     E. Tail recursion & Accumulation
  - How to trace and debug in Haskell
  - How laziness affects recursive call evaluation


A. Iteration & Reduction

Iteration is the process of repeatedly applying a function to a value
until one or more conditions (base cases) are met. It often makes sense to 
think of iteration as incrementally "building up" a result, as in constructing 
a list element by element. Sometimes, iteration is used to "reduce" an input to a final value (e.g., as in summing up the elements of a list).

E.g., implement the following functions using iteration/reduction:

\begin{code}
-- a classic!
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)


-- sum up the elements of a list
sumList :: (Show a, Num a) => [a] -> a
sumList [] = 0
sumList (x:xs) = x + sumList xs


-- sometimes we iterate over lists in parallel
weightedSum :: (Show a, Num a) => [a] -> [a] -> a
weightedSum [] _ = 0
weightedSum _ [] = 0
weightedSum (w:ws) (v:vs) = w*v + weightedSum ws vs


-- sometimes we process more than one "item" at a time
swapLetters :: String -> String
swapLetters [] = []
swapLetters [x] = [x]
swapLetters (x:y:xs) = y : x : swapLetters xs


-- implement this using append (++)
cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs


-- can we do better? (why is it better?)
cycle'' :: [a] -> [a]
cycle'' xs = c xs
  where c [] = c xs
        c (y:ys) = y : c ys


-- we'll need to pass values into subsequent iterations to track progress
fibs :: [Integer]
fibs = f 0 1
  where f j k = j : f k (j+k)
\end{code}


B. Filtering (conditional iteration/reduction)

Filtering is the process of iterating over a list and processing only those elements that satisfy a given condition. 

\begin{code}
-- sum only the positive numbers in a list
sumPositives :: Integral a => [a] -> a
sumPositives [] = 0
sumPositives (x:xs) | x > 0     = x + sumPositives xs
                    | otherwise = sumPositives xs


-- palindroms are strings that read the same forwards as backwards
palindromes :: [String] -> [String]
palindromes [] = []
palindromes (w:ws) | w == reverse w = w : palindromes ws
                   | otherwise = palindromes ws
\end{code}


C. Combinations & Permutations

Combinations and permutations are classic problems in combinatorics that arise 
in many different problems.

\begin{code}
-- generate all combinations (order doesn't matter -- how many are there?)
combinations :: [a] -> [[a]]
combinations [] = [[]]
combinations (x:xs) = [ x:ys | ys <- combinations xs ] ++ combinations xs


-- generate all combinations of a given size (nCr = n!/(r!(n-r)!))
combinations' :: Int -> [a] -> [[a]]
combinations' 0 _ = [[]]
combinations' _ [] = []
combinations' n (x:xs) = [ x:ys | ys <- combinations' (n-1) xs ] 
                        ++ combinations' n xs


-- the "making change" problem
change :: (Ord a, Num a) => a -> [a] -> [[a]]
change _ [] = []
change 0 _ = [[]]
change amt (c:cs) 
  | amt < c   = change amt cs
  | otherwise = [ c:xs | xs <- change (amt-c) (c:cs) ] ++ change amt cs


-- the knapsack problem: given a list of items (value,weight) and a weight 
-- capacity, find the maximum value that can be carried
knapsack :: (Ord a, Num a) => a -> [(a,a)] -> a
knapsack _ [] = 0
knapsack wcap ((v,w):xs) 
  | w > wcap   = knapsack wcap xs
  | otherwise = max (v + knapsack (wcap-w) xs) (knapsack wcap xs)


-- find the actual set of items that maximizes value (under the weight cap)
knapsack' :: (Ord a, Num a) => a -> [(a,a)] -> [(a,a)]
knapsack' _ [] = []
knapsack' wcap ((v,w):xs) 
    | w > wcap   = knapsack' wcap xs
    | otherwise = let k1 = (v,w) : knapsack' (wcap-w) xs
                      k2 = knapsack' wcap xs
                  in if val k1 > val k2 then k1 else k2
  where val [] = 0
        val ((v,_):xs) = v + val xs


-- find the two closest points in a list of points (brute force)
closestPoints :: (Ord a, Num a) => [(a,a)] -> [(a,a)]
closestPoints ps = minByDist (combinations' 2 ps)
  where minByDist [] = []
        minByDist [p] = p
        minByDist (p:ps) = let q = minByDist ps
                           in if dist p < dist q then p else q
        dist [(x1,y1),(x2,y2)] = (x1-x2)*(x1-x2) + (y1-y2)*(y1-y2)


-- generate all permutations (order matters -- how many are there?)
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat [ interleave x p | p <- permutations xs ]
  where interleave x [] = [[x]]
        interleave x (y:ys) = (x:y:ys) : [ y:zs | zs <- interleave x ys]


-- generate all palindromes from a given string
allPalindromes :: String -> [String]
allPalindromes cs = [ p | p <- permutations cs, p == reverse p ]

    -- try Set.fromList (allPalindromes "alfalfa")
\end{code}


D. Divide & Conquer

Divide and conquer is a technique for solving problems by breaking them into
smaller subproblems and then combining the solutions to the subproblems to
obtain a solution to the original problem.

\begin{code}
-- a classic!
fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


-- sort by splitting the list in half and merging the sorted halves
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort l) (mergesort r)
  where (l,r) = splitAt (length xs `div` 2) xs
        merge [] ys = ys
        merge xs [] = xs
        merge l1@(x:xs) l2@(y:ys) | x <= y = x : merge xs l2
                                  | otherwise = y : merge l1 ys


-- sort by choosing a pivot and "partitioning" the list around it
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y < x] 
                ++ [x] 
                ++ quicksort [y | y <- xs, y >= x]


-- find the two closest points in a list of points (more efficiently)
closestPoints' :: (Ord a, Num a) => [(a,a)] -> [(a,a)]
closestPoints' = undefined
\end{code}


E. Tail recursion & Accumulation

Tail recursion is a special case of recursion where the recursive call is the
last thing done in the function.  In non-lazy languages, this is important
because it allows the compiler to optimize the code by eliminating the need
for a stack frame. In Haskell (and other lazy languages), tail recursion does 
not quite have the same importance, but it is still a useful technique.

Accumulation is a technique for solving problems by passing an extra
parameter to the recursive call that accumulates the solution.

\begin{code}
-- are all elements even?
allEven :: [Integer] -> Bool
allEven [] = True
allEven (x:xs) | even x = True
               | otherwise = allEven xs


-- are two lists the same length?
sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength [] _  = False
sameLength _  [] = False
sameLength (_:xs) (_:ys) = sameLength xs ys


-- tail recursive factorial with explicit accumulator
factorial' :: Integer -> Integer -> Integer
factorial' 0 r = r
factorial' n r = factorial' (n-1) (n*r)


-- tail recursive factorial with hidden accumulator
factorial'' :: Integer -> Integer
factorial'' n = f n 1
  where f 0 r = r
        f n r = f (n-1) (n*r)               


-- reverse a list using an accumulator
reverse' :: [a] -> [a]
reverse' xs = rev xs []
  where rev [] ys = ys
        rev (x:xs) ys = rev xs (x:ys)


-- enumerate the integers from m to n (with an accumulator)
enumFromTo' :: Integer -> Integer -> [Integer]
enumFromTo' m n = reverse (f m [])
  where f i xs | i > n = xs
               | otherwise = f (i+1) (i:xs)


-- can we write the infinite list version using an accumulator?
enumFrom' :: Integer -> [Integer]
enumFrom' n = f n []
  where f i xs = f (i+1) (i:xs)
\end{code}
