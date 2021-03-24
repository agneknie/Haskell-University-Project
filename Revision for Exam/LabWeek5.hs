-- Exercise 1
{-- Given:       [f x | x <- xs, p x]
    Answer:      filter p (map f xs) or map f (filter p xs) 
--}

-- Exercise 2
{-- mergesort takes a function which determines on what the sorting is made.
    e.g. mergesort (<) [1,5,8,2,6]
            [1,2,5,6,8]
    
    so my_function should be
    my_function :: (String, Int) -> (String, Int) -> Bool
    my_function (n1, g1) (n2, g2) = g1 < g2
--}

-- Exercise 3
maximumDiffRecursion :: Ord a => [a] -> a
maximumDiffRecursion [x] = x
maximumDiffRecursion (x:xs)
    | x > head xs           = maximumDiffRecursion (x : tail xs)
    | otherwise             = maximumDiffRecursion xs

maximumDiff :: Ord a => [a] -> a
-- maximumDiff list = foldl1 max list or:
maximumDiff = foldl1 max

-- Exercise 4
a :: [Int] -> [Int]
a list = [x+3 | x <- list]

b :: [Int] -> [Int]
b list = [x | x <- list, x > 7]

cGiven :: [Int] -> [Int] -> [(Int, Int)]
cGiven xs ys = concat (map (\x -> map (\y -> (x,y)) ys) xs)

c :: [Int] -> [Int] -> [(Int, Int)]
c aList bList = [(a, b) | a <- aList, b <- bList]

dGiven :: [(Int, Int)] -> [Int]
dGiven xys = filter (>3) (map (\(x,y) -> x+y) xys)

d :: [(Int, Int)] -> [Int]
d tuples = [x+y | (x,y) <- tuples, x+y>3]

-- Exercise 5
mystery :: [a] -> [a]
mystery xs = foldr (++) [] (map sing xs) 
    where 
    sing x = [x]
-- Function just outputs the same list