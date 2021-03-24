import BSTTrees

-- Exercise 13 from LabWeek2 (For Exercise 4)
luhnDouble :: Int -> Int
luhnDouble number 
    | number*2 > 9        = number*2-9
    | otherwise           = number*2

-- Exercise 1
curryDiff :: ((a, b) -> c) -> a -> b -> c
curryDiff f x y = f (x,y)

uncurryDiff :: (a -> b -> c) -> (a, b) -> c
uncurryDiff f (x, y) = f x y
-- part a
minimumDiff :: Eq a => Tree a -> a
minimumDiff Empty = error "Empty tree"
minimumDiff (Node Empty root _) = root
minimumDiff (Node left root _) = minimumDiff left

-- part b
maximumDiff :: Eq a => Tree a -> a
maximumDiff Empty = error "Empty tree"
maximumDiff (Node _ root Empty) = root
maximumDiff (Node _ root right) = maximumDiff right

-- part c
isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

successorDiff :: Ord a => a -> Tree a -> a
successorDiff val (Node left root right)
    | val == root = minimumDiff right
    | val < root = if isEmpty left || val >= (maximumDiff left)
    then root else successorDiff val left
    | otherwise = successorDiff val right

-- part d
predecessor :: Ord a => a -> Tree a -> a
predecessor val (Node left root right)
    | val == root = maximumDiff left
    | val > root = if isEmpty right || val <= (minimumDiff right)
    then root else predecessor val right
    | otherwise = predecessor val left

-- Exercise 3
altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap _ _ [] = []      -- List is empty (base case)
altMap f g [x] = [f x]  -- List has one element (another base case)
altMap f g (x:y:xs) = [f x] ++ [g y] ++ altMap f g xs

-- Exercise 4
checkCardNumber :: [Int] -> Bool
checkCardNumber numbers = sum modNumbers `rem` 10 == 0
    where
    modNumbers = altMap (+0) luhnDouble numbers

-- Exercise 5
pipeline :: [(a->a)] -> [a] -> [a]
pipeline fns xs = map (piper fns) xs
    where
    piper [] = id
    piper (fn:fns) = fn . (piper fns)