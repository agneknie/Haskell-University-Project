{-
   Class proided as an example of the implementation of
   BST Trees in Haskell
-}

module BSTTrees where
data Tree a = Empty | Node (Tree a) a (Tree a) -- deriving Show

-- declare Tree a to be an instance of Show
instance (Show a) => Show (Tree a) where
  -- will start by a '<' before the root
  -- and put a : a begining of line
  show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
    where
    -- treeshow pref Tree
    --   shows a tree and starts each line with pref
    -- We don't display the Empty tree
    treeshow pref Empty = ""
    -- Leaf
    treeshow pref (Node Empty x Empty) =
                  (pshow pref x)

    -- Right branch is empty
    treeshow pref (Node left x Empty) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`-." "   " left)

    -- Left branch is empty
    treeshow pref (Node Empty x right) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "`-." "   " right)

    -- Tree with left and right children non empty
    treeshow pref (Node left x right) =
                  (pshow pref x) ++ "\n" ++
                  (showSon pref "|-." "|  " left) ++ "\n" ++
                  (showSon pref "`-." "   " right)

    -- shows a tree using some prefixes to make it nice
    showSon pref before next t =
                  pref ++ before ++ treeshow (pref ++ next) t

    -- pshow replaces "\n" by "\n"++pref
    pshow pref x = replace '\n' ("\n"++pref) (show x)

    -- replaces one char by another string
    replace c new string =
      concatMap (change c new) string
      where
          change c new x
              | x == c = new
              | otherwise = x:[] -- "x"

height :: (Tree a) -> Int
height Empty = 0
height (Node left a right)
     = 1 + max (height left) (height right)

isComplete :: (Tree a) -> Bool
isComplete Empty = True
isComplete (Node left a right)
          = isComplete left &&
            isComplete right &&
            height left == height right

balance :: [a] -> Tree a
balance [] = Empty
balance (x:xs) = Node (balance fstHalf) x (balance sndHalf)
      where (fstHalf, sndHalf) = splitAt ((length xs) `div` 2) xs

insert :: Ord a => a -> Tree a -> Tree a
insert a Empty = Node Empty a Empty
insert a (Node left root right)
    | a < root  = Node (insert a left) root right
    | otherwise = Node left root (insert a right)

foldTree :: Ord a => [a] -> Tree a
foldTree = foldr insert Empty

contains :: Ord a => a -> Tree a -> Bool
contains a Empty = False
contains a (Node left root right)
    | a == root = True
    | a < root  = contains a left
    | otherwise = contains a right

avlInsert:: Ord a => a -> Tree a -> Tree a
avlInsert v Empty = Node Empty v Empty
avlInsert v t@(Node left root right)
    | v > root  = rotate (Node left root (avlInsert v right))
    | v < root  = rotate (Node (avlInsert v left) root right)
    | otherwise = t

foldAVLTree :: Ord a => [a] -> Tree a
foldAVLTree = foldr avlInsert Empty

balanced :: (Ord a) => Tree a -> Bool
balanced Empty = True
balanced  (Node l root r)
   | not (balanced l) = False
   | not (balanced r) = False
   | abs ((height l) - (height r)) > 1 = False
   | otherwise = True

rotate :: (Ord a) => Tree a -> Tree a
rotate Empty = Empty
rotate (Node l root r)
    | not (balanced l)
                = Node (rotate l) root r
    | not (balanced r)
                = Node l root (rotate r)
    | (height l) + 1 < (height r) && (height (left r)) < (height (right r))
                = Node (Node l root (left r)) (value r) ((right r))
    | (height r) + 1 < (height l) && (height (right l)) < (height (left l))
                = Node ((left l)) (value l) (Node ((right l)) root r)
    | (height l) + 1 < (height r) && (height (left r)) > (height (right r))
                = Node (Node l root (left (left r))) (value (left r))
                  (Node (right (left r)) (value r) (right r))
    | (height r) + 1 < (height l) && (height (right l)) > (height (left l))
                = Node (Node (left l) (value l) (left (right l))) (value (right l))
                  (Node (right (right l)) root r)
    | otherwise = Node l root r
    where
    left (Node l root r) = l
    right (Node l root r) = r
    value (Node l root r) = root

minimum' :: Eq a => Tree a -> a
minimum' Empty = error "Empty tree"
minimum' (Node Empty root _) = root
minimum' (Node left root _) = minimum' left

maximum' :: Eq a => Tree a -> a
maximum' Empty = error "Empty tree"
maximum' (Node _ root Empty) = root
maximum' (Node _ root right) = maximum' right

successor :: Ord a => a -> Tree a -> a
successor val (Node left root right)
    | val == root = minimum' right
    | val < root  = if val >= (maximum' left) then root else successor val left
    | otherwise = successor val right
