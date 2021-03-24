
module LabWeek4 where
    import Data.Char ( ord, isLower, isUpper, chr, toLower ) 

    -- Converts a letter to an integer
    let2int :: Char -> Int
    let2int c = ord c - ord 'a'
    
    -- Converts an integer to a letter
    int2let :: Int -> Char
    int2let n = chr (ord 'a' + n) 

    -- Converts a given letter into the one which is in specified number of increments
    shift :: Int -> Char -> Char
    shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
              | isUpper c = shift n (toLower c)
              | otherwise = c
    
    -- Implementation of Caesar Cipher with recursion
    caesarCipherRecursion :: Int -> String -> String
    caesarCipherRecursion n [] = []
    caesarCipherRecursion n (c:cs) = [shift n c] ++ caesarCipherRecursion n cs

    -- Implementation of Caesar Cipher with list comprehension
    caesarCipherListCompr :: Int -> String -> String
    caesarCipherListCompr n s = [shift n c | c <- s]

    -- Implementation of Caesar Cipher with mapping
    caesarCipherMap :: Int -> String -> String
    caesarCipherMap n s = map (shift n) s

    -- Function which outputs a list which represents a grid of given dimensions
    grid :: Int -> Int -> [(Int, Int)]
    grid m n = [(x, y) | x <- [0..m], y <- [0..n]]