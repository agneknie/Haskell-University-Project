module LabWeek2 where
    -- TASK 3
    -- Function which halves a given even list into to parts
    halve :: [a] -> ([a], [a])
    halve a = (return1stHalf a, return2ndHalf a)

    -- Returns 1st half of a given list
    return1stHalf :: [a] -> [a]
    return1stHalf a = take (length a `div` 2) a

    -- Returns 2nd half of a given list
    return2ndHalf :: [a] -> [a]
    return2ndHalf a = drop (length a `div` 2) a
    
    -- TASK 4
    -- Defines a point in 2D
    type Point = (Int, Int)

    -- Computes Euclidean distance
    euclideanDistance :: Point -> Point -> Double
    euclideanDistance x y = sqrt (fromIntegral (xSquare + ySquare))
        where
            xSquare = (snd x - fst x)*(snd x - fst x)
            ySquare = (snd y - fst y)*(snd y - fst y)

    --TASK 5
    -- Returns first word in a String
    firstWord :: String -> String
    firstWord sentence = head (words sentence)

    --TASK 7
    -- Returns a list with its 1st element at the end
    stack :: [a] -> [a]
    stack list = listWithout1st ++ [first]
        where
            listWithout1st = tail list
            first = head list
    -- Checks if number is between 1 and 10
    range :: Int -> Bool
    range number | number < 10 && number > 0 = True
                 | otherwise = False
    -- Adds char to the beggining of a String
    addc :: Char -> String -> String
    addc givenChar givenString = givenChar : givenString
    -- Divides each element in a list by 2
    halves :: [Double] -> [Double]
    halves list = map (/2) list

    --TASK 10
    -- Increments an integer value
    incrementBy1 :: Int -> Int
    incrementBy1 = (+1)

    --TASK 13
    luhnDouble :: Int -> Int
    luhnDouble number | number*2>9 = number*2-9
                      | otherwise = number*2
    luhn :: Int -> Int -> Int -> Int -> Bool
    luhn a b c d | (luhnDouble a + luhnDouble b + luhnDouble c + luhnDouble d) `rem` 10 == 0 = True
                 | otherwise = False

    

