-- Function from LabWeek2
luhnDouble :: Int -> Int
luhnDouble number | number*2>9 = number*2-9
                  | otherwise = number*2

-- Implementation of Luhn Algorithm, part 1
luhn :: [Int] -> Bool
luhn [] = True
luhn numbers = (sum [luhnDouble n | n <- numbers]) `rem` 10 == 0

-- Implementation of Luhn Algorithm, part 2
luhnSum :: [Int] -> Int
luhnSum numbers = sum [luhnDouble n | n<-numbers]