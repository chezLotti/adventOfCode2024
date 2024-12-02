module DayTwo (

) where

-- star 1
getDistances :: [Int] -> [(Int, Int)]
getDistances xs = [(abs $ x - y, signum $ x - y) | (x, y) <- zip xs (tail xs)]

checkIfSafe :: [(a, Int)] -> Bool
checkIfSafe xs = (length xs) == (abs $ sum [snd x | x <- xs])

countSafeReports :: [[Int]] -> Int
countSafeReports xs = length [1 | x <- xs, checkIfSafe (getDistances x)]

-- star 2
