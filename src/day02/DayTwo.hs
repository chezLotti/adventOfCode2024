{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DayTwo (

) where

inputFile :: String
inputFile = "/home/charlotte/projects/haskell/adventOfCode2024/src/day02/input.txt"

handleInput :: ([[Int]] -> b) -> FilePath -> IO b
handleInput f filePath = do
  contents <- readFile filePath
  let lineContents = lines contents
  let entriesPerLine = [map read (words x) :: [Int] | x <- lineContents]
  return (f entriesPerLine)

{-
second :: (a, b, c) -> b
second (_, x, _) = x

first :: (a, b, c) -> a
first (x, _, _) = x

third :: (a, b, c) -> c
third (_, _, x) = x
-}

-- star 1
getDistances :: Num b => [b] -> [(b, b)]
getDistances xs =
  [(abs $ x - y, signum $ x - y) | (x, y) <- zip xs (tail xs)]

checkIfAscOrDsc :: [(a, Int)] -> Bool
checkIfAscOrDsc xs = length xs == abs (sum [snd x | x <- xs])

checkDistances :: (Eq a, Num a) => [(a, b)] -> Bool
checkDistances xs
  | null xs = True
  | x /= 1 && x /= 2 && x /= 3 = False
  | x == 1 || x == 2 || x == 3 = checkDistances (tail xs)
  | otherwise = True
  where
    x = fst $ head xs

countSafeReports :: [[Int]] -> Int
countSafeReports xs =
  length [x | x <- xs
            , let ys = getDistances x
            , checkIfAscOrDsc ys && checkDistances ys]

-- star 2
