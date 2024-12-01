{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DayOne (

) where
import GHC.OldList (sort)

inputFile :: String
inputFile = "/home/charlotte/projects/haskell/adventOfCode2024/src/day1/star1input.txt"

handleInput :: ([Int] -> [Int] -> b) -> FilePath -> IO b
handleInput f filePath= do
  contents <- readFile filePath
  let lineContents = lines contents
  let wordsPerLine = [words x | x <- lineContents]
  let firstWord = [read (head x) :: Int | x <- wordsPerLine]
  let lastWord = [read (last x) :: Int | x <- wordsPerLine]
  return (f firstWord lastWord)

-- star 1
distanceBetweenLists :: (Num a, Ord a) => [a] -> [a] -> a
distanceBetweenLists xs ys = sum [abs (a - b) | (a, b) <- zip (sort xs) (sort ys)]

--star 2
countOccurences :: (Num a1, Eq a2) => a2 -> [a2] -> a1
countOccurences n xs = sum [1 | x <- xs, n == x]

similarity :: (Num a2, Eq a2) => [a2] -> [a2] -> a2
similarity xs ys = sum [x * countOccurences x ys | x <- xs]

