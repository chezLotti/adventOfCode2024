{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module DayOne (

) where
import GHC.OldList (sort)

filePath :: String
filePath = "/home/charlotte/projects/haskell/adventOfCode2024/src/day1/star1input.txt"

-- star 1
handleInput :: IO Int
handleInput = do
  contents <- readFile filePath
  let lineContents = lines contents
  let wordsPerLine = [words x | x <- lineContents]
  let firstWord = [read (head x) :: Int | x <- wordsPerLine]
  let lastWord = [read (last x) :: Int | x <- wordsPerLine]
  return (distanceBetweenLists firstWord lastWord)

distanceBetweenLists :: (Num a, Ord a) => [a] -> [a] -> a
distanceBetweenLists xs ys = sum [abs (a - b) | (a, b) <- zip (sort xs) (sort ys)]



