
{-# LANGUAGE TypeApplications #-}

module Problem2 where

import qualified Data.List.Split as S
import qualified Data.List as L

{-|
Methods are unsafe for simplicity, to just produce the single
int answer required by advent of code
-}

-- | Part 1
unsafeChecksum1 :: FilePath -> IO Int
unsafeChecksum1 f = do
  contents <- readFile f
  return $ sum $ (\line ->
                  let is = read @Int <$> line
                  in (maximum is - minimum is)
               ) <$> (words  <$> lines contents)


-- | Part 2
unsafeChecksum2 :: FilePath -> IO Int
unsafeChecksum2 f = do
  contents <- readFile f
  return $
    sum $
    (\line -> unsafeMod0Elem $ read @Int <$> line) <$>
    (words <$> lines contents)


{-|
[1, 2, 3, 4] -> [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)]
-}
uniquePairs :: [a] -> [(a, a)]
uniquePairs as = [(x, y) | (x: xs) <- L.tails as, y <- xs]

{-|
[9, 8, 4, 7] -> 8/4 = 2
Finds first pair where larger num is divisible by smaller num and divides them

Unsafe due to `head`
-}
unsafeMod0Elem :: [Int] -> Int
unsafeMod0Elem is = let (x, y) = head $ filter (\(x, y) -> if x >= y then mod x y == 0 else mod y x == 0 ) (uniquePairs is)
          in if x >= y then div x y else div y x
