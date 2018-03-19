
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Problem1 where

import qualified Data.List.Split as S
import qualified Data.Text as T
import qualified Data.Vector as V

{-|
Methods are unsafe for simplicity, to just produce the single
int answer required by advent of code
-}

-- | Part 1
unsafeCaptcha1 :: FilePath -> IO Int
unsafeCaptcha1 f = (unsafeCompute1 . head . lines) <$> (readFile f)


unsafeCompute1 :: String -> Int
unsafeCompute1 line =
  foldl
    (\i (x1, x2) ->
       if x1 == x2
         then i + x1
         else i)
    0
    (zip (x : xs) (xs ++ [x]))
  where
    (x:xs) = read @Int <$> tail (S.splitOn "" line)


-- | Part 2
unsafeCaptcha2 :: FilePath -> IO Int
unsafeCaptcha2 f = (unsafeCompute2 . head . lines) <$> (readFile f)


unsafeCompute2 :: String -> Int
unsafeCompute2 line =
  foldl
    (\i (x1, x2) ->
       if x1 == x2
         then i + x1 + x1
         else i)
    0
    (V.zip firstHalf lastHalf)
  where
    v = V.fromList $ read @Int <$> tail (S.splitOn "" line)
    l = V.length v
    firstHalf = V.unsafeSlice 0 (div l 2) v
    lastHalf = V.unsafeSlice (div l 2) (div l 2) v
