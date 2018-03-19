{-# LANGUAGE OverloadedStrings #-}

module Problem4 where

import qualified Data.Text as T
import qualified Data.List.Split as S
import qualified Data.HashMap.Strict as M
import qualified Data.List as L

{-|
Methods are unsafe for simplicity, to just produce the single
int answer required by advent of code
-}

-- | Part 1
numValidPhrases1 :: FilePath -> IO Int
numValidPhrases1 f = (numUniqueElemPhrases . lines) <$> readFile f

numUniqueElemPhrases :: [String] -> Int
numUniqueElemPhrases =
  sum .
  fmap
    (\phrase ->
       if M.null $
          M.filter (> 1) $
          foldl (\m s -> M.insertWith (+) s 1 m) M.empty (S.splitOn " " phrase)
         then 1
         else 0)

test :: Int
test = let x = 2
           y = x + 2
       in x + y + z + a
       where z = 4
             a = z + 2

-- | Part 2
numValidPhrases2 :: FilePath -> IO Int
numValidPhrases2 f = (numNoPermElemPhrases . lines) <$> readFile f


numNoPermElemPhrases :: [String] -> Int
numNoPermElemPhrases =
  sum .
  fmap
    (\phrase ->
       let elems = S.splitOn " " phrase
           l = length elems
           x =
             sum $
             M.elems $
             foldl
               (\m elem ->
                  let elem' = L.sort elem
                  in if M.member elem' m
                       then m
                       else M.insertWith (+) elem' 1 m)
               M.empty
               elems
       in if x == l
            then 1
            else 0)
