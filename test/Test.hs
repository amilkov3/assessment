
{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import System.IO.Unsafe
import Problem1
import Problem2
import Problem4

main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit and IT tests" [unitTests, integrationTests]

assertErr1 = "Captchas solution does not match"
assertErr2 = "Checksum solution does not match"
assertErr4 = "Num valid phrases does not match"


unitTests = testGroup "Unit tests"
  [
    testCase "Compute captcha part1 correctly" $
    assertEqual assertErr1 9 (compute1 "91212129")
  ,
    testCase "Compute captcha part2 correctly" $
    assertEqual assertErr1 4 (compute2 "12131415")
  ,

    testCase "Compute unique pairs" $
    assertEqual
    "Unique pairs do not match expected" [(1, 2), (1, 3), (1, 4), (2, 3), (2, 4), (3, 4)] (uniquePairs [1, 2, 3, 4])
  ,
    testCase "Divide first pair that is divisible" $
    assertEqual "Divided pair does not match expected" 2 (unsafeMod0Elem [9, 8, 4, 7])
  ,

    testCase "Compute num valid phrases part1 correctly" $
    assertEqual assertErr4 2 (numUniqueElemPhrases ["aa bb cc dd ee", "aa bb cc dd aa", "aa bb cc dd aaa"])
  ,
    testCase "Compute num valid phrases part2 correctly" $
    assertEqual assertErr4 3 (numNoPermElemPhrases [
                                 "abcde fghij"
                               , "abcde xyz ecdab"
                               , "a ab abc abd abf abj"
                               , "iiii oiii ooii oooi oooo"
                               , "oiii ioii iioi iiio"
                               ])
  ]


integrationTests = testGroup "Integration tests"
  [
    testCase "Compute captcha from file part1 correctly " $
    assertEqual assertErr1 1203 (unsafePerformIO $ unsafeCaptcha1 "src/input1")
  ,
    testCase "Compute catcha from file part2 correctly" $
    assertEqual assertErr1 1146 (unsafePerformIO $ unsafeCaptcha2 "src/input1")
  ,

    testCase "Compute checksum from file part1 correctly" $
    assertEqual assertErr2 58975 (unsafePerformIO $ unsafeChecksum1 "src/input2")
  ,
    testCase "Compute checksum from file part2 correctly" $
    assertEqual assertErr2 308 (unsafePerformIO $ unsafeChecksum2 "src/input2")
  ,

    testCase "Compute num valid phrases from file part1 correctly" $
    assertEqual assertErr4 477 (unsafePerformIO $ numValidPhrases1 "src/input4")
  ,
    testCase "Compute num valid phrases from file part2 correctly" $
    assertEqual assertErr4 167 (unsafePerformIO $ numValidPhrases2 "src/input4")

  ]
