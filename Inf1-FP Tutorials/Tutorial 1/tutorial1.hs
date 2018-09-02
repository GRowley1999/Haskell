-- Informatics 1 - Functional Programming 
-- Tutorial 1
--
-- Solutions
--
-- Remember: there are many possible solutions, and if your solution produces
-- the right results, then it is (most likely) correct. However, if your code
-- looks far more complicated than these sample solutions, then you're probably
-- making things too difficult for yourself---try to keep it simple!

import Data.Char
import Data.List
import Test.QuickCheck
import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Int] -> [Int]
halveEvens xs = [x `div` 2 | x <- xs, x `mod` 2 == 0]


-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Int] -> [Int]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)


-- -- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = halveEvens xs == halveEvensReference xs


-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, lo <= x && x <= hi]



-- 3. countPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int
countPositives list = sum [1 | x <- list, x > 0]


-- 4. pennypincher

-- List-comprehension version.
discount :: Int -> Int
discount price = round (0.9 * fromIntegral price)

pennypincher :: [Int] -> Int
pennypincher prices = sum [discount price | price <- prices, discount price <= 19900]

-- -- And the test itself
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = pennypincher xs <= sum [x | x <- xs, x > 0]



-- 5. multDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits str = product [digitToInt c | c <- str, isDigit c]

countDigits :: String -> Int
countDigits str = sum [1 | c <- str, isDigit c]

prop_multDigits :: String -> Bool
prop_multDigits xs = multDigits xs <= 9 ^ countDigits xs


-- 6. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise [] = []
capitalise (c:cs) = toUpper c : [toLower c | c <- cs] 


-- 7. title

lowercase :: String -> String
lowercase xs = [toLower x | x <- xs]

capitaliseLong :: String -> String
capitaliseLong word | length word >= 4 = capitalise word
                    | otherwise       = lowercase word

-- List-comprehension version
title :: [String] -> [String]
title [] = []
title (word:words) = capitalise word : [capitaliseLong word | word <- words]


-- 8. signs

sign :: Int -> Char
sign i | i == 0             = '0'
       | 1 <= i && i <= 9   = '+'
       | -9 <= i && i <= -1 = '-'
       | otherwise          = error "Entered value not within valid ranges!"

signs :: [Int] -> String
signs xs = [sign x | x <- xs, (-9) <= x && x <= 9]

-- 9. score

score :: Char -> Int
score x = counter (isAlpha x) + counter (isUpper x) + counter (x `elem` "AEIOUaeiou")
        where counter x = if x then 1 else 0


totalScore :: String -> Int
totalScore xs = product [score x | x <- xs, isAlpha x]

prop_totalScore_positive :: String -> Bool
prop_totalScore_positive xs = totalScore xs >= 1


-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind letter pos len words = [word | word <- words, length word == len, 0 <= pos, pos <= len, word !! pos == letter]


-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str goal = [pos | (ch, pos) <- zip str [0..], ch == goal]

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = length [ 42 | c <- str, c == goal] == length (search str goal)


-- 10. contains

contains :: String -> String -> Bool
contains str substr = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2 = undefined
