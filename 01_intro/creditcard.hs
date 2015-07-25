{-# OPTIONS_GHC -Wall #-}

validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
sumDigits :: [Integer] -> Integer
sumDigits []    = 0
sumDigits [x]  = sumInt (toDigits x)
sumDigits (x:xs) = sumInt (toDigits x) +  sumDigits(xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []         = []
doubleEveryOther [x0]       = [x0]
doubleEveryOther [x1, x0]   = [x1*2, x0]
doubleEveryOther (x1:x0:xs) = (x1*2) : (x0) : (doubleEveryOther xs)

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0     = (n `mod` 10) : toDigitsRev(n `div` 10)
  | otherwise = []

sumInt :: [Integer] -> Integer
sumInt []     = 0
sumInt [x]    = x
sumInt (x:xs) = x + sumInt(xs)
