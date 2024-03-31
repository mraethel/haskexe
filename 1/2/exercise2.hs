toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | (n > 0) = mod n 10 : toDigitsRev (div n 10)
  | otherwise = []

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = x:[]
doubleEveryOtherRev (x:y:z) = (x:2*y:doubleEveryOtherRev z)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther z = reverse (doubleEveryOtherRev (reverse z))
