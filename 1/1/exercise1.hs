toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | (n > 0) = mod n 10 : toDigitsRev (div n 10)
  | otherwise = []

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)
