module Assignment1 where

--First function is composed of isPerfect and perfectHelp
--isPerfect tells a user whether the inputed number is a perfect number or not
perfectHelp :: Int->Int->Int
perfectHelp num factor
    | factor == 1 = 1
    | (mod num factor) == 0 = perfectHelp (num) (factor-1) + factor
    | otherwise = perfectHelp (num) (factor-1)

isPerfect :: Int -> Bool
isPerfect x 
    | x < 1 = False
    | perfectHelp x x == (2 * x) = True
    | otherwise = False

--Second function is composed of nthHelp2 and nthTrue
--nthTrue returns the value of the inputed function when num number 
--of trues have been returned by the function
nthHelp2 :: (Int->Bool)->Int->Int->Int->Int
nthHelp2 function x num count
  | num == count = x-1
  | function x == True = nthHelp2 function (x+1) num (count+1)
  | otherwise = nthHelp2 function (x+1) num count


nthTrue :: (Int -> Bool) -> Int -> Int
nthTrue function num 
  | num < 1 = error "nthTrue called with n < 1"
  | otherwise = nthHelp2 function 1 num 0