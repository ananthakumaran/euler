{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -XTupleSections #-}

import           Control.Monad
import qualified Data.Set      as S
import Data.List
import Data.Ord (comparing)

-- helpers
noDivs n fs = foldr (\f r -> f*f > n || (rem n f /= 0 && r)) True fs
primesTD = 2 : 3 : filter (`noDivs` tail primesTD) [5,7..]
isPrime n = n > 1 && noDivs n primesTD

-- non abundant sums (23)

sumOfProperDivisors :: Int -> Int
sumOfProperDivisors n = sum $ filter divides [1..(n `div` 2)]
  where divides x = n `mod` x == 0

abundant :: Int -> Bool
abundant n = (sumOfProperDivisors n) > n

notSumOfTwoAbudants = filter (not . isSumOfTwoAbudant) [1..limit]
  where
    abundants = filter abundant [1..limit]
    sumOfTwoAbudants = liftM2 (+) abundants abundants
    isSumOfTwoAbudant n = S.member n $ S.fromList sumOfTwoAbudants
    limit = 20161


-- quadratic primes
primeLength :: Int -> Int -> Int
primeLength a b =
  let eq x = (x * x) + (a * x) + b
      count eq' n =
        if isPrime (eq' n) then count eq' (n + 1) else n
  in count eq 0

maxQuadraticCoffecients :: Int -> (Int, (Int, Int))
maxQuadraticCoffecients limit = maximumBy (comparing fst) $ map (\(x, y) -> (primeLength x y, (x, y))) $ [-limit..limit] >>= (\x -> map (x, ) [-limit..limit])


main = print $ maxQuadraticCoffecients 1000
