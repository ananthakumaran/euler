{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Control.Monad
import qualified Data.Set as S

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

main = print $ sum $ notSumOfTwoAbudants
