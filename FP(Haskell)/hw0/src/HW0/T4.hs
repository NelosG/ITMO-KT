module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import Data.Function (fix)
import GHC.Natural (Natural)

-- | Computes the factorial
fac :: Natural -> Natural
fac = fix (\f accamulator n ->
  if n == 0
  then accamulator
  else f (accamulator * n) (n - 1)) 1

-- | Computes the n-th Fibonacci number.
fib 0 = 0
fib 1 = 1
fib n | even n         = f1 * (f1 + 2 * f2)
      | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
      | otherwise      = (2 * f1 + f2) * (2 * f1 - f2) - 2
   where k = n `div` 2
         f1 = fib k
         f2 = fib (k-1)

-- | Behaves like Data.List.map
map' :: (a -> b) -> [a] -> [b]
map' = fix $ \f mapper list ->
  case list of
    []     -> []
    x : xs -> mapper x : f mapper xs

-- | Behaves like Data.List.repeat
repeat' :: a -> [a]
repeat' x = fix (x :)
