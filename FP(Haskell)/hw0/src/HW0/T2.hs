module HW0.T2
  ( Not
  , doubleNeg
  , reduceTripleNeg
  ) where

import Data.Void (Void)

type Not a = a -> Void

-- | Creates value that double wrapped in 'Not'.
doubleNeg :: a -> Not (Not a)
doubleNeg a f = f a

-- | Reduces triple negation.
reduceTripleNeg :: Not (Not (Not a)) -> Not a
reduceTripleNeg f = f . doubleNeg