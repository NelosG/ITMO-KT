module HW0.T5
  ( Nat
  , nFromNatural
  , nToNum
  , nmult
  , nplus
  , ns
  , nz
  ) where

import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

-- | Constantly returns given value.
--
-- Ignores given function.
nz :: Nat a
nz _ x = x

-- | Creates 'Nat' with function which will be applied twice to the argument.
ns :: Nat a -> Nat a
ns n f x = f $ n f x

-- | Creates 'Nat' with function
-- which will add results of application first arg function and second arg function.
nplus:: Nat a -> Nat a -> Nat a
nplus n m f x = n f (m f x)

-- | Creates 'Nat' with function
-- which will multiplicate results of application first arg function and second arg function.
nmult :: Nat a -> Nat a -> Nat a
nmult n m f = n (m f)

-- | Creates 'Nat' from 'Natural' value.
nFromNatural :: Natural -> Nat a
nFromNatural 0 = \_ x -> x
nFromNatural n = \f x -> nFromNatural (n - 1) f (f x)

-- | Creates 'Natural' from 'Nat' value.
nToNum :: Num a => Nat a -> a
nToNum n = n (+ 1) 0
