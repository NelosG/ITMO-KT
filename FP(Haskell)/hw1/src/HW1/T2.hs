module HW1.T2
  ( N (..)
  , nEven
  , nFromNatural
  , nOdd
  , nToNum
  , ncmp
  , ndiv
  , nmod
  , nmult
  , nplus
  , nsub
  ) where

import GHC.Natural (Natural)

-- | Datatype that represents.
data N = Z | S N

-- | Addition.
nplus :: N -> N -> N
nplus a Z     = a
nplus Z a     = a
nplus a (S b) = S $ nplus a b

-- | Multiplication.
nmult :: N -> N -> N
nmult _ Z     = Z
nmult Z _     = Z
nmult (S Z) a = a
nmult a (S Z) = a
nmult a (S b) = nplus a $ nmult a b

-- | Subtraction (Nothing if result is negative).
nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) = nsub a b

-- | Comparison (Do not derive 'Ord').
ncmp :: N -> N -> Ordering
ncmp n m = case nsub n m of
  Just Z  -> EQ
  Nothing -> LT
  _       -> GT

-- | Converts 'Natural' to 'N'.
nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural a = S $ nFromNatural $ a - 1

-- | Converts 'N' to 'Natural'.
nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S a) = nToNum a + 1

-- | Parity checking
nEven, nOdd :: N -> Bool
nEven a = (==) EQ $ ncmp Z $ nmod a $ S $ S Z
nOdd a = not $ nEven a

-- | Integer division
ndiv :: N -> N -> N
ndiv a = ndivImpl $ Just a

-- | Support function.
ndivImpl :: Maybe N -> N -> N
ndivImpl Nothing _ = undefined
ndivImpl (Just a) b = case ncmp a b of
  EQ -> S Z
  LT -> Z
  GT -> S $ ndivImpl (nsub a b) b

-- | Modulo operation
nmod :: N -> N -> N
nmod a = nmodImpl (Just a)

-- | Support function.
nmodImpl :: Maybe N -> N -> N
nmodImpl Nothing _ = undefined
nmodImpl (Just a) b = case ncmp a b of
  EQ -> Z
  LT -> a
  GT -> nmodImpl (nsub a $ nmult b $ ndiv a b) b
