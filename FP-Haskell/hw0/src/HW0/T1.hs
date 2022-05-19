{-# LANGUAGE TypeOperators #-}

module HW0.T1
  ( type (<->)(Iso),
    assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

-- | Datatype that stores 2 isomorphic functions.
data a <-> b = Iso (a -> b) (b -> a)

-- | Swaps functions
flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

-- | Returns first function.
runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

-- | Represents distributiveness.
distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib  (Left a)       = (Left a, Left a)
distrib  (Right (b, c)) = (Right b, Right c)

-- | Represents associativity.
assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair  = Iso (\(a, (b, c)) -> 
  ((a, b), c)) (\((a, b), c) -> 
    (a, (b, c)))

-- | Support function.
assocLeft :: Either a (Either b c) -> Either (Either a b) c
assocLeft (Left a)          = Left (Left a)
assocLeft (Right (Left b))  = Left (Right b)
assocLeft (Right (Right c)) = Right c

-- | Support function.
assocRight :: Either (Either a b) c -> Either a (Either b c)
assocRight (Left (Left a))  = Left a
assocRight (Left (Right b)) = Right (Left b)
assocRight (Right c)        = Right (Right c)

-- | Associativity for 'Either'.
assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso assocLeft assocRight
