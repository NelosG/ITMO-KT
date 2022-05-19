module HW0.T3
  ( compose
  , contract
  , i
  , k
  , permute
  , s
  ) where

-- | From the B, C, K, W system (variant of combinatory logic).
--
-- S = B (B (B W) C) (B B) = B (B W) (B B C)
s :: (a -> b -> c) -> (a -> b) -> (a -> c)
s f g x = f x (g x)

-- | K from the B, C, K, W system (variant of combinatory logic).
k :: a -> b -> a
k x _ = x

-- | From the B, C, K, W system (variant of combinatory logic).
--
-- I = W K
i :: a -> a
i = s k k

-- | B from the B, C, K, W system (variant of combinatory logic).
--
-- B = S (K S) K
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose = s (k s) k

-- | W from the B, C, K, W system (variant of combinatory logic).
--
-- W = S S (S K)
contract :: (a -> a -> b) -> (a -> b)
contract = s s (s k)

-- | C from the B, C, K, W system (variant of combinatory logic).
--
-- C = S (S (K (S (K S) K)) S) (K K)
permute :: (a -> b -> c) -> (b -> a -> c)
permute = s (s (k compose) s) (k k)
