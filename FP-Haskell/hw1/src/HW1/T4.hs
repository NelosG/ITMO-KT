module HW1.T4
  ( tfoldr
  , treeToList
  ) where

import HW1.T3 (Tree (Leaf, Branch))

-- | 'foldr' for 'Tree'.
tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ curList Leaf                = curList
tfoldr conc curList (Branch _ l v r) = tfoldr conc (conc v $ tfoldr conc curList r) l

-- | Creates sorted '[]' from 'Tree'.
treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
