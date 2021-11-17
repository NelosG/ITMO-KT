module HW1.T3
  ( Tree (..)
  , tFromList
  , tdepth
  , tinsert
  , tmember
  , tsize
  ) where
    
import Data.Foldable (Foldable (foldr'))

-- | Datatype that represents tree
-- contains 'Leaf' is there is no value and does not contain subtrees
-- or 'Branch' with left subtree, value and right subtree.
data Tree a
  = Leaf       -- ^ if there is no value
  | Branch     -- ^ constructor
    (Int, Int) -- ^ ( size of tree, depth)
    (Tree a)   -- ^ left subtree
    a          -- ^ value
    (Tree a)   -- ^ right subtree


-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf                     = 0
tsize (Branch (size, _) _ _ _) = size

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf                      = 0
tdepth (Branch (_, depth) _ _ _) = depth

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ l v r)
  | x == v    = True
  | x > v     = tmember x r
  | otherwise = tmember x l

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList a = tFromListImpl a Leaf

tFromListImpl :: Ord a => [a] -> Tree a -> Tree a
tFromListImpl = flip $ foldr' tinsert

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert key Leaf = mkBranch Leaf key Leaf
tinsert key tree@(Branch _ left value right)
  | key < value = rotate $ mkBranch (tinsert key left) value right
  | key > value = rotate $ mkBranch left value (tinsert key right)
  | otherwise   = tree

-- | Support function.
mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch Leaf value Leaf  = Branch (1, 1) Leaf value Leaf
mkBranch Leaf value right = Branch (tsize right + 1, tdepth right + 1) Leaf value right
mkBranch left value Leaf  = Branch (tsize left + 1, tdepth left + 1) left value Leaf
mkBranch left value right = Branch (tsize left + tsize right + 1, (tdepth right `max` tdepth left) + 1) left value right

-- | Support function.
rotate :: Tree a -> Tree a
rotate tree@(Branch _ (Branch (_, ld) ll _ lr) _ (Branch (_, rd) rl _ rr))
  | (rd - ld) >= 2 && tdepth rl <= tdepth rr = smallLeft tree
  | (rd - ld) >= 2 && tdepth rl > tdepth rr = bigLeft tree
  | (ld - rd) >= 2 && tdepth lr <= tdepth ll = smallRight tree
  | (ld - rd) >= 2 && tdepth lr > tdepth ll = bigRight tree
  | otherwise = tree

rotate tree@(Branch _ (Branch (_, ld) ll _ lr) _ Leaf)
  | ld >= 2 && tdepth lr <= tdepth ll = smallRight tree
  | ld >= 2 && tdepth lr > tdepth ll = bigRight tree
  | otherwise = tree

rotate tree@(Branch _ Leaf _ (Branch (_, rd) rl _ rr))
  | rd >= 2 && tdepth rl <= tdepth rr = smallLeft tree
  | rd >= 2 && tdepth rl > tdepth rr = bigLeft tree
  | otherwise = tree

rotate tree = tree

-- | Support function.
smallLeft :: Tree a -> Tree a
smallLeft (Branch _ l v (Branch _ rl rv rr)) = mkBranch (mkBranch l v rl) rv rr
smallLeft _                                  = undefined

-- | Support function.
bigLeft :: Tree a -> Tree a
bigLeft (Branch _ l v (Branch _ (Branch _ rll rlv rlr) rv rr)) = mkBranch (mkBranch l v rll) rlv (mkBranch rlr rv rr)
bigLeft _                                                      = undefined

-- | Support function.
smallRight :: Tree a -> Tree a
smallRight (Branch _ (Branch _ ll lv lr) v r) = mkBranch ll lv (mkBranch lr v r)
smallRight _                                  = undefined

-- | Support function.
bigRight :: Tree a -> Tree a
bigRight (Branch _ (Branch _ ll lv (Branch _ lrl lrv lrr)) v r) = mkBranch (mkBranch ll lv lrl) lrv (mkBranch lrr v r)
bigRight _                                                      = undefined
