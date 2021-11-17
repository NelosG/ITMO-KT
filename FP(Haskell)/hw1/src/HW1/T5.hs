module HW1.T5
  ( joinWith
  , splitOn
  ) where

import Data.Foldable (Foldable (foldr'))
import Data.List.NonEmpty (NonEmpty ((:|)))

-- | Splits 'List' by given separator.
--
-- Returns 'NonEmpty' 'List'.
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn s = foldr spl ([] :| []) where
  spl cur (x :| xs) =
    if cur == s
    then [] :| (x : xs)
    else (cur : x) :| xs

-- | Joins value with 'NonEmpty' list.
joinWith :: a -> NonEmpty [a] -> [a]
joinWith s (h :| t) = h ++ foldr' (\first second -> s : (first ++ second)) [] t

