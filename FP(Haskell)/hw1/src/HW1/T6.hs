module HW1.T6
  ( epart
  , mcat
  ) where

import Data.Either (lefts, rights)
import Data.Maybe (catMaybes)

-- | Concatenate 'List' of 'Maybe'.
mcat :: Monoid a => [Maybe a] -> a
mcat = mconcat . catMaybes

-- | Concatenate 'List' of 'Either'.
epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart x = (mconcat . lefts $ x, mconcat . rights $ x)
