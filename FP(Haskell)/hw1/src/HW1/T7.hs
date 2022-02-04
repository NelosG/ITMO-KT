module HW1.T7
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

-- | Represents 'Nonempty' 'List'.
data ListPlus a =
  a :+ ListPlus a  -- ^ element and tail
  | Last a         -- ^ last element
infixr 5 :+

instance Semigroup (ListPlus a) where
  (<>) (Last a) c = a :+ c
  (<>) (a :+ b) c = a :+ b <> c

-- | Something like 'Either'.
data Inclusive a b =
  This a
  | That b
  | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  (<>) (This a) (This b)     = This (a <> b)
  (<>) (That a) (That b)     = That (a <> b)
  (<>) (This a) (That b)     = Both a b
  (<>) (That a) (This b)     = Both b a
  (<>) (Both a b) (This c)   = Both (a <> c) b
  (<>) (Both a b) (That c)   = Both a (b <> c)
  (<>) (This a) (Both b c)   = Both (a <> b) c
  (<>) (That a) (Both b c)   = Both b (a <> c)
  (<>) (Both a b) (Both c d) = Both (a <> c) (b <> d)

-- | Wrapped 'String'.
newtype DotString = DS String

instance Semigroup DotString where
  (<>) (DS "") a     = a
  (<>) a (DS "")     = a
  (<>) (DS a) (DS b) = DS $ a ++ ('.' : b)

instance Monoid DotString where
    mempty = DS ""

-- | Wrapped function.
newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
    (<>) (F f) (F g) = F $ f . g

instance Monoid (Fun a) where
    mempty = F id
