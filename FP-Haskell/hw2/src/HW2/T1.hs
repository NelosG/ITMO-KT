module HW2.T1
  ( -- * Datatypes
    Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
    -- * map functions
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

import Prelude ()

-- | Datatype that contains some value or nothing:
-- - 'None' if there nothing
-- - 'Some' with some value
data Option a
  = None   -- ^ if there nothing
  | Some a -- ^ if there some value

-- | Datatype that contains two values of equivalent type.
data Pair a = P a a

-- | Datatype that contains four values of equivalent type.
data Quad a = Q a a a a

-- | Datatype that contains some value and it's annotation:
-- > value ':#' annotation
infix 0 :#
data Annotated e a = a :# e

-- | Datatype that contains value or error value:
-- - 'Error' errorValue
-- - 'Success' value
data Except e a
  = Error e   -- ^ error with error-value
  | Success a -- ^ successfull result

-- | Datatype that contains value and constructor-specific precedence:
-- - 'Low' value
-- - 'Medium' value
-- - 'High' value
data Prioritised a
  = Low a    -- ^ Low priority
  | Medium a -- ^ Medium priority
  | High a   -- ^ High priority

-- | Datatype that contains an infinite number of values:
infixr 5 :>
data Stream a = a :> Stream a

-- | Datatype that contains value and pointer on tails List:
-- - 'Nil' if there is no value
-- - value ':.' 'List'
infixr 5 :.
data List a
  = Nil         -- ^ there is no element(List ended)
  | a :. List a -- ^ value :. tailPart

-- | Datatype that contains function.
newtype Fun i a = F (i -> a)

-- | Datatype that represents tree
-- contains 'Leaf' is there is no value and does not contain subtrees
-- or 'Branch' with left subtree, value and right subtree.
data Tree a
  = Leaf      -- ^ if there is no value
  | Branch
    (Tree a)  -- ^ left subtree
    a         -- ^ value
    (Tree a)  -- ^ right subtree

-- | Lifts function to 'Option'.
mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f (Some a) = Some (f a)
mapOption _ _        = None

-- | Lifts function to 'Pair'.
mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P a1 a2) = P (f a1) (f a2)

-- | Lifts function to 'Quad'.
mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a1 a2 a3 a4) = Q (f a1) (f a2) (f a3) (f a4)

-- | Lifts function to 'Annotated'.
mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# annotation) = f a :# annotation

-- | Lifts function to 'Except'.
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Success a) = Success (f a)
mapExcept _ (Error err) = Error err

-- | Lifts function to 'Prioritised'.
--
-- Keeping priority.
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f prioritised =
  case prioritised of
    Low a    -> Low (f a)
    Medium a -> Medium (f a)
    High a   -> High (f a)

-- | Lifts function to 'Stream'.
mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (val :> tailPart) = f val :> mapStream f tailPart

-- | Lifts function to 'List'.
mapList :: (a -> b) -> (List a -> List b)
mapList f (head :. tail) =  f head :. mapList f tail
mapList _ _              = Nil

-- | Lifts function to 'Fun'.
mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F fun) = F (\x -> f (fun x))

-- | Lifts function to 'Tree'.
mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f Leaf                         = Leaf
mapTree f (Branch leftSubTree value rightSubTree) =
  Branch (mapTree f leftSubTree) (f value) (mapTree f rightSubTree)
