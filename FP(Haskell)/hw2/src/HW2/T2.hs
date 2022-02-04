module HW2.T2
  ( -- * dist functions
    distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
    -- * wrap functions
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
    -- * support functions
  , (+=)
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some), Pair (P), Prioritised (High, Low, Medium), Quad (Q),
               Stream ((:>)))
import Prelude (Monoid, Semigroup, mempty, (<>))

-- | Collects pair of 'Option' values to 'Option' of pairs.
-- > (Some 1, Some 2) -> Some (1, 2)
distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption _                = None

-- | Collects pair of 'Pair' values to 'Pair' of pairs.
distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

-- | Collects pair of 'Quad' values to 'Quad' of pairs.
distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

-- | Collects pair of wrapped in 'Annotated' values to wrapped in 'Annotated' pair
-- with concated annotation.
distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (aValue :# aAnnotation, bValue :# bAnnotation) =
  (aValue, bValue) :# (aAnnotation <> bAnnotation)

-- | Collects pair of 'Except' values to 'Except' of pairs.
distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept pair =
  case pair of
    (Error aErr, _)        -> Error aErr
    (_, Error bErr)        -> Error bErr
    (Success a, Success b) -> Success (a, b)

-- | Collects pair of 'Prioritised' values to 'Prioritised' of pairs
-- with the highest priority of the priorities of the given pair.
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised pair =
  case pair of
    (Low a,    Low b)    -> Low (a, b)
    (Low a,    Medium b) -> Medium (a, b)
    (Medium a, Low b)    -> Medium (a, b)
    (Medium a, Medium b) -> Medium (a, b)
    (High a,   Low b)    -> High (a, b)
    (Low a,    High b)   -> High (a, b)
    (High a,   Medium b) -> High (a, b)
    (Medium a, High b)   -> High (a, b)
    (High a,   High b)   -> High (a, b)

-- | Collects pair 'Stream' to 'Stream' of pairs.
distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (aVal :> aTail, bVal :> bTail) = (aVal, bVal) :> distStream (aTail, bTail)

-- | Concats two 'List'.
infixr 5 +=
(+=) :: List a -> List a -> List a
(+=) (a :. aTail) b = a :. (aTail += b)
(+=) Nil b          = b

-- | Collects pair of wrapped in 'List' values to wrapped in 'List' pair
-- Result List size is n x m where
--
-- - n - size of first 'List'
-- - m - size of second 'List'
--
-- Result 'List' contains pairs in the following order:
-- - first 'List' first value with all values of second 'List',
-- - first 'List' second value with all values of second 'List'
-- - ...
distList :: (List a, List b) -> List (a, b)
distList (head :. tail, seconList) = combineElement head seconList += distList (tail, seconList)
  where
    combineElement :: a -> List b -> List (a, b)
    combineElement value (head :. tail) = (value, head) :. combineElement value tail
    combineElement _ _                  = Nil
distList _                         = Nil

-- | Collects pair of wrapped in 'Fun' functions to wrapped in 'Fun' function
-- that receives pair and returns pair.
distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F aF, F bF) = F (\x -> (aF x, bF x))

-- | Wraps a value in 'Option' (Some value).
wrapOption :: a -> Option a
wrapOption = Some

-- | Wraps a value in 'Pair' (two equivalent values).
wrapPair :: a -> Pair a
wrapPair a = P a a

-- | Wraps a value in 'Quad' (four equivalent values).
wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

-- | Wraps a value in 'Annotated' with empty annotation.
wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

-- | Wraps a value in 'Except':
-- > 'Success' value
wrapExcept :: a -> Except e a
wrapExcept = Success

-- | Wraps a value in 'Prioritised' with low priority:
-- > 'Low' value
wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

-- | Wraps a value in 'Stream' ('Stream' of equivalent values).
wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

-- | Wraps a value in 'List' of the one element:
-- > value ':.' 'Nil'
wrapList :: a -> List a
wrapList a = a :. Nil

-- | Wraps a value in 'Fun' (inside function that ignores input and returns ours value).
wrapFun :: a -> Fun i a
wrapFun a = F (\x -> a)
