module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some))
import HW2.T2 (distFun, (+=))

-- | Unwraps double wrapped in 'Option' value to wrapped in 'Option' value.
joinOption :: Option (Option a) -> Option a
joinOption (Some a) = a
joinOption None     = None

-- | Unwraps double wrapped in 'Except' value to wrapped in 'Except' value.
joinExcept :: Except e (Except e a) -> Except e a
joinExcept wraped =
  case wraped of
    Error err     -> Error err
    Success (inh) -> inh

-- | Unwraps double wrapped in 'Except' value to wrapped in 'Except' value,
-- concats annotations (first will be top wrapper annotation).
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# inhAnn) :# ann) = a :# (ann <> inhAnn)

-- | Flattens a List of Lists.
joinList :: List (List a) -> List a
joinList  wraped =
  case wraped of
    a   :. tail -> a += joinList tail
    Nil         -> Nil

-- | Unwraps double wrapped in 'Fun' function to wrapped in 'Fun' function.
joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F fun) = F $ fun >>= (\(F g) -> g)
