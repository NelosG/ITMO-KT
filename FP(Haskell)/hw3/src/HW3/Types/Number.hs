module HW3.Types.Number
  ( numberBinOperation
  ) where

import Control.Monad.Trans.Except (ExceptT, throwE)
import HW3.Base (HiAction (..), HiError (..), HiMonad, HiValue (..))
import HW3.Helper (CreateF (..), Function (..), getInt)

-- | Evaluates binary operation with 'HiValueNumber'.
numberBinOperation :: HiMonad m => Function -> Rational -> Rational -> ExceptT HiError m HiValue
numberBinOperation Rand a b = do
  aVal <- getInt a
  bVal <- getInt b
  return $ HiValueAction $ HiActionRand aVal bVal
numberBinOperation Range a b = returnHiValue $ Prelude.map createHiValue [a..b]
numberBinOperation fun a b = case fun of
  Mul -> returnHiValue $ a * b
  Add -> returnHiValue $ a + b
  Sub -> returnHiValue $ a - b
  Div
    | b /= 0 -> returnHiValue $ a / b
    | otherwise -> throwE HiErrorDivideByZero
  _   -> throwE HiErrorInvalidArgument



