module HW2.T5
  ( -- * Datatypes
    EvaluationError (..)
  , ExceptState (..)
    -- * map functions
  , eval
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , throwExceptState
  , wrapExceptState
  ) where

import qualified Control.Monad
import HW2.T1 (Annotated (..), Except (..), mapAnnotated, mapExcept)
import HW2.T4 (Expr (..), Prim (..), calc, evalM)

-- | State with exception to write in iperative style.
data ExceptState e s a = ES
  { runES :: s -> Except e (Annotated s a) -- ^ function that runs ExceptState
  }

-- | Lifts function to 'ExceptState'.
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f a = ES $ \x -> mapExcept (mapAnnotated f) $ runES a x

-- | Wraps a value in 'ExceptState'.
--
-- @runES 'ExceptState' s@ will return given value annotated with s
wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \x -> Success (a :# x)

-- | Runs nested 'ExceptState' on result of external if result is successfull
-- or just thow error.
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState a = ES $ \x ->
  case runES a x of
    Success (val :# def) -> runES val def
    Error error          -> Error error

-- | Change annotation in 'ExceptState' with given function.
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \x -> Success (() :# f x)

-- | Throws error with specified error-value.
throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \x -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

-- | Error thowned whent something gone wrong.
data EvaluationError = DivideByZero -- ^ Throwned when we try to divide by zero.

-- | Evaluate expression.
--
-- Returns result of evaluation annotated with list of performed operation wrapped in 'Success'
-- or an 'Error' with 'EvaluationError'.
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval expr = evalM expr $ \evOp -> do
  case evOp of
    Div _ 0 -> throwExceptState DivideByZero
    _       -> modifyExceptState $ (:) evOp
  pure $ calc evOp
