module HW2.T4
  ( -- * Datatypes
    Expr (..)
  , Prim (..)
  , State (..)
    -- * map functions
  , eval
  , joinState
  , mapState
  , modifyState
  , wrapState
    -- * support functions
  , calc
  , evalM
  ) where
import qualified Control.Monad
import HW2.T1 (Annotated (..), mapAnnotated)

-- | State to write in iperative style.
data State s a = S
  { runS :: s -> Annotated s a -- ^ function that runs State
  }

-- | Lifts function to 'State'.
mapState :: (a -> b) -> State s a -> State s b
mapState f a = S $ \x -> mapAnnotated f $ runS a x

-- | Wraps a value in 'State'.
--
-- @runS 'State' s@ will return given value annotated with s.
wrapState :: a -> State s a
wrapState a = S (a :#)

-- | Runs nested 'State' on result of external.
joinState :: State s (State s a) -> State s a
joinState a = S $ \x ->
  case runS a x of
    val :# def -> runS val def

-- | Change annotation in 'State' with given function.
modifyState :: (s -> s) -> State s ()
modifyState f = S $ \x -> () :# f x

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

-- | Represents operation.
data Prim a
  = Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum

-- | Represents expression.
data Expr
  = Val Double       -- ^ just value
  | Op (Prim Expr) -- ^ opertion on two expression

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

-- | Evaluate expression.
--
-- Returns result of evaluation annotated with list of performed operation.
eval :: Expr -> State [Prim Double] Double
eval expr = evalM expr $ \evOp -> do
  modifyState $ (:) evOp
  pure $ calc evOp

-- | Support function.
calc :: (Fractional f) => Prim f -> f
calc (Add a b) = a + b
calc (Sub a b) = a - b
calc (Mul a b) = a * b
calc (Div a b) = a / b
calc (Abs a)   = abs a
calc (Sgn a)   = signum a

-- | Support function.
evalM :: (Monad m, Fractional f) => Expr -> (Prim f -> m f) -> m f
evalM op f =
  case op of
    Op (Add a b) -> supportBinary Add a b f
    Op (Sub a b) -> supportBinary Sub a b f
    Op (Mul a b) -> supportBinary Mul a b f
    Op (Div a b) -> supportBinary Div a b f
    Op (Abs a)   -> supportUnary Abs a f
    Op (Sgn a)   -> supportUnary Sgn a f
    Val a        -> pure $ realToFrac a

-- | Support function.
supportUnary :: (Monad m, Fractional f) => (f -> Prim f) -> Expr -> (Prim f -> m f) -> m f
supportUnary constructor a f = evalM a f >>= \x -> f $ constructor x

-- | Support function.
supportBinary :: (Monad m, Fractional f) => (f -> f -> Prim f) -> Expr -> Expr -> (Prim f -> m f) -> m f
supportBinary constructor a b f = do
  resA <- evalM a f
  resB <- evalM b f
  f (constructor resA resB)
