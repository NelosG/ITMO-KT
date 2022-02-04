module HW3.Evaluator
  ( eval
  ) where

import Codec.Serialise (serialise)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except (throwE)
import Data.Map (lookup)
import Data.Sequence as DS (Seq (..))
import Data.Text as T (pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime)
import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiMonad (..), HiValue (..))
import HW3.Helper (Arity (..), CreateF (..), Function (..), calcSTimes, getByIndex, slice, wrapInArity)
import HW3.Types.Bytes (bytesUnOperation)
import HW3.Types.Dict (dictUnOperation)
import HW3.Types.List (listUnOperation)
import HW3.Types.Number (numberBinOperation)
import HW3.Types.String (stringUnOperation)

-- | Evaluates 'HiExpr'.
--
-- Returns:
-- - 'HiValue'
-- - 'HiError' if there a failure in evaluation.
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . helper

-- | Auxiliary function to avoid dragging right and left.
helper :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
helper (HiExprValue val) = return val
helper (HiExprDict list) = createHiValue <$> evalListPairs list
helper (HiExprApply fun list) = do
  res <- helper fun
  evalExprList (wrapInArity res) list
helper (HiExprRun forRun) = do
  res <- helper forRun
  case res of
    HiValueAction action -> ExceptT $ fmap Right $ runAction action
    _                    -> throwE HiErrorInvalidArgument

-- | Evaluate list of pairs of 'HiExpr'.
evalListPairs :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m [(HiValue, HiValue)]
evalListPairs [] = return []
evalListPairs ((x, y) : t) = do
      res1 <- helper x
      res2 <- helper y
      next <- evalListPairs t
      pure $ (res1, res2) : next

-- | Evaluate function on list of 'HiExpr'.
evalExprList :: HiMonad m => Arity -> [HiExpr] -> ExceptT HiError m HiValue
evalExprList (BiUnary binary) list@(_ : [_]) = evalExprList (Binary binary) list
evalExprList (BiUnary single) [t]            = evalExprList (Unary single) [t]

evalExprList (AnyCount List) list            = createSeq list Empty
evalExprList (Binary And) (h : [t])          = (flip evalAnd t) =<< helper h
evalExprList (Binary Or) (h : [t])           = (flip evalOr t) =<< helper h
evalExprList (Unary single) [t]              = unaryOperation single =<< helper t
evalExprList (Binary binary) (h : [t])       = do
  res <- (helper h)
  binaryOperation binary res =<< (helper t)

evalExprList (Triple triple) (h : (m : [t])) = do
  res <- (helper h)
  tripleOperation triple res m t

evalExprList (AnyCount NotAFunction) _   = throwE HiErrorInvalidFunction
evalExprList _ _                         = throwE HiErrorArityMismatch

-- | Evaluates \'or\' operation lazily.
evalOr :: HiMonad m => HiValue -> HiExpr -> ExceptT HiError m HiValue
evalOr a b = case a of
  HiValueBool False -> helper b
  HiValueNull       -> helper b
  _                 -> return a

-- | Evaluates \'and\' operation lazily.
evalAnd :: HiMonad m => HiValue -> HiExpr -> ExceptT HiError m HiValue
evalAnd a b = case a of
  HiValueBool False -> return a
  HiValueNull       -> return a
  _                 -> helper b

-- | Creates 'Seq' of 'HiValue' from list of 'HiExpr'.
createSeq :: HiMonad m => [HiExpr] -> (Seq HiValue) -> ExceptT HiError m HiValue
createSeq [] res      = returnHiValue res
createSeq (h : t) res = createSeq t . (res :|>) =<< (helper h)

-- | Applies unary operation.
unaryOperation :: HiMonad m => Function -> HiValue -> ExceptT HiError m HiValue
unaryOperation (String t) (HiValueNumber n)   = getByIndex t n
unaryOperation (Sequence t) (HiValueNumber n) = getByIndex t n
unaryOperation (Bytes t) (HiValueNumber n)    = getByIndex t n
unaryOperation (Dict mp) value                = return $ maybe HiValueNull id $ Data.Map.lookup value mp
unaryOperation fun a                          = evalUn fun a

-- | Evaluates operation with 1 arg (only functions).
evalUn :: HiMonad m => Function -> HiValue -> ExceptT HiError m HiValue
evalUn Serialise s           = returnHiValue $ serialise s
evalUn fun (HiValueBytes b)  = bytesUnOperation fun b
evalUn fun (HiValueString t) = stringUnOperation fun t
evalUn fun (HiValueDict d)   = dictUnOperation fun d
evalUn fun (HiValueList l)   = listUnOperation fun l
evalUn Not (HiValueBool a)   = returnHiValue $ not a
evalUn _ _                   = throwE HiErrorInvalidArgument

-- | Evaluates operation with 2 args
binaryOperation :: HiMonad m => Function -> HiValue -> HiValue -> ExceptT HiError m HiValue
binaryOperation (String t) a b = slice t a b
binaryOperation (Sequence t) a b = slice t a b
binaryOperation (Bytes t) a b = slice t a b
binaryOperation Write str (HiValueString b)   = binaryOperation Write str (createHiValue $ encodeUtf8 b)
binaryOperation Write (HiValueString t) (HiValueBytes b)   = return $ HiValueAction $ HiActionWrite (T.unpack t) b
binaryOperation fun a b = calcBin fun a b

-- | Evaluates operation with 2 args (only functions).
calcBin :: HiMonad m => Function -> HiValue -> HiValue -> ExceptT HiError m HiValue
calcBin Add (HiValueTime a) (HiValueNumber b) = returnHiValue $ addUTCTime (fromRational b) a
calcBin Sub (HiValueTime a) (HiValueTime b)   = returnHiValue $ diffUTCTime a b

calcBin Fold _ (HiValueList Empty)       = return $ HiValueNull
calcBin Fold fun (HiValueList (h :<| t)) = case (wrapInArity fun) of
  Binary f  -> foldList f h t
  BiUnary f -> foldList f h t
  _         -> throwE HiErrorInvalidFunction

calcBin LessThan a b       = returnHiValue $ a < b
calcBin GreaterThan a b    = returnHiValue $ a > b
calcBin NotLessThan a b    = returnHiValue $ a >= b
calcBin NotGreaterThan a b = returnHiValue $ a <= b
calcBin Equals a b         = returnHiValue $ a == b
calcBin NotEquals a b      = returnHiValue $ a /= b

calcBin Add (HiValueString a) (HiValueString b) = returnHiValue $ a <> b
calcBin Add (HiValueList a) (HiValueList b)     = returnHiValue $ a <> b
calcBin Add (HiValueBytes a) (HiValueBytes b)   = returnHiValue $ a <> b
calcBin Div (HiValueString a) (HiValueString b) = returnHiValue $ a <> (T.pack "/") <> b

calcBin Mul (HiValueString a) (HiValueNumber b) = calcSTimes a b
calcBin Mul (HiValueList a) (HiValueNumber b)   = calcSTimes a b
calcBin Mul (HiValueBytes a) (HiValueNumber b)  = calcSTimes a b

calcBin fun (HiValueNumber a) (HiValueNumber b) = numberBinOperation fun a b
calcBin _ _ _                                   = throwE HiErrorInvalidArgument

-- | Evaluates operation with 3 args
tripleOperation :: HiMonad m => Function -> HiValue -> HiExpr -> HiExpr -> ExceptT HiError m HiValue
tripleOperation If (HiValueBool a) b c = if a then helper b else helper c
tripleOperation _ _ _ _                = throwE HiErrorInvalidArgument

-- | Folds list.
foldList :: HiMonad m => Function -> HiValue -> Seq HiValue -> ExceptT HiError m HiValue
foldList _ val Empty       = return val
foldList fun val (h :<| t) = do
  ans <- evalExprList (Binary fun) [(HiExprValue val), (HiExprValue h)]
  foldList fun ans t
