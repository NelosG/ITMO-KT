{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module HW3.Helper
  ( Arity (..)
  , CreateF (..)
  , Function (..)
  , MyType (..)
  , calcSTimes
  , countF
  , getByIndex
  , slice
  , wrapInArity
  , getInt
  , jointOperation
  ) where

import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Except (throwE)
import Data.ByteString as BS (ByteString, drop, index, length, pack, reverse, take, unpack)
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import Data.ByteString.Lazy.Char8 (toStrict)
import qualified Data.Foldable
import Data.Map as DM (Map, fromList, fromListWith, map)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence as DS (Seq, drop, fromList, index, length, reverse, take)
import Data.Text as T (Text, drop, index, length, pack, reverse, take, unpack)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import HW3.Base (HiError (..), HiFun (..), HiMonad, HiValue (..))

-- | A helper type that determines the number of arguments to a function.
data Arity
  = Unary Function
  | Binary Function
  | Triple Function
  | BiUnary Function
  | AnyCount Function

-- | A class that combines several simple functions of different types to avoid copy-paste.
class MyType a where
    lengthF     :: a -> Rational
    lengthF = toRational . lengthI
    lengthI     :: a -> Int
    indexF      :: a -> Int -> HiValue
    takeF       :: Int -> a -> a
    dropF       :: Int -> a -> a
    reverseF    :: a -> a
    toListF     :: a -> [HiValue]

-- | Class to create 'HiValue'
class CreateF a where
  createHiValue :: a -> HiValue
  returnHiValue :: HiMonad m => a -> ExceptT HiError m HiValue
  returnHiValue = return . createHiValue

instance {-# OVERLAPPABLE #-} Real a => CreateF a where
  createHiValue = HiValueNumber . toRational

instance CreateF [Word8] where
  createHiValue = createHiValue . BS.pack

instance CreateF HiValue where
  createHiValue = id

instance CreateF HiFun where
  createHiValue = HiValueFunction

instance CreateF Char where
  createHiValue a = createHiValue [a]

instance CreateF String where
  createHiValue = createHiValue . T.pack

instance CreateF Text where
  createHiValue = HiValueString

instance CreateF [HiValue] where
  createHiValue = createHiValue . DS.fromList

instance CreateF (Seq HiValue) where
  createHiValue = HiValueList

instance CreateF Data.ByteString.Lazy.ByteString where
  createHiValue = createHiValue . toStrict

instance CreateF Data.ByteString.ByteString where
  createHiValue = HiValueBytes

instance CreateF UTCTime where
  createHiValue = HiValueTime

instance CreateF [(HiValue, HiValue)] where
  createHiValue = createHiValue . DM.fromList

instance CreateF (Map HiValue HiValue) where
  createHiValue = HiValueDict

instance CreateF Bool where
  createHiValue = HiValueBool

instance MyType Text where
    lengthI = T.length
    indexF t ind = createHiValue $ T.index t ind
    takeF  = T.take
    dropF = T.drop
    reverseF = T.reverse
    toListF t = fmap createHiValue (T.unpack t)

instance MyType (Seq HiValue) where
    lengthI = DS.length
    indexF = DS.index
    takeF  = DS.take
    dropF = DS.drop
    reverseF = DS.reverse
    toListF t = Data.Foldable.toList t

instance MyType ByteString where
    lengthI = BS.length
    indexF s ind = createHiValue $ BS.index s ind
    takeF  = BS.take
    dropF = BS.drop
    reverseF = BS.reverse
    toListF b = fmap createHiValue (BS.unpack b)

-- | A helper type that contains everything that can be used as a function.
data Function
  = NotAFunction
  | Div
  | Mul
  | Add
  | Sub
  | And
  | Or
  | LessThan
  | GreaterThan
  | Equals
  | NotLessThan
  | NotGreaterThan
  | NotEquals
  | Fold
  | Range
  | Not
  | Length
  | ToUpper
  | ToLower
  | Reverse
  | Trim
  | List
  | If
  | String Text
  | Sequence (Seq HiValue)
  | PackBytes
  | UnpackBytes
  | Encode
  | Decode
  | Zip
  | Unzip
  | Serialise
  | Deserialise
  | Bytes ByteString
  | Read
  | Write
  | MkDir
  | ChDir
  | Time
  | Rand
  | Echo
  | Dict (Map HiValue HiValue)
  | Count
  | Keys
  | Values
  | Invert

-- | Wraps in support type 'Arity'.
wrapInArity :: HiValue -> Arity
wrapInArity (HiValueFunction fun) = case fun of
  HiFunNot            -> Unary Not
  HiFunLength         -> Unary Length
  HiFunToUpper        -> Unary ToUpper
  HiFunToLower        -> Unary ToLower
  HiFunReverse        -> Unary Reverse
  HiFunTrim           -> Unary Trim
  HiFunPackBytes      -> Unary PackBytes
  HiFunUnpackBytes    -> Unary UnpackBytes
  HiFunZip            -> Unary Zip
  HiFunUnzip          -> Unary Unzip
  HiFunSerialise      -> Unary Serialise
  HiFunDeserialise    -> Unary Deserialise
  HiFunEncodeUtf8     -> Unary Encode
  HiFunDecodeUtf8     -> Unary Decode
  HiFunRead           -> Unary Read
  HiFunMkDir          -> Unary MkDir
  HiFunChDir          -> Unary ChDir
  HiFunParseTime      -> Unary Time
  HiFunEcho           -> Unary Echo
  HiFunCount          -> Unary Count
  HiFunKeys           -> Unary Keys
  HiFunValues         -> Unary Values
  HiFunInvert         -> Unary Invert
  HiFunDiv            -> Binary Div
  HiFunMul            -> Binary Mul
  HiFunAdd            -> Binary Add
  HiFunSub            -> Binary Sub
  HiFunAnd            -> Binary And
  HiFunOr             -> Binary Or
  HiFunLessThan       -> Binary LessThan
  HiFunGreaterThan    -> Binary GreaterThan
  HiFunEquals         -> Binary Equals
  HiFunNotLessThan    -> Binary NotLessThan
  HiFunNotGreaterThan -> Binary NotGreaterThan
  HiFunNotEquals      -> Binary NotEquals
  HiFunFold           -> Binary Fold
  HiFunRange          -> Binary Range
  HiFunWrite          -> Binary Write
  HiFunRand           -> Binary Rand
  HiFunIf             -> Triple If
  HiFunList           -> AnyCount List

wrapInArity (HiValueString text) = BiUnary $ String text
wrapInArity (HiValueList s)      = BiUnary $ Sequence s
wrapInArity (HiValueBytes s)     = BiUnary $ Bytes s
wrapInArity (HiValueDict mp)     = Unary $ Dict mp
wrapInArity _                    = AnyCount NotAFunction

-- | Returns n concatenated copies of 'MyType'.
calcSTimes :: (HiMonad m, Semigroup a, CreateF a) => a -> Rational -> ExceptT HiError m HiValue
calcSTimes a b = do
  ind <- getInt b
  if ind > 0
  then returnHiValue $ stimes ind a
  else throwE HiErrorInvalidArgument

-- | Returns slice of 'MyType' by given 'HiValues'.
slice :: (HiMonad m, MyType a, CreateF a) => a -> HiValue -> HiValue -> ExceptT HiError m HiValue
slice t (HiValueNumber a) (HiValueNumber b) = doSlice t a b
slice t HiValueNull (HiValueNumber b)       = doSlice t 0 b
slice t (HiValueNumber a) HiValueNull       = doSlice t a $ lengthF t
slice t HiValueNull HiValueNull             = doSlice t 0 $ lengthF t
slice _ _ _                                 = throwE HiErrorInvalidArgument

-- | Returns slice of 'MyType' by given numbers.
doSlice :: (HiMonad m, MyType a, CreateF a) => a -> Rational -> Rational -> ExceptT HiError m HiValue
doSlice t a b = do
  leftI <- getInt a
  rightI <- getInt b
  let left  = fixIndex t leftI
      right = fixIndex t rightI
  returnHiValue $ takeF (right - left) $ dropF left t

-- | Fix below zero inexes.
fixIndex :: (MyType a) => a -> Int -> Int
fixIndex t a
  | a < 0 = lengthI t + a
  | otherwise = a

-- | Returns element by index.
getByIndex ::(MyType a, HiMonad m) => a -> Rational -> ExceptT HiError m HiValue
getByIndex t n = do
  ind <- getInt n
  if ind >= 0 && ind < lengthI t
  then return $ indexF t ind
  else return $ HiValueNull

-- | Evaluates Count operation on 'MyType'.
countF :: (MyType a) => a -> HiValue
countF t = createHiValue $ DM.map HiValueNumber $
  DM.fromListWith (+) pairs
    where pairs = [(k, 1) | k <- toListF t]

-- | Returns pair with 'Bool' in the second arg that men is given number an 'Int' or not,
-- and result in first if second is true
-- or undefined if second is false (to protect this value from use)
getInt :: HiMonad m => Rational -> ExceptT HiError m Int
getInt rat =  case (numerator rat, denominator rat) of
  (ind, 1)
    | toInteger (minBound::Int) <= ind && ind <= toInteger (maxBound::Int) -> return $ fromInteger ind
    | otherwise -> throwE HiErrorInvalidArgument
  _ -> throwE HiErrorInvalidArgument

-- | Some joint operations.
jointOperation :: (HiMonad m, MyType a, CreateF a) => Function -> a -> ExceptT HiError m HiValue
jointOperation Length t  = returnHiValue $ lengthF t
jointOperation Reverse t = returnHiValue $ reverseF t
jointOperation Count t   = return $ countF t
jointOperation _ _       = throwE HiErrorInvalidArgument