{-# LANGUAGE DeriveGeneric #-}
module HW3.Base
  ( HiAction (..)
  , HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiMonad (..)
  , HiValue (..)
  ) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- | Evaluation errors (invalid arguments, ...)
data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Eq, Ord, Show)

-- | Actions that do something with
data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Eq, Ord, Show, Generic)

-- | function names (e.g. div, sort, length, ...)
data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunNot
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Ord, Eq, Generic, Enum)

-- | Expressions (literals, function calls, ...)
data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Eq, Ord, Show)

-- | Values (numbers, booleans, strings, ...)
data HiValue
  = HiValueBool Bool
  | HiValueNumber Rational
  | HiValueFunction HiFun
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Eq, Ord, Show, Generic)

instance Serialise HiValue
instance Serialise HiFun
instance Serialise HiAction

-- | Class to run actions
class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

instance Show HiFun where
    show HiFunDiv            = "div"
    show HiFunMul            = "mul"
    show HiFunAdd            = "add"
    show HiFunSub            = "sub"
    show HiFunNot            = "not"
    show HiFunAnd            = "and"
    show HiFunOr             = "or"
    show HiFunLessThan       = "less-than"
    show HiFunGreaterThan    = "greater-than"
    show HiFunEquals         = "equals"
    show HiFunNotLessThan    = "not-less-than"
    show HiFunNotGreaterThan = "not-greater-than"
    show HiFunNotEquals      = "not-equals"
    show HiFunIf             = "if"
    show HiFunLength         = "length"
    show HiFunToUpper        = "to-upper"
    show HiFunToLower        = "to-lower"
    show HiFunReverse        = "reverse"
    show HiFunTrim           = "trim"
    show HiFunList           = "list"
    show HiFunRange          = "range"
    show HiFunFold           = "fold"
    show HiFunPackBytes      = "pack-bytes"
    show HiFunUnpackBytes    = "unpack-bytes"
    show HiFunZip            = "zip"
    show HiFunUnzip          = "unzip"
    show HiFunEncodeUtf8     = "encode-utf8"
    show HiFunDecodeUtf8     = "decode-utf8"
    show HiFunSerialise      = "serialise"
    show HiFunDeserialise    = "deserialise"
    show HiFunRead           = "read"
    show HiFunWrite          = "write"
    show HiFunMkDir          = "mkdir"
    show HiFunChDir          = "cd"
    show HiFunParseTime      = "parse-time"
    show HiFunRand           = "rand"
    show HiFunEcho           = "echo"
    show HiFunCount          = "count"
    show HiFunKeys           = "keys"
    show HiFunValues         = "values"
    show HiFunInvert         = "invert"
