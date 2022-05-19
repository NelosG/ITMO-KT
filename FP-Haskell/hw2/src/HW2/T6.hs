{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( -- * Datatypes
    ParseError (..)
  , Parser (..)
    -- * functions
  , pChar
  , pEof
  , parseError
  , parseExpr
  , runP
  ) where

import Control.Applicative (Alternative (..), Applicative (liftA2), liftA3, (<**>))
import Control.Monad (MonadPlus, mfilter)
import Data.Char (digitToInt, isDigit)
import Data.Foldable (Foldable (foldl'))
import Data.Scientific (scientific, toRealFloat)
import GHC.Natural (Natural)
import HW2.T1 (Annotated ((:#)), Except (Error, Success))
import HW2.T4 (Expr (Op, Val), Prim (Add, Div, Mul, Sub))
import HW2.T5 (ExceptState (ES, runES))

-- | Error: thrown when 'Parser' can't parse input.
newtype ParseError = ErrorAtPos Natural

-- | Parser that parse something.
newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Function to run 'Parser'.
runP
  :: Parser a -- ^ 'Parser'
  -> String -- ^ 'Parser' input
  -> Except ParseError a -- result: value if 'Success' or an 'ParseError' if an 'Error'
runP (P p) s =
  case runES p (0,  s) of
    Success (val :# _) ->  Success val
    Error err          ->  Error err

-- | Parser that parse one character.
--
-- Throws 'Error' if there is no nextCharacter.
pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    []       -> Error (ErrorAtPos pos)
    (c : cs) -> Success (c :# (pos + 1, cs))

-- | Returns 'Parser' with 'Error' with 'ParseError' at pos 0.
parseError :: Parser a
parseError = P $ ES (\(pos, _) -> Error $ ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P a) (P b) = P $ ES $ \(pos, str) ->
    case runES a (pos,  str) of
      Error _ -> runES b (pos,  str)
      success -> success

instance MonadPlus Parser   -- No methods.

-- | 'Parser' that checks is there end of file or not.
--
-- Throws 'ParseError' at pos 0 if given non-empty string.
pEof :: Parser ()
pEof = P $ ES \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)

-- | Function for parsing an 'Expr' from 'String'.
parseExpr :: String -> Except ParseError Expr
parseExpr s = runP syntax s

{-| Lets create context-free grammar:

Grammar in EBNF format:
  - syntax=expr;
  - expr = term, expr1;
  - expr1 = (\"+\" | \"-\"), term, expr1 | \'eps\';
  - term = factor, term1;
  - term1 = (\"*\" | \"/\"), factor, term1 | \'eps\';
  - factor = doubleNumeric | \"(\", expr, \")\";
  - doubleNumeric = nonEmptyNumeric, additional;
  - additional = \".\", nonEmptyNumeric | \'eps\';
  - nonEmptyNumeric = num, numeric;
  - numeric = num | \'eps\';
  - num = \"1\" | \"2\" | \"3\" | \"4\" | \"5\" | \"6\" | \"7\" | \"8\" | \"9\" | \"0\";
-}
syntax :: Parser Expr
syntax = pExpr <* pWhiteSpace <* pEof

-- | expr = term, expr1;
pExpr :: Parser Expr
pExpr = pTerm <**> pExpr1

-- | Support function.
filterChar :: Char -> Parser Char
filterChar a = mfilter (== a) pChar

-- | Support function.
getExpr :: Char -> Expr -> (Expr -> Expr) -> Expr -> Expr
getExpr op cur next prev = next $
  case op of
     '+' -> Op (Add prev cur)
     '-' -> Op (Sub prev cur)
     '*' -> Op (Mul prev cur)
     '/' -> Op (Div prev cur)
     _   -> error "Undefined operator"

-- | expr1 = (\"+\" | \"-\"), term, expr1 | \'eps\';
pExpr1 :: Parser (Expr -> Expr)
pExpr1 = pWhiteSpace *> (nextOperation <|> nothing)
  where
    nextOperation = liftA3 getExpr (filterChar '+' <|> filterChar '-') pTerm pExpr1
    nothing       = pure id

-- | term = factor, term1;
pTerm :: Parser Expr
pTerm = pFactor <**> pTerm1

-- | term1 = (\"*\" | \"/\"), factor, term1 | \'eps\';
pTerm1 :: Parser (Expr -> Expr)
pTerm1 = pWhiteSpace *> (nextOperation <|> nothing)
  where
    nextOperation = liftA3 getExpr (filterChar '*' <|> filterChar '/') pFactor pTerm1
    nothing       = pure id

-- | factor = DoubleNumeric | \"(\", expr, \")\";
pFactor :: Parser Expr
pFactor = pWhiteSpace *> (value <|> exprInBrackets)
  where
    value          = Val <$> pDoubleNumeric
    exprInBrackets = filterChar '(' *> pExpr <* pWhiteSpace <* filterChar ')'

-- | doubleNumeric = nonEmptyNumeric, additional;
pDoubleNumeric :: Parser Double
pDoubleNumeric = pWhiteSpace *> liftA2 myRead pNonEmptyNumeric pAdditional
  where
    -- | additional = \".\", nonEmptyNumeric | \'eps\';
    pAdditional :: Parser String
    pAdditional = (filterChar '.') *> pNonEmptyNumeric <|> pure []

-- | Take two string before dot and after and returns double created from this two strings.
myRead :: String -> String -> Double
myRead beforeDot afterDot = toRealFloat $ scientific integer tensDegree
  where
    concatNumbers :: Int -> Char -> Int
    concatNumbers prev c = prev * 10 + digitToInt c

    numb       = foldl' concatNumbers 0 $ beforeDot <> afterDot
    integer    = toInteger numb
    tensDegree = negate $ length afterDot

-- | nonEmptyNumeric = num, numeric;
--
-- - numeric = num | \'eps\';
-- - num = \"1\" | \"2\" | \"3\" | \"4\" | \"5\" | \"6\" | \"7\" | \"8\" | \"9\" | \"0\";
pNonEmptyNumeric :: Parser String
pNonEmptyNumeric = some (mfilter Data.Char.isDigit pChar)

-- | Skips some or none white spaces.
pWhiteSpace :: Parser String
pWhiteSpace = many $ filterChar ' '
