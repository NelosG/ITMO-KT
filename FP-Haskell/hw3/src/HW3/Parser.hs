module HW3.Parser
  ( HW3.Parser.parse
  ) where

import qualified Control.Monad.Combinators as CMC
import qualified Control.Monad.Combinators.Expr as LE
import Control.Monad.Identity (Identity)
import Data.Char (isAlpha, isAlphaNum)
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))
import HW3.Helper (CreateF (..))
import Numeric (readHex)
import Text.Megaparsec
import Text.Megaparsec.Char (char, hexDigitChar, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parses 'String' into 'HiExpr' or fails with ParseErrorBundle.
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser parser ""

-- | Support type.
type Parser = Parsec Void String

-- | Parser that parse expression and checks that there is end of file.
parser :: Parser HiExpr
parser = expr <* eof

-- | Creates parser that parse expression.
expr :: Parser HiExpr
expr = LE.makeExprParser term binaryTable

-- | Term for 'expr' parser.
term :: Parser HiExpr
term = do
  _ <- wss
  res <- pDot =<< pHiArgs =<< (try pHiValue <|> exprInBrackets)
  _ <- wss
  return res

-- | Parser that parse expression in brackets.
exprInBrackets :: Parser HiExpr
exprInBrackets = do
  _ <- char '(' >> wss
  res <- expr
  _ <- wss >> char ')'
  return res

-- | Parses many 'HiExpr' applications.
pHiArgs :: HiExpr -> Parser HiExpr
pHiArgs inExpr = try (pDot =<< wss *> exprWithArgs) <|> pure inExpr
  where
    exprWithArgs = HiExprApply inExpr <$> pHiArgsVals '(' expr ')'

-- | Parses args list.
pHiArgsVals :: Char -> Parser a -> Char -> Parser [a]
pHiArgsVals start p end = do
  let separated = p  `sepBy` (char ',' <* wss)
  between (char start  <* wss) (char end) separated

-- | Parses 'HiValue'.
pHiValue :: Parser HiExpr
pHiValue = try pHiList <|>
  try pHiDict <|>
  HiExprValue <$>
    (try pHiAction
    <|>  try pHiByteList
    <|>  try pHiFun
    <|>  try pHiValueNumberExpr
    <|>  try pHiBool
    <|>  try pHiNull
    <|>  try pHiString)

-- | Parses 'HiValueNumber'.
pHiValueNumberExpr :: Parser HiValue
pHiValueNumberExpr = createHiValue <$> L.signed wss L.scientific

-- | Parses 'HiValueBool'.
pHiBool :: Parser HiValue
pHiBool = choice
  [HiValueBool True <$ string "true"
  ,HiValueBool False <$ string "false"]

-- | Parses 'HiValueNull'.
pHiNull :: Parser HiValue
pHiNull = do
  _ <- string "null"
  return HiValueNull

-- | Parses 'HiValueString'.
pHiString :: Parser HiValue
pHiString = do
  text <- stringLiteral
  return $ createHiValue text

-- | Parses 'HiValueString' content.
stringLiteral :: Parser String
stringLiteral = char '\"' >> manyTill L.charLiteral (char '\"')

-- | Parses list into 'HiFunList' with args.
pHiList :: Parser HiExpr
pHiList = do
  let applyList = HiExprApply (HiExprValue $ createHiValue $ HiFunList)
  applyList <$> pHiArgsVals '[' expr ']'

-- | Parses 'HiValueBytes'.
pHiByteList :: Parser HiValue
pHiByteList = do
  list <- between (string "[#"  <* wss) (string "#]") (pHiByte  `sepEndBy` (char ' ' <* wss))
  return $ createHiValue list

-- | Parses 2 hex numbers into 'Word8'.
pHiByte :: Parser Word8
pHiByte =  do
  hexString <- CMC.count 2 hexDigitChar
  case readHex hexString :: [(Word8, String)] of
    [(hex,"")] ->
      if hex <= 255
      then return $ fromIntegral hex
      else Text.Megaparsec.empty
    _ -> Text.Megaparsec.empty

-- | Parses 'cdw' and 'now' actions.
pHiAction :: Parser HiValue
pHiAction =  HiValueAction <$> choice
  [ HiActionCwd <$ string "cwd"
  , HiActionNow <$ string "now" ]

-- | Parses Dictionary
pHiDict :: Parser HiExpr
pHiDict = HiExprDict <$> pHiArgsVals '{' pHiDictPair '}'

-- | Parses pair of 'HiExpr' separated by \':\' character
pHiDictPair :: Parser (HiExpr, HiExpr)
pHiDictPair = do
  _ <- wss
  first <- expr
  _ <- wss >> char ':' >> wss
  second <- expr
  pure $ (first, second)

-- | Creates a parser that parses the 'HiValueFunction' based on the show instance of 'HiValueFunction'.
pHiFun :: Parser HiValue
pHiFun = createHiValue <$> (choice $ Prelude.map (\x -> x <$ string(show x)) [HiFunDiv ..])

-- | Table for binary operations presented in operator style.
binaryTable :: [[LE.Operator (ParsecT Void String Identity) HiExpr]]
binaryTable =
  [ [ (LE.InfixL . try) $ (consumeOperation HiFunDiv) <$ char '/' <* notFollowedBy (char '=')
    , infixL (consumeOperation HiFunMul)            "*"]

  , [ infixL (consumeOperation HiFunSub)            "-"
    , infixL (consumeOperation HiFunAdd)            "+"]

  , [ infixN (consumeOperation HiFunEquals)         "=="
    , infixN (consumeOperation HiFunNotEquals)      "/="
    , infixN (consumeOperation HiFunNotGreaterThan) "<="
    , infixN (consumeOperation HiFunNotLessThan)    ">="
    , infixN (consumeOperation HiFunLessThan)       "<"
    , infixN (consumeOperation HiFunGreaterThan)    ">"]

  , [infixR (consumeOperation HiFunAnd)             "&&"]
  , [infixR (consumeOperation HiFunOr)              "||"]]

-- | Consumes operation and creates 'HiExpr' with the corresponding 'HiFun' and args for it.
consumeOperation :: HiFun -> HiExpr -> HiExpr -> HiExpr
consumeOperation fun a b = HiExprApply (HiExprValue $ createHiValue fun) [a, b]

-- | Creates infixl operator
infixL :: MonadParsec e s m => (a -> a -> a) -> Tokens s -> LE.Operator m a
infixL = createOperator LE.InfixL

-- | Creates infixr operator
infixR :: MonadParsec e s m => (a -> a -> a) -> Tokens s -> LE.Operator m a
infixR = createOperator LE.InfixR

-- | Creates infixn operator
infixN :: MonadParsec e s m => (a -> a -> a) -> Tokens s -> LE.Operator m a
infixN = createOperator LE.InfixN

-- | Creates operator by specified infixness.
createOperator :: MonadParsec e s m => (m (a -> a -> a) -> LE.Operator m a) -> (a -> a -> a) -> Tokens s -> LE.Operator m a
createOperator inf f name = (inf . try) (f <$ string name)

-- | Wraps expression in 'HiExprRun'.
pRun :: HiExpr -> Parser HiExpr
pRun inExpr = do
  run <- optional $ char '!'
  maybe (return $ inExpr) (const $ pDot $ HiExprRun inExpr) run

-- | Parses dot access argument.
pDot :: HiExpr -> Parser HiExpr
pDot res = do
  let charOrNum    = (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
      concatList l = HiExprValue $ createHiValue $ Prelude.foldl1 (\x y-> x <> "-" <> y) l
      applyRes l   = pDot $ HiExprApply res $ [concatList l]
  list <- optional $ char '.' >> charOrNum `sepBy1` char '-'
  pHiArgs =<< maybe (pRun res) applyRes list

-- | Many whitespaces.
wss :: Parser ()
wss = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")
