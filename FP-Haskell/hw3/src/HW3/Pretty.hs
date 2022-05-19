module HW3.Pretty
  ( prettyValue
  ) where

import Data.ByteString as BS (ByteString, unpack)
import Data.Foldable as DF (Foldable (foldr', toList))
import Data.Map as DM (toList)
import Data.Ratio (denominator, numerator)
import Data.Scientific (fromRationalRepetendUnlimited, toRealFloat)
import HW3.Base (HiAction (..), HiValue (..))
import Numeric (showFFloat, showHex)
import Prettyprinter (Doc, Pretty (pretty), comma, hsep, punctuate)
import Prettyprinter.Render.Terminal (AnsiStyle)

-- | Renders 'HiValue' into 'Doc'.
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueTime time)       = pretty $ "parse-time(\"" <> show time <> "\")"
prettyValue (HiValueFunction fun)    = pretty $ show fun
prettyValue (HiValueAction a)        = renderAction a
prettyValue (HiValueNumber rational) = renderRational rational
prettyValue (HiValueBool b)          = pretty $ if b then "true" else "false"
prettyValue (HiValueNull)            = pretty "null"
prettyValue (HiValueString s)        = pretty $ show s
prettyValue (HiValueBytes l)         = pretty $ renderBytes l

prettyValue (HiValueDict mp)         = (hsep $ pretty "{" :
    (punctuate comma $ Prelude.map renderPair $ DM.toList mp))
  <> pretty " }"

prettyValue (HiValueList l)          = (hsep $ pretty "[" :
    (punctuate comma $ Prelude.map prettyValue (DF.toList l)))
  <> pretty " ]"

-- | Renders pair of 'HiValue' into 'Doc'.
renderPair :: (HiValue, HiValue) -> Doc AnsiStyle
renderPair (a, b) = prettyValue a <> (pretty ": ") <>  prettyValue b

-- | Wraps 'String' in double quotes.
renderString :: String -> String
renderString s = "\"" <> s <> "\""

-- | Renders 'HiAction' into 'Doc'.
renderAction :: HiAction -> Doc AnsiStyle
renderAction action = pretty $ case action of
  HiActionRead  path       -> "read(" <> (renderString path) <> ")"
  HiActionWrite path bytes -> "write(" <> (renderString path) <> ", " <> (renderBytes bytes) <> ")"
  HiActionMkDir path       -> "mkdir(" <> (renderString path) <> ")"
  HiActionChDir path       -> "cd(" <> (renderString path) <> ")"
  HiActionCwd              -> "cwd"
  HiActionNow              -> "now"
  HiActionRand a b         -> "rand(" <> (show a) <> ", " <> (show b) <> ")"
  HiActionEcho t           -> "echo(" <> (show t) <> ")"

-- | Renders 'ByteString' into 'String'.
renderBytes :: ByteString -> String
renderBytes b = "[# " <> (DF.foldr' fun "#]" $ BS.unpack  b)
  where
    fun = \ h ->
      (<>) $ let str = (showHex h " ")
             in if (Prelude.length str == 2)
                then "0" ++ str
                else str

-- | Renders 'Rational' into 'Doc'.
renderRational :: Rational -> Doc AnsiStyle
renderRational x = pretty $ case (numerator x, denominator x) of
  (n, 1) -> show n
  (n, d) -> prettyDecimalOrFractional (x, n, d)

-- | Support function
prettyDecimalOrFractional :: (Rational, Integer, Integer) -> String
prettyDecimalOrFractional (x, n, d) = 
  if checkEndless d 
  then case fromRationalRepetendUnlimited x of
    (s, Nothing) -> showFFloat Nothing (toRealFloat s :: Double) ""
    (_, Just _)  -> prettyFractional d (quotRem n d)
  else prettyFractional d (quotRem n d)

-- | Support function
prettyFractional :: Integer -> (Integer, Integer) -> String
prettyFractional d (0, x) = show x ++ "/" ++ show d
prettyFractional d (x, y) = show x ++ (if y > 0 then " + " else " - ") ++ prettyFractional d (0, abs y)

-- | Checks to validate final decimal
checkEndless :: Integer -> Bool
checkEndless denom
  | even denom = checkEndless (denom `div` 2)
  | denom `mod` 5 == 0 = checkEndless (denom `div` 5)
  | otherwise = denom == 1
