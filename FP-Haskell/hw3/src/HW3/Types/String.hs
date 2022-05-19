module HW3.Types.String
  ( stringUnOperation
  ) where

import Control.Monad.Except (ExceptT)
import Data.Text as T (Text, strip, toLower, toUpper, unpack)
import Data.Text.Encoding (encodeUtf8)
import HW3.Base (HiAction (..), HiError (..), HiMonad, HiValue (..))
import HW3.Helper (CreateF (..), Function (..), jointOperation)
import Text.Read (readMaybe)

-- | Evaluates unary operation with 'HiValueString'.
stringUnOperation :: HiMonad m => Function -> Text -> ExceptT HiError m HiValue
stringUnOperation ToUpper t = returnHiValue $ T.toUpper t
stringUnOperation ToLower t = returnHiValue $ T.toLower t
stringUnOperation Trim t    = returnHiValue $ strip t
stringUnOperation Encode t  = returnHiValue $ encodeUtf8 t
stringUnOperation Echo t    = return $ HiValueAction $ HiActionEcho t
stringUnOperation Time t    = return $ maybe HiValueNull HiValueTime $ readMaybe $ T.unpack t
stringUnOperation Read t    = return $ HiValueAction $ HiActionRead $ T.unpack t
stringUnOperation MkDir t   = return $ HiValueAction $ HiActionMkDir $ T.unpack t
stringUnOperation ChDir t   = return $ HiValueAction $ HiActionChDir $ T.unpack t
stringUnOperation fun t     = jointOperation fun t
