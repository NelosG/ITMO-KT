module HW3.Types.List
  ( listUnOperation
  ) where
import Control.Monad.Except (ExceptT)
import Control.Monad.Trans.Except (throwE)
import Data.Bits.Extras (w8)
import Data.Sequence (Seq (Empty, (:|>)))
import Data.Word (Word8)
import HW3.Base (HiError (..), HiMonad, HiValue (..))
import HW3.Helper (CreateF (..), Function (..), getInt, jointOperation)

-- | Creates 'HiValueBytes'
createByteList :: HiMonad m => Seq HiValue -> [Word8] -> ExceptT HiError m HiValue
createByteList Empty  res = returnHiValue $ res
createByteList (h :|> (HiValueNumber rat)) res = do
  resValue <- getInt rat
  if resValue >= 0 && resValue <= 255
  then createByteList h ((w8 resValue) : res)
  else throwE HiErrorInvalidArgument
createByteList _ _ = throwE HiErrorInvalidArgument

-- | Evaluates unary operation with 'HiValueList'.
listUnOperation :: HiMonad m => Function -> Seq HiValue -> ExceptT HiError m HiValue
listUnOperation PackBytes t = createByteList t []
listUnOperation fun t       = jointOperation fun t
