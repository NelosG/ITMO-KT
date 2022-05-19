module HW3.Types.Dict
  ( dictUnOperation
  ) where

import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.Map as DM
import Data.Sequence (Seq (Empty, (:<|)), (><))
import HW3.Base (HiError (..), HiMonad, HiValue (..))
import HW3.Helper (CreateF (..), Function (..))

-- | Evaluates unary operation with 'HiValueDict'.
dictUnOperation :: HiMonad m => Function -> (DM.Map HiValue HiValue) -> ExceptT HiError m HiValue
dictUnOperation Invert mp = returnHiValue $ DM.map createHiValue $
  DM.fromListWith (><) pairs
    where pairs = [(v, k :<| Empty) | (k, v) <- DM.toList mp]

dictUnOperation Keys   mp    = returnHiValue $ DM.keys mp
dictUnOperation Values mp    = returnHiValue $ DM.elems mp
dictUnOperation    _ _     = throwE HiErrorInvalidArgument
