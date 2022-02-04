module HW3.Types.Bytes
  ( bytesUnOperation
  ) where

import Codec.Compression.Zlib (CompressParams (..), bestCompression, compressWith, decompress, defaultCompressParams)
import Codec.Serialise (deserialiseOrFail)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.Either (fromRight)
import Data.Text.Encoding (decodeUtf8')
import HW3.Base (HiError (..), HiMonad, HiValue (..))
import HW3.Helper (CreateF (..), Function (..), MyType (..), jointOperation)

-- | Default params for compression.
compressParams :: CompressParams
compressParams = (defaultCompressParams {compressLevel = bestCompression})

-- | Evaluates unary operation with 'HiValueBytes'.
bytesUnOperation :: HiMonad m => Function -> BS.ByteString -> ExceptT HiError m HiValue
bytesUnOperation Deserialise s = return $ fromRight HiValueNull $ deserialiseOrFail $ fromStrict s
bytesUnOperation UnpackBytes s = returnHiValue $ toListF s
bytesUnOperation Unzip s       = returnHiValue $ decompress $ fromStrict s
bytesUnOperation Decode s      = return $ either (const $ HiValueNull) createHiValue $ decodeUtf8' s
bytesUnOperation Zip s         = returnHiValue $ compressWith compressParams $ fromStrict s
bytesUnOperation fun t         = jointOperation fun t
