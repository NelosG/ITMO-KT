module HW3.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Applicative (Alternative ((<|>)), optional)
import Control.Exception (Exception, throwIO)
import qualified Control.Monad
import qualified Data.ByteString as BS
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock.POSIX (getCurrentTime)
import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))
import HW3.Helper (CreateF (createHiValue))
import System.Directory (createDirectory, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random.Stateful (getStdRandom, uniformR)

-- | Permissions to allow operation
data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Ord,Enum, Eq, Show, Bounded)

-- | 'Exception': the permissions required for the operation were not granted.
newtype PermissionException = PermissionRequired HiPermission deriving Show

instance Exception PermissionException

-- | Checks permissions and do something with IO
data HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap f a = HIO $ \set ->
    f <$> runHIO a set

instance Applicative HIO where
  pure a = HIO $ \_ -> pure a
  p <*> q = Control.Monad.ap p q

instance Monad HIO where
  (>>=) a f = HIO $ \set ->
    runHIO a set >>= \res1 ->
    runHIO (f res1) set

instance HiMonad HIO where
  runAction (HiActionRead path) = HIO $ \set -> do
    let toValuesList = fmap createHiValue
        toHiValueList = createHiValue . toValuesList
        readBytes bytes = either (\_ -> createHiValue bytes) createHiValue $ decodeUtf8' bytes
    if AllowRead `elem` set
    then (readBytes <$> BS.readFile path)
         <|> (toHiValueList <$> listDirectory path)
    else throwPermission AllowRead

  runAction (HiActionWrite path bytes) = HIO $ \set ->
    if AllowWrite `elem` set
    then do
      BS.writeFile path bytes
      pure $ HiValueNull
    else throwPermission AllowWrite

  runAction (HiActionChDir path) = HIO $ \set ->
    if AllowRead `elem` set
    then do
      setCurrentDirectory path
      pure $ HiValueNull
    else throwPermission AllowRead

  runAction (HiActionMkDir path) = HIO $ \set ->
    if AllowWrite `elem` set
    then do
      _ <- optional $ createDirectory path
      pure $ HiValueNull
    else throwPermission AllowWrite

  runAction (HiActionCwd) = HIO $ \set ->
    if AllowRead `elem` set
    then createHiValue <$> getCurrentDirectory
    else throwPermission AllowRead

  runAction (HiActionNow) = HIO $ \set ->
    if AllowTime `elem` set
    then createHiValue <$> getCurrentTime
    else throwPermission AllowTime

  runAction(HiActionRand a b) = HIO $ \_ ->
    createHiValue <$> getStdRandom (uniformR (a, b))

  runAction(HiActionEcho t) = HIO $ \set ->
    if AllowWrite `elem` set
    then do
      putStrLn $ T.unpack t
      pure $ HiValueNull
    else throwPermission AllowWrite

-- | Throws in IO that required some permission.
throwPermission :: HiPermission -> IO a
throwPermission = throwIO . PermissionRequired
