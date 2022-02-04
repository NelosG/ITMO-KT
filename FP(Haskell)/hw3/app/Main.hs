module Main
  ( main
  ) where

import HW3.Base
import HW3.Action
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import System.Console.Haskeline
import Control.Monad.IO.Class
import Data.Set

main :: IO ()
main = runInputT defaultSettings $ loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return()
        Just ":q" -> return ()
        Just "" -> loop
        Just input -> do
          pf <- getExternalPrint
          let parseResult = parse input
          case parseResult of
            Left parseError -> liftIO $ pf $ show parseError
            Right parsed -> do
              let evaluateResult = eval parsed :: HIO (Either HiError HiValue)
              let allPermissions = fromList [AllowRead ..]
              r <- liftIO $ runHIO evaluateResult allPermissions
              liftIO $ case r of
                Left evalError -> pf $ show evalError
                Right result -> pf $ show $ prettyValue result
          loop
