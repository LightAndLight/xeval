{-# LANGUAGE ScopedTypeVariables #-}
module Logging where

import Prelude hiding (log)
import System.IO
import Control.Exception (finally, SomeException, catch, throwIO)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (getCurrentTime)

withLogger :: FilePath -> ((String -> IO ()) -> IO a) -> IO a
withLogger file k = do
  handle <- openFile file WriteMode
  
  let
    log str = do
      now <- getCurrentTime
      hPutStrLn handle $
        formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3Q" now <> " " <> str
      hFlush handle
  
  logExceptions log (k log) `finally` hClose handle
  where
    logExceptions :: (String -> IO ()) -> IO a -> IO a
    logExceptions log m = m `catch` \(err :: SomeException) -> do
      log $ "exception: " <> show err
      throwIO err
