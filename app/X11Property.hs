{-# LANGUAGE TypeApplications #-}
module X11Property where

import Graphics.X11
import Foreign.C (CLong, peekCString)
import Graphics.X11.Xlib.Extras
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)

getPropertyString :: Display -> Window -> Atom -> IO String
getPropertyString display window property =
  peekCString . tp_value =<< getTextProperty display window property

getPropertyAtoms :: HasCallStack => Display -> Window -> Atom -> IO [Atom]
getPropertyAtoms display window property =
  fmap (fromIntegral @CLong @Atom) . fromMaybe (error $ "missing value for property " <> show property) <$>
    getWindowProperty32 display property window
