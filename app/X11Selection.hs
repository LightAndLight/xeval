module X11Selection where

import Graphics.X11
import X11Atoms (Atoms (..))
import Prelude hiding (log)
import Graphics.X11.Xlib.Extras
import X11Event (waitNextEvent, getSelectionNotify, SelectionNotify (..))
import X11Property (getPropertyAtoms, getPropertyString)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT, ExceptT (..))
import Control.Monad.Trans (lift)

data ConvertError
  = ConversionFailed
  | UnexpectedEvent Event
  deriving Show

convertSelectionTargets ::
  Atoms ->
  Display ->
  Window ->
  Atom ->
  Atom ->
  IO (Either ConvertError [Atom])
convertSelectionTargets atoms display window selection property =
  runExceptT $ do
    lift $ xConvertSelection display selection (atoms_TARGETS atoms) property window currentTime
    event <- ExceptT . waitNextEvent display $ \eventPtr -> do
      mEvent <- getSelectionNotify eventPtr
      case mEvent of
        Nothing ->
          Left . UnexpectedEvent <$> getEvent eventPtr
        Just event ->
          pure $ Right event

    if sn_property event == none
      then do
        throwError ConversionFailed
      else do
        value <- lift $ getPropertyAtoms display window (sn_property event)
        lift $ deleteProperty display window (sn_property event)
        pure value

convertSelectionString ::
  Atoms ->
  Display ->
  Window ->
  Atom ->
  Atom ->
  Atom ->
  IO (Either ConvertError String)
convertSelectionString _atoms display window selection target property =
  runExceptT $ do
    lift $ xConvertSelection display selection target property window currentTime
    event <- ExceptT . waitNextEvent display $ \eventPtr -> do
      mEvent <- getSelectionNotify eventPtr
      case mEvent of
        Nothing ->
          Left . UnexpectedEvent <$> getEvent eventPtr
        Just event ->
          pure $ Right event

    if sn_property event == none
      then throwError ConversionFailed
      else lift $ getPropertyString display window (sn_property event)
