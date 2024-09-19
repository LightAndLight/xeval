{-# LANGUAGE ScopedTypeVariables #-}
module X11Event where

import Graphics.X11
import Foreign.C (CULong)
import Foreign (peekByteOff)
import Graphics.X11.Xlib.Extras

waitNextEvent :: Display -> (XEventPtr -> IO a) -> IO a
waitNextEvent display f =
  allocaXEvent $ \eventPtr -> nextEvent display eventPtr *> f eventPtr

data SelectionNotify
  = SelectionNotify
  { sn_target :: Atom
  , sn_property :: Atom
  } deriving (Eq, Show)

getSelectionNotify :: XEventPtr -> IO (Maybe SelectionNotify)
getSelectionNotify eventPtr = do
  eventType <- get_EventType eventPtr
  if eventType == selectionNotify
    then do
      {-
      typedef struct {
      	int type;		/* SelectionNotify */
      	unsigned long serial;	/* # of last request processed by server */
      	Bool send_event;	/* true if this came from a SendEvent request */
      	Display *display;	/* Display the event was read from */
      	Window requestor;
      	Atom selection;
      	Atom target;
      	Atom property;		/* atom or None */
      	Time time;
      } XSelectionEvent;
      -}
      _type :: EventType <- peekByteOff eventPtr 0
      _serial :: CULong <- peekByteOff eventPtr 8
      _send_event :: Bool <- peekByteOff eventPtr 16
      _display <- fmap Display (peekByteOff eventPtr 24)
      _window :: Window <- peekByteOff eventPtr 32
      _selection :: Atom <- peekByteOff eventPtr 40
      target :: Atom <- peekByteOff eventPtr 48
      property :: Atom <- peekByteOff eventPtr 56
      _time :: Time <- peekByteOff eventPtr 64

      pure $ Just SelectionNotify{ sn_target = target, sn_property = property }
    else pure Nothing

waitEvent :: Display -> IO Event
waitEvent display = 
  allocaXEvent $ \eventPtr -> do
    nextEvent display eventPtr
    getEvent eventPtr

sendKeyPress :: Display -> Window -> Window -> KeyMask -> KeyCode -> IO ()
sendKeyPress display root targetWindow mask code =
  allocaXEvent $ \eventPtr -> do
    setEventType eventPtr keyPress
    setKeyEvent eventPtr targetWindow root none mask code True
    sendEvent display targetWindow False keyPressMask eventPtr

sendKeyRelease :: Display -> Window -> Window -> KeyMask -> KeyCode -> IO ()
sendKeyRelease display root targetWindow mask code =
  allocaXEvent $ \eventPtr -> do
    setEventType eventPtr keyRelease
    setKeyEvent eventPtr targetWindow root none mask code True
    sendEvent display targetWindow False keyReleaseMask eventPtr

sendSelectionNotify :: Display -> Window -> Atom -> Atom -> Atom -> Time -> IO ()
sendSelectionNotify display requestor selection target property time =
  allocaXEvent $ \eventPtr -> do
    setEventType eventPtr selectionNotify
    setSelectionNotify eventPtr requestor selection target property time
    sendEvent display requestor False noEventMask eventPtr
