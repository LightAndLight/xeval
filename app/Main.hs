{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Graphics.X11.Xlib hiding (createWindow)
import Graphics.X11.Xlib.Extras
import Control.Monad (when)
import Foreign.C.Types (CChar(..), CLong (..))
import Data.Word (Word8)
import Data.Maybe (fromJust)
import qualified Codec.Binary.UTF8.String
import Data.List (intersect)
import Prelude hiding (log, break)
import Text.ParserCombinators.ReadP (readP_to_S)
import Control.Applicative ((<**>))
import System.Exit (exitFailure)
import Data.List.NonEmpty (nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Options.Applicative as Options

import qualified Expr
import X11Atoms (Atoms (..), initAtoms)
import X11Selection (convertSelectionTargets, convertSelectionString)
import X11Event (sendKeyPress, sendKeyRelease, waitEvent, sendSelectionNotify)
import Logging (withLogger)
import Errors (onErrorM, orElseM)
import Control.Exception (finally)
import Data.IORef (newIORef, writeIORef, readIORef)
import System.Environment (getProgName)

data Context
  = Context
  { ctx_display :: Display
  , ctx_root :: Window
  , ctx_window :: Window
  , ctx_supportedFormats :: [Atom]
  , ctx_atoms :: Atoms
  , ctx_property :: Atom
  }

setup :: IO (IO (), Context)
setup = do
  display <- openDisplay ":0"
  root <- rootWindow display (defaultScreen display)
  window <- setupWindow display root

  atoms <- initAtoms display  
  uTF8_STRING <- internAtom display "UTF8_STRING" False
  tEXTPLAIN_UTF8 <- internAtom display "text/plain;charset=utf-8" False
  let supportedFormats = [sTRING, uTF8_STRING, tEXTPLAIN_UTF8]

  property <- internAtom display "EVAL_DATA" False

  let
    ctx =
      Context
      { ctx_display = display
      , ctx_root = root
      , ctx_window = window
      , ctx_supportedFormats = supportedFormats
      , ctx_atoms = atoms
      , ctx_property = property
      }
  pure
    ( do
        destroyWindow display window
        closeDisplay display
    , ctx
    )
  where
    width = 1
    height = 1
    
    setupWindow :: Display -> Window -> IO Window
    setupWindow display root =
      createSimpleWindow
        display
        root
        0
        0
        1
        width
        height
        (0 :: Pixel)
        (0 :: Pixel)

withContext :: (Context -> IO a) -> IO a
withContext k = do
  (finalise, ctx) <- setup
  k ctx `finally` finalise

data Config
  = Config
  { config_logFile :: FilePath
  }

configParserInfo :: String -> Options.ParserInfo Config
configParserInfo progName = Options.info (configParser <**> Options.helper) Options.fullDesc
  where
    configParser :: Options.Parser Config
    configParser =
      (\logFile -> Config{ config_logFile = logFile }) <$>
      Options.strOption
        (Options.short 'l' <>
          Options.long "log-file" <>
          Options.metavar "FILE" <>
          Options.value ("/tmp/" <> progName <> ".log") <>
          Options.showDefault <>
          Options.help "Write logs to FILE"
        )

transform :: String -> Maybe String
transform input = do
  e <- case readP_to_S Expr.parser input of
    [(a, _)] -> Just a
    _ -> Nothing
  pure . Expr.printValue $ Expr.eval e

main :: IO ()
main = do
  progName <- getProgName
  config <- Options.execParser (configParserInfo progName)
  withLogger (config_logFile config) $ \log -> withContext $ \ctx -> do
    let fatal err = log ("fatal: " <> err) *> exitFailure
    
    let display = ctx_display ctx
    let root = ctx_root ctx
    let window = ctx_window ctx
    let supportedFormats = ctx_supportedFormats ctx
    let tARGETS = atoms_TARGETS $ ctx_atoms ctx

    clipboard <- internAtom display "CLIPBOARD" False

    targets <-
      convertSelectionTargets (ctx_atoms ctx) display window pRIMARY (ctx_property ctx)
      `onErrorM` (fatal . show)

    supportedTargets <-
      pure (nonEmpty $ targets `intersect` ctx_supportedFormats ctx) `orElseM` do
        log "no supported targets received"
        exitFailure

    str <-
      convertSelectionString
        (ctx_atoms ctx)
        display
        window
        pRIMARY
        (NonEmpty.head supportedTargets)
        (ctx_property ctx)
      `onErrorM` (fatal . show)

    xSetSelectionOwner display clipboard window currentTime
    do
      owner <- xGetSelectionOwner display clipboard
      when (owner /= window) $ fatal "failed to get ownership of clipboard"

    vCode <- keysymToKeycode display xK_v

    (targetWindow, _) <- getInputFocus display

    sendKeyPress display root targetWindow controlMask vCode
    sendKeyRelease display root targetWindow controlMask vCode

    destPropertyVar <- newIORef Nothing
    loop_ $ \continue break -> do
      event <- waitEvent display
      case event of
        SelectionRequest{ev_requestor = requestor, ev_selection = selection, ev_target = target, ev_property = property, ev_time = time} -> do
          case target of
            _
              | target == tARGETS -> do
                  log "selectionRequest: TARGETS"
                  changeProperty32 display requestor property aTOM propModeReplace $ fmap (fromIntegral @Atom @CLong) supportedFormats

                  sendSelectionNotify display requestor selection target property time

                  continue
              | target `elem` supportedFormats -> do
                  selectInput display requestor propertyChangeMask
            
                  targetValue <- fromJust <$> getAtomName display target
                  log $ "selectionRequest: " <> targetValue
      
                  case transform str of
                    Nothing -> do
                      log $ "parse error for " <> show str
                      sendSelectionNotify display requestor selection target (none :: Atom) time
                    Just str' -> do
                      log $ "result: " <> show str'
                      writeIORef destPropertyVar $ Just property
                      changeProperty8 display requestor property target propModeReplace (fromIntegral @Word8 @CChar <$> Codec.Binary.UTF8.String.encode str')
                      sendSelectionNotify display requestor selection target property time

                  continue
              | otherwise -> do
                  targetValue <- fromJust <$> getAtomName display target
                  log $ "unsupported target: " <> show targetValue
                  break ()
        PropertyEvent{ev_atom = atom, ev_propstate = propstate}
          | propstate == propertyDelete -> do
              atomValue <- fromJust <$> getAtomName display atom
              log $ "property deleted (atom = " <> show atomValue <> ")"
              mDestProperty <- readIORef destPropertyVar
              if mDestProperty == Just atom
                then do
                  log "finished"
                  break ()
                else continue
          | otherwise -> do
              atomValue <- fromJust <$> getAtomName display atom
              log $ "other property event (atom = " <> show atomValue <> ", state = " <> show propstate <> ")"
              continue
        SelectionClear{} -> do
          log "ownership relinquished"
          break ()
        _ -> do
          log $ "unexpected event: " <> show event
          break ()
  
    flush display  
  where
    loop_ :: Monad m => (forall r. m r -> (b -> m r) -> m r) -> m b
    loop_ f = do
      result <- f (pure $ Left ()) (pure . Right)
      case result of
        Left () -> loop_ f
        Right b -> pure b
