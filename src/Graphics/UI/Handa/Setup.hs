{-|
Module      :  Graphics.UI.Handa.Setup
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Functions for setting up GLUT applications.
-}


{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}

module Graphics.UI.Handa.Setup (
  -- * Functions
  Setup(..)
, Stereo(..)
, Viewer(..)
, setup
, handleArguments
, idle
) where


import Control.Monad (when)
import Data.AdditiveGroup (AdditiveGroup)
import Data.Aeson (FromJSON)
import Data.Binary (Binary(..))
import Data.Data (Data)
import Data.Default (Default(def))
import Data.List ((\\))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import Graphics.Rendering.DLP (DlpEncoding)
import Graphics.Rendering.Handa.Viewer (ViewerParameters(eyeSeparation), desktopViewer, laptopViewer, phoneViewer, projectorViewer, reshape)
import Graphics.Rendering.OpenGL (BlendingFactor(..), Capability(Enabled), ComparisonFunction(Less), MatrixComponent, Vector3(..), ($=), blend, blendFunc)
import Graphics.UI.GLUT (DisplayMode(..), IdleCallback, createWindow, depthFunc, fullScreen, idleCallback, initialDisplayMode, initialize, postRedisplay, reshapeCallback)

import qualified Graphics.Rendering.DLP as D (DlpEncoding(..))


-- | The configuration for setting up the display.
data Setup a =
  Setup
  {
    stereo     :: Stereo                             -- ^ The type of stereo.
  , switchEyes :: Bool                               -- ^ Whether to switch the left and right eyes.
  , viewer     :: Either (ViewerParameters a) Viewer -- ^ The viewer information.
  , fullscreen :: Bool                               -- ^ Whether to display full screen.
  }
  deriving (Binary, Data, Eq, FromJSON, Generic, Read, Show, Typeable)

instance Default (Setup a) where
  def = Setup def False (Right def) False


-- | The type of stereo.  
data Stereo =
    DLP        -- ^ Frame-sequential DLP 3D ReadySync stereo.
  | QuadBuffer -- ^ Quad buffer stereo.
  | Cardboard  -- ^ Google Cardboard stereo.
  | Mono       -- ^ No stereo.
  deriving (Binary, Bounded, Data, Enum, Eq, FromJSON, Generic, Ord, Read, Show, Typeable)

instance Default Stereo where
  def = Mono


-- | The viewer information.
data Viewer =
    Phone     -- ^ A typical phone.
  | Laptop    -- ^ A typical laptop.
  | Desktop   -- ^ A typical desktop display.
  | Projector -- ^ A typical projector.
  deriving (Binary, Bounded, Data, Enum, Eq, FromJSON, Generic, Ord, Read, Show, Typeable)

instance Default Viewer where
  def = Laptop


-- | Set up a window with basic callbacks.  This creates a double-buffered window with a depth buffer, a transparency blending function, a generic reshaping callback, and a redisplaying idle function.  See 'handleArguments' for information on how command-line arguments are interpretted.
setup :: (AdditiveGroup a, MatrixComponent a, RealFloat a, Storable a)
      => String                                         -- ^ The window title.
      -> String                                         -- ^ The program name.
      -> [String]                                       -- ^ The X11 arguments.
      -> Setup a                                        -- ^ The setup configuration.
      -> IO (DlpEncoding, ViewerParameters a, [String]) -- ^ An action returing the DLP encoding requested, the viewer parameters, and the uninterpretted arguments.
setup title program arguments Setup{..} =
  do
    arguments' <- initialize program arguments
    initialDisplayMode $=
      (if stereo == QuadBuffer then (Stereoscopic :) else id)
        [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow title
    depthFunc $= Just Less 
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    when fullscreen fullScreen
    let
      dlp = case stereo of
        DLP        -> D.FrameSequential
        QuadBuffer -> D.QuadBuffer
        Cardboard  -> D.SideBySide
        Mono       -> D.LeftOnly
      viewerParameters = case viewer of
        Right Phone     -> phoneViewer
        Right Laptop    -> laptopViewer
        Right Desktop   -> desktopViewer
        Right Projector -> projectorViewer
        Left _          -> undefined
      viewerParameters' =
        if switchEyes
        then viewerParameters {eyeSeparation = (\(Vector3 x y z) -> Vector3 (-x) (-y) (-z)) $ eyeSeparation viewerParameters}
        else viewerParameters
    reshapeCallback $= Just (reshape viewerParameters')
    idleCallback $= Just idle
    return (dlp, viewerParameters', arguments')


-- | Act on command-line arguments.
--
-- *   \"--dlp\" puts the application in frame-sequential DLP (3D ReadySync) stereo mode.
--
-- *   \"--quadbuffer\" puts the application in quad-buffer stereo mode.
--
-- *   \"--cardboard\" puts the application in side-by-side (Google Cardboard) stereo mode.
--
-- *   \"--switchEyes\" swaps the views of the left and right eyes.
--
-- *   \"--phone\" sets the frustum for a typical smartphone.
--
-- *   \"--laptop\" sets the frustum for a typical laptop.
--
-- *   \"--desktop\" sets the frustum for a typical desktop monitor.
--
-- *   \"--projection\" sets the frustum for a typical projector.
--
-- *   \"--fullscreen\" puts the application in full screen mode.
handleArguments :: [String]            -- ^ The arguments.
                -> (Setup a, [String]) -- ^ The setup configuration and the remaining, uninterpretted, arguments.
handleArguments arguments =
  let
    stereo
      | "--dlp"        `elem` arguments = DLP
      | "--cardboard"  `elem` arguments = Cardboard
      | "--quadbuffer" `elem` arguments = QuadBuffer
      | otherwise                       = Mono
    switchEyes = "--switchEyes" `elem` arguments
    viewer
      | "--phone"      `elem` arguments  = Right Phone
      | "--laptop"     `elem` arguments  = Right Laptop
      | "--desktop"    `elem` arguments  = Right Desktop
      | "--projector"  `elem` arguments  = Right Projector
      | otherwise                        = Right Laptop
    fullscreen = "--fullscreen" `elem` arguments
    keywords = ["--dlp", "--cardboard", "--switchEyes", "--phone", "--laptop", "--desktop", "--projector", "--fullscreen"]
  in
    (Setup{..}, arguments \\ keywords)


-- | An idle callback that simply posts a request for redisplay.
idle :: IdleCallback
idle = postRedisplay Nothing
