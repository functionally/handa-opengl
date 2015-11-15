{-|
Module      :  Graphics.UI.Handa.Setup
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Functions for setting up GLUT applications.
-}


module Graphics.UI.Handa.Setup (
  -- * Functions
  setup
, handleArguments
, idle
) where


import Control.Monad (when)
import Data.Default (def)
import Data.List ((\\))
import Graphics.Rendering.DLP (DlpEncoding(..))
import Graphics.Rendering.Handa.Viewer (ViewerParameters(eyeSeparation), desktopViewer, laptopViewer, phoneViewer, projectorViewer, reshape)
import Graphics.Rendering.OpenGL (BlendingFactor(..), Capability(Enabled), ComparisonFunction(Less), Vector3(..), ($=), blend, blendFunc)
import Graphics.UI.GLUT (DisplayMode(..), IdleCallback, createWindow, depthFunc, getArgsAndInitialize, fullScreen, idleCallback, initialDisplayMode, postRedisplay, reshapeCallback)


-- | Set up a window with basic callbacks.  This creates a double-buffered window with a depth buffer, a transparency blending function, a generic reshaping callback, and a redisplaying idle function.  See 'handleArguments' for information on how command-line arguments are interpretted.
setup :: String                                       -- ^ The window title.
      -> IO (DlpEncoding, ViewerParameters, [String]) -- ^ An action returing the DLP encoding requested, the viewer parameters, and the uninterpretted arguments.
setup title =
  do
    (_, arguments) <- getArgsAndInitialize
    initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
    _window <- createWindow title
    depthFunc $= Just Less 
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    r@(_, viewerParameters, _) <- handleArguments arguments
    reshapeCallback $= Just (reshape viewerParameters)
    idleCallback $= Just idle
    return r


-- | Act on command-line arguments.
--
-- *   \"--fullscreen\" puts the application in full screen mode.
--
-- *   \"--stereo\" puts the application in frame-sequential DLP stereo mode.
--
-- *   \"--cardboard\" puts the application in side-by-side (Google Cardboard) stereo mode.
--
-- *   \"--phone\" sets the frustum for a typical smartphone.
--
-- *   \"--laptop\" sets the frustum for a typical laptop.
--
-- *   \"--desktop\" sets the frustum for a typical desktop monitor.
--
-- *   \"--projection1 sets the frustum for a typical projector.
--
-- *   \"--switchEyes\" swaps the views of the left and right eyes.
handleArguments :: [String] -> IO (DlpEncoding, ViewerParameters, [String])
handleArguments arguments =
  do
    when ("--fullscreen" `elem` arguments) fullScreen
    let
      dlp
        | "--stereo"    `elem` arguments = FrameSequential
        | "--cardboard" `elem` arguments = SideBySide
        | otherwise                     = LeftOnly
      viewerParameters
        | "--phone"     `elem` arguments = phoneViewer
        | "--laptop"    `elem` arguments = laptopViewer
        | "--desktop"   `elem` arguments = desktopViewer
        | "--projector" `elem` arguments = projectorViewer
        | otherwise                      = def
      viewerParameters' =
        if "--switchEyes" `elem` arguments
        then viewerParameters {eyeSeparation = (\(Vector3 x y z) -> Vector3 (-x) (-y) (-z)) $ eyeSeparation viewerParameters}
        else viewerParameters
      keywords = ["--fullscreen", "--stereo", "--cardboard", "--phone", "--laptop", "--desktop", "--projector", "--switchEyes"]
    return (dlp, viewerParameters', arguments \\ keywords)


-- | An idle callback that simply posts a request for redisplay.
idle :: IdleCallback
idle = postRedisplay Nothing
