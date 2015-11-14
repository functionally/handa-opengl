module Graphics.UI.Handa.Setup (
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


setup :: String -> IO (DlpEncoding, ViewerParameters, [String])
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


idle :: IdleCallback
idle = postRedisplay Nothing
