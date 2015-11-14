{-# LANGUAGE RecordWildCards #-}


module Graphics.Rendering.Handa.Viewer (
  ViewerParameters(..)
, viewerGeometry
, phoneViewer
, laptopViewer
, desktopViewer
, projectorViewer
, fieldOfView
, reshape
, loadViewer
, dlpViewerDisplay
) where


import Data.Default (Default, def)
import Graphics.Rendering.DLP (DlpEncoding, DlpEye(..))
import Graphics.Rendering.DLP.Callbacks (DlpDisplay(..))
import Graphics.Rendering.Handa.Util (degree)
import Graphics.Rendering.OpenGL (GLdouble, MatrixMode(..), Position(..), Size(..), Vector3(..), Vertex3(..), ($=!), loadIdentity, lookAt, matrixMode, perspective, viewport)
import Graphics.UI.GLUT (DisplayCallback, ReshapeCallback)


data ViewerParameters =
  ViewerParameters
  {
    displayAspectRatio :: GLdouble -- width over height
  , displayThrowRatio  :: GLdouble -- width over distance
  , distanceNearPlane  :: GLdouble
  , distanceFarPlane   :: GLdouble
  , eyePosition        :: Vertex3 GLdouble
  , eyeSeparation      :: Vector3 GLdouble
  , eyeUpward          :: Vector3 GLdouble
  , sceneCenter        :: Vertex3 GLdouble
  }
    deriving (Eq, Read, Show)

instance Default ViewerParameters where
  def =
    ViewerParameters
    {
      displayAspectRatio = 1
    , displayThrowRatio  = 1
    , distanceNearPlane  = 0.5
    , distanceFarPlane   = 4.5
    , eyePosition        = Vertex3 0   0 2
    , eyeSeparation      = Vector3 0.2 0 0
    , eyeUpward          = Vector3 0   1 0
    , sceneCenter        = Vertex3 0   0 0
    }


viewerGeometry :: GLdouble -> GLdouble -> GLdouble -> ViewerParameters
viewerGeometry width height throw =
  def
  {
    displayAspectRatio = width / height
  , displayThrowRatio  = throw / width
  }


phoneViewer :: ViewerParameters
phoneViewer = viewerGeometry 5.27 2.80 12


laptopViewer :: ViewerParameters
laptopViewer = viewerGeometry 13.625 7.875 24


desktopViewer :: ViewerParameters
desktopViewer = viewerGeometry 20.75 11.625 32


projectorViewer :: ViewerParameters
projectorViewer =
  def
  {
    displayAspectRatio = 1.6 / 1.0
  , displayThrowRatio  = 1.5 / 1.0
  }


fieldOfView :: ViewerParameters -> GLdouble
fieldOfView ViewerParameters{..} = 2 * atan2 0.5 displayThrowRatio * degree


reshape :: ViewerParameters -> ReshapeCallback
reshape vp@ViewerParameters{..} wh@(Size w h) = 
  do
    viewport $=! (Position 0 0, wh)
    matrixMode $=! Projection
    loadIdentity
    perspective (fieldOfView vp) (fromIntegral w / fromIntegral h) distanceNearPlane distanceFarPlane
    matrixMode $=! Modelview 0


loadViewer :: ViewerParameters -> DlpEye -> IO ()
loadViewer ViewerParameters{..} eye =
  do
    loadIdentity
    let
      offset =
        case eye of
          LeftDlp  -> -1/2
          RightDlp ->  1/2
      Vertex3  xEye  yEye  zEye = eyePosition
      Vector3 dxEye dyEye dzEye = eyeSeparation
    lookAt
      (Vertex3 (xEye + offset * dxEye) (yEye + offset * dyEye) (zEye + offset * dzEye))
      sceneCenter
      eyeUpward


dlpViewerDisplay :: DlpEncoding -> ViewerParameters -> DisplayCallback -> DlpDisplay
dlpViewerDisplay encoding viewerParameters display =
  def 
    {
      dlpEncoding = encoding
    , doDisplay = \eye -> loadViewer viewerParameters eye >> display
    }
