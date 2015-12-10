{-|
Module      :  Graphics.Rendering.Handa.Viewer
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Functions for managing perspectives and frusta.
-}


{-# LANGUAGE RecordWildCards #-}


module Graphics.Rendering.Handa.Viewer (
  -- * Viewer Geometry
  ViewerParameters(..)
, viewerGeometry
, displayAspectRatio
, displayThrowRatio
, fieldOfView
  -- * Typical Devices
, phoneViewer
, laptopViewer
, desktopViewer
, projectorViewer
  -- * Callbacks and Rendering
, reshape
, loadViewer
, dlpViewerDisplay
) where


import Data.Default (Default, def)
import Graphics.Rendering.DLP (DlpEncoding, DlpEye(..))
import Graphics.Rendering.DLP.Callbacks (DlpDisplay(..))
import Graphics.Rendering.Handa.Projection (Screen(..), aspectRatio, projection, throwRatio)
import Graphics.Rendering.Handa.Util (degree)
import Graphics.Rendering.OpenGL (GLdouble, MatrixMode(..), Position(..), Vector3(..), Vertex3(..), ($=!), loadIdentity, lookAt, matrixMode, viewport)
import Graphics.UI.GLUT (DisplayCallback, ReshapeCallback)


-- | Paramaters specifying a viewer, including the frustum of the view.
data ViewerParameters =
  ViewerParameters
  {
    screen             :: Screen GLdouble  -- ^ The screen location.
  , distanceNearPlane  :: GLdouble         -- ^ The distance to the near plane of the frustum.
  , distanceFarPlane   :: GLdouble         -- ^ The distance to the far plane of the frustum.
  , eyePosition        :: Vertex3 GLdouble -- ^ The position of the eyes.
  , eyeSeparation      :: Vector3 GLdouble -- ^ The separation between the eyes.
  , eyeUpward          :: Vector3 GLdouble -- ^ The upward direction.
  , sceneCenter        :: Vertex3 GLdouble -- ^ The center of the scene.
  }
    deriving (Eq, Read, Show)

instance Default ViewerParameters where
  def =
    ViewerParameters
    {
      screen =
        Screen
        {
          lowerLeft  = Vertex3 (-0.5) (-0.5) 0
        , lowerRight = Vertex3   0.5  (-0.5) 0
        , upperLeft  = Vertex3 (-0.5)   0.5  0
        }
    , distanceNearPlane  = 0.1
    , distanceFarPlane   = 100
    , eyePosition        = Vertex3 0   0 1
    , eyeSeparation      = Vector3 0.2 0 0
    , eyeUpward          = Vector3 0   1 0
    , sceneCenter        = Vertex3 0   0 0
    }


-- | Construct viewer geometry from physical geometry.
viewerGeometry :: GLdouble         -- ^ The width of the screen or display.
               -> GLdouble         -- ^ The height of the screen or display.
               -> GLdouble         -- ^ The distance from the eyes to the screen or display.
               -> ViewerParameters -- ^ The corresponding viewer parameters.
viewerGeometry width height throw =
  def
  {
    screen =
      Screen
      {
        lowerLeft  = Vertex3 (- 1 / 2) (- height / width / 2) 0
      , lowerRight = Vertex3 (  1 / 2) (- height / width / 2) 0
      , upperLeft  = Vertex3 (- 1 / 2) (  height / width / 2) 0
      }
  , eyePosition = Vertex3 0 0 (throw / width)
  }


-- | Viewer parameters for a typical smartphone screen.
phoneViewer :: ViewerParameters
phoneViewer = viewerGeometry 5.27 2.80 12


-- | Viewer parameters for a typical laptop screen.
laptopViewer :: ViewerParameters
laptopViewer = viewerGeometry 13.625 7.875 24


-- | Viewer parameters for a typical desktop monitor.
desktopViewer :: ViewerParameters
desktopViewer = viewerGeometry 20.75 11.625 32


-- | Viewer parameters for a typical projector.
projectorViewer :: ViewerParameters
projectorViewer = viewerGeometry 1.6 1.0 (1.5 * 1.6)


displayAspectRatio :: ViewerParameters
                   -> GLdouble
displayAspectRatio ViewerParameters{..} = aspectRatio screen


displayThrowRatio :: ViewerParameters
                  -> GLdouble
displayThrowRatio ViewerParameters{..} = throwRatio screen eyePosition


-- | Compute the field of view for viewer parameters.
fieldOfView :: ViewerParameters -- ^ The viewer parameters
            -> GLdouble         -- ^ The field of view, in degrees.
fieldOfView ViewerParameters{..} = 2 * atan2 0.5 (throwRatio screen eyePosition) * degree


-- | Construct a reshape callback from viewer parameters.  This simply sets the frustum based on the viewer parameters and the size of the viewport.
reshape :: ViewerParameters -- ^ The viewer parameters.
        -> ReshapeCallback  -- ^ The reshape callback.
reshape ViewerParameters{..} wh = 
  do
    viewport $=! (Position 0 0, wh)
    matrixMode $=! Projection
    loadIdentity
    projection screen eyePosition distanceNearPlane distanceFarPlane
    matrixMode $=! Modelview 0


-- | Create an action look at the scene according to the viewer parameters.
loadViewer :: ViewerParameters -- ^ The viewer parameters.
           -> DlpEye           -- ^ The eye from which to view.
           -> IO ()            -- ^ An action for looking at the scene using the specified eye and viewer parameters.
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


-- | Construct a DLP display from a display callback.
dlpViewerDisplay :: DlpEncoding      -- ^ The DLP encoding.
                 -> ViewerParameters -- ^ The viewer parameters.
                 -> DisplayCallback  -- ^ The display callback.
                 -> DlpDisplay       -- ^ The DLP display data for using the specified encoding, viewer parameters, and display callback.
dlpViewerDisplay encoding viewerParameters display =
  def 
    {
      dlpEncoding = encoding
    , doDisplay = \eye -> loadViewer viewerParameters eye >> display
    }
