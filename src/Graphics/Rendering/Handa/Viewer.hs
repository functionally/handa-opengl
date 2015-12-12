{-|
Module      :  Graphics.Rendering.Handa.Viewer
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Functions for managing perspectives and frusta.
-}


{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}


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


import Data.AdditiveGroup (AdditiveGroup)
import Data.Aeson (FromJSON)
import Data.Binary (Binary(..))
import Data.Data (Data)
import Data.Default (Default, def)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import Graphics.Rendering.DLP (DlpEncoding, DlpEye(..))
import Graphics.Rendering.DLP.Callbacks (DlpDisplay(..))
import Graphics.Rendering.Handa.Projection (Screen(..), aspectRatio, projection, throwRatio)
import Graphics.Rendering.Handa.Util (degree)
import Graphics.Rendering.OpenGL (MatrixComponent, MatrixMode(..), Position(..), Vector3(..), Vertex3(..), ($=!), loadIdentity, lookAt, matrixMode, viewport)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()
import Graphics.UI.GLUT (DisplayCallback, ReshapeCallback)


-- | Paramaters specifying a viewer, including the frustum of the view.
data ViewerParameters a =
  ViewerParameters
  {
    screen        :: Screen a  -- ^ The screen location.
  , nearPlane     :: a         -- ^ The distance to the near plane of the frustum.
  , farPlane      :: a         -- ^ The distance to the far plane of the frustum.
  , eyePosition   :: Vertex3 a -- ^ The position of the eyes.
  , eyeSeparation :: Vector3 a -- ^ The separation between the eyes.
  , eyeUpward     :: Vector3 a -- ^ The upward direction.
  , sceneCenter   :: Vertex3 a -- ^ The center of the scene.
  , sceneScale    :: a         -- ^ The factor by which to scale the scene.
  }
    deriving (Binary, Data, Eq, FromJSON, Generic, Read, Show)

instance Functor ViewerParameters where
  fmap f ViewerParameters{..} =
    ViewerParameters
    {
      screen        = fmap f screen
    , nearPlane     =      f nearPlane
    , farPlane      =      f farPlane
    , eyePosition   = fmap f eyePosition
    , eyeSeparation = fmap f eyeSeparation
    , eyeUpward     = fmap f eyeUpward
    , sceneCenter   = fmap f sceneCenter
    , sceneScale    =      f sceneScale
    }

instance (Fractional a, Storable a) => Default (ViewerParameters a) where
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
    , nearPlane     = 0.1
    , farPlane      = 100
    , eyePosition   = Vertex3 0   0 1
    , eyeSeparation = Vector3 0.2 0 0
    , eyeUpward     = Vector3 0   1 0
    , sceneCenter   = Vertex3 0   0 0
    , sceneScale    = 1
    }


-- | Construct viewer geometry from physical geometry.
viewerGeometry :: (Fractional a, Storable a)
               => a                  -- ^ The width of the screen or display.
               -> a                  -- ^ The height of the screen or display.
               -> a                  -- ^ The distance from the eyes to the screen or display.
               -> ViewerParameters a -- ^ The corresponding viewer parameters.
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
phoneViewer :: (Fractional a, Storable a) => ViewerParameters a
phoneViewer = viewerGeometry 5.27 2.80 12


-- | Viewer parameters for a typical laptop screen.
laptopViewer :: (Fractional a, Storable a) => ViewerParameters a
laptopViewer = viewerGeometry 13.625 7.875 24


-- | Viewer parameters for a typical desktop monitor.
desktopViewer :: (Fractional a, Storable a) => ViewerParameters a
desktopViewer = viewerGeometry 20.75 11.625 32


-- | Viewer parameters for a typical projector.
projectorViewer :: (Fractional a, Storable a) => ViewerParameters a
projectorViewer = viewerGeometry 1.6 1.0 (1.5 * 1.6)


-- | The aspect ratio of the viewer.
displayAspectRatio :: (AdditiveGroup a, RealFloat a, Storable a)
                   => ViewerParameters a -- ^ The viewer parameters.
                   -> a                  -- ^ The aspect ratio, namely the screen width divided by its height.
displayAspectRatio ViewerParameters{..} = aspectRatio screen


-- | The throw ratio of the viewer.
displayThrowRatio :: (AdditiveGroup a, RealFloat a, Storable a)
                  => ViewerParameters a -- ^ The viewer parameters.
                  -> a                  -- ^ The throw ratio, namely the distance to the screen divided by its height.
displayThrowRatio ViewerParameters{..} = throwRatio screen eyePosition


-- | Compute the field of view for viewer parameters.
fieldOfView :: (AdditiveGroup a, RealFloat a, Storable a)
            => ViewerParameters a -- ^ The viewer parameters
            -> a                  -- ^ The field of view, in degrees.
fieldOfView ViewerParameters{..} = 2 * atan2 0.5 (throwRatio screen eyePosition) * degree


-- | Construct a reshape callback from viewer parameters.  This simply sets the frustum based on the viewer parameters and the size of the viewport.
reshape :: (AdditiveGroup a, MatrixComponent a, RealFloat a, Storable a)
        => ViewerParameters a -- ^ The viewer parameters.
        -> ReshapeCallback    -- ^ The reshape callback.
reshape ViewerParameters{..} wh = 
  do
    viewport $=! (Position 0 0, wh)
    matrixMode $=! Projection
    loadIdentity
    projection screen eyePosition nearPlane farPlane
    matrixMode $=! Modelview 0


-- | Create an action look at the scene according to the viewer parameters.
loadViewer :: (RealFloat a, Storable a)
           => ViewerParameters a -- ^ The viewer parameters.
           -> DlpEye             -- ^ The eye from which to view.
           -> IO ()              -- ^ An action for looking at the scene using the specified eye and viewer parameters.
loadViewer ViewerParameters{..} eye =
  do
    loadIdentity
    let
      offset =
        case eye of
          LeftDlp  -> -0.5
          RightDlp ->  0.5
      Vertex3  xEye  yEye  zEye = eyePosition
      Vector3 dxEye dyEye dzEye = eyeSeparation
    lookAt
      (realToFrac <$> Vertex3 (xEye + offset * dxEye) (yEye + offset * dyEye) (zEye + offset * dzEye))
      (realToFrac <$> sceneCenter)
      (realToFrac <$> eyeUpward)


-- | Construct a DLP display from a display callback.
dlpViewerDisplay :: (RealFloat a, Storable a)
                 => DlpEncoding        -- ^ The DLP encoding.
                 -> ViewerParameters a -- ^ The viewer parameters.
                 -> DisplayCallback    -- ^ The display callback.
                 -> DlpDisplay         -- ^ The DLP display data for using the specified encoding, viewer parameters, and display callback.
dlpViewerDisplay encoding viewerParameters display =
  def 
    {
      dlpEncoding = encoding
    , doDisplay = \eye -> loadViewer viewerParameters eye >> display
    }
