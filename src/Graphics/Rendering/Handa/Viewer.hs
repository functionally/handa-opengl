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
, glassesViewer
  -- * Callbacks and Rendering
, reshape
, loadViewer
, dlpViewerDisplay
, dlpViewerDisplay'
) where


import Data.AdditiveGroup (AdditiveGroup)
import Data.AffineSpace ((.+^))
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Data (Data)
import Data.VectorSpace ((*^))
import Data.Default (Default, def)
import Data.IORef (IORef)
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import Graphics.Rendering.DLP (DlpEncoding, DlpEye(..))
import Graphics.Rendering.DLP.Callbacks (DlpDisplay(..))
import Graphics.Rendering.Handa.Projection (OffAxisProjection(VTKOffAxis), Screen(..), aspectRatio, projection, throwRatio)
import Graphics.Rendering.Handa.Util (degree)
import Graphics.Rendering.OpenGL (GLdouble, MatrixComponent, MatrixMode(..), Position(..), Vector3(..), Vertex3(..), ($=!), get, loadIdentity, lookAt, matrixMode, scale, viewport)
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
  , sceneScale    :: Vector3 a -- ^ The factor by which to scale the scene.
  }
    deriving (Binary, Data, Eq, FromJSON, Generic, Read, Show, ToJSON)

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
    , sceneScale    = fmap f sceneScale
    }

instance (Fractional a, Storable a) => Default (ViewerParameters a) where
  def = laptopViewer


-- | Construct viewer geometry from physical geometry.
viewerGeometry :: (Fractional a, Storable a)
               => a                  -- ^ The aspect ratio (width over height) of the screen or display.
               -> a                  -- ^ The throw ratio (distance over width) of the screen or display.
               -> a                  -- ^ The distance from the eyes to the screen or display, in inches.
               -> ViewerParameters a -- ^ The corresponding viewer parameters.
viewerGeometry aspect throw distance =
    ViewerParameters
    {
    screen =
      Screen
      {
        lowerLeft  = Vertex3 (- width / 2) (- height / 2) 0
      , lowerRight = Vertex3 (  width / 2) (- height / 2) 0
      , upperLeft  = Vertex3 (- width / 2) (  height / 2) 0
      }
    , nearPlane     = 1           
    , farPlane      = 5 * distance
    , eyePosition   =         Vertex3 0                         0 distance
    , eyeSeparation =         Vector3 (typical * (1 - comfort)) 0 0
    , eyeUpward     =         Vector3 0                         1 0
    , sceneCenter   =         Vertex3 0                         0 1
    , sceneScale    = size *^ Vector3 aspect                    1 1
    }
      where
        width = distance / throw
        height = width / aspect
        margin = 0.10 -- 10% margin
        size = (1 - margin) / (1 / height + 1 / distance)
        -- See <https://www.nvidia.com/content/GTC-2010/pdfs/2010_GTC2010.pdf>:
        typical = 2.5  -- inches for typical maximum eye separation
        comfort = 0.50 -- 25% reduction in eye separation for comforat at mid-range focus


-- | Viewer parameters for a typical smartphone screen.
phoneViewer :: (Fractional a, Storable a) => ViewerParameters a
phoneViewer = viewerGeometry (1280 / 768) 0.5 18


-- | Viewer parameters for a typical laptop screen.
laptopViewer :: (Fractional a, Storable a) => ViewerParameters a
laptopViewer = viewerGeometry (1920 / 1080) 1.8 24


-- | Viewer parameters for a typical desktop monitor.
desktopViewer :: (Fractional a, Storable a) => ViewerParameters a
desktopViewer = viewerGeometry (1920 / 1080) 1.6 32


-- | Viewer parameters for a typical projector.
projectorViewer :: (Fractional a, Storable a) => ViewerParameters a
projectorViewer = viewerGeometry 1.6 1.5 36


-- | Viewer parameters for typical VR glasses.
glassesViewer :: (Fractional a, Storable a) => ViewerParameters a
glassesViewer = 
  viewerGeometry (realToFrac $ pixelWidth / pixelHeight) (distance / width) $ 0.2 * distance
    where
      distance      = 4 * 39.37
      pixelWidth    = 852
      pixelHeight   = 480
      pixelDiagonal = sqrt (pixelWidth * pixelWidth + pixelHeight * pixelHeight)
      width         = 100 * realToFrac (pixelWidth / pixelDiagonal :: Double)


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
    projection VTKOffAxis screen eyePosition nearPlane farPlane
    matrixMode $=! Modelview 0


-- | Create an action look at the scene according to the viewer parameters.
loadViewer :: (AdditiveGroup a, MatrixComponent a, RealFloat a, Storable a)
           => Bool               -- ^ Whether to use an on-axis projection.
           -> ViewerParameters a -- ^ The viewer parameters.
           -> DlpEye             -- ^ The eye from which to view.
           -> IO ()              -- ^ An action for looking at the scene using the specified eye and viewer parameters.
loadViewer onAxis ViewerParameters{..} eye =
  let
    offset =
      case eye of
        LeftDlp  -> -0.5
        RightDlp ->  0.5
    eyePosition' = eyePosition .+^ offset *^ eyeSeparation
    Vector3 sx sy sz = realToFrac <$> sceneScale :: Vector3 GLdouble
  in
    if onAxis
      then do
        loadIdentity
        lookAt
          (realToFrac <$> eyePosition')
          (realToFrac <$> sceneCenter)
          (realToFrac <$> eyeUpward)
        scale sx sy sz
      else do
        matrixMode $=! Projection
        loadIdentity
        projection VTKOffAxis screen eyePosition' nearPlane farPlane
        matrixMode $=! Modelview 0
        loadIdentity
        scale sx sy sz


-- | Construct a DLP display from a display callback.
dlpViewerDisplay :: (AdditiveGroup a, MatrixComponent a, RealFloat a, Storable a)
                 => Bool               -- ^ Whether to use on-axis projection.
                 -> DlpEncoding        -- ^ The DLP encoding.
                 -> ViewerParameters a -- ^ The viewer parameters.
                 -> DisplayCallback    -- ^ The display callback.
                 -> DlpDisplay         -- ^ The DLP display data for using the specified encoding, viewer parameters, and display callback.
dlpViewerDisplay onAxis encoding viewerParameters display =
  def 
    {
      dlpEncoding = encoding
    , doDisplay   = \eye -> loadViewer onAxis viewerParameters eye >> display
    }


-- | Construct a DLP display from a display callback.
dlpViewerDisplay' :: (AdditiveGroup a, MatrixComponent a, RealFloat a, Storable a)
                  => Bool                       -- ^ Whether to use on-axis projection.
                  -> DlpEncoding                -- ^ The DLP encoding.
                  -> IORef (ViewerParameters a) -- ^ A reference to the viewer parameters.
                  -> DisplayCallback            -- ^ The display callback.
                  -> DlpDisplay                 -- ^ The DLP display data for using the specified encoding, viewer parameters, and display callback.
dlpViewerDisplay' onAxis encoding viewerParameters display =
  def 
    {
      dlpEncoding = encoding
    , doDisplay   = \eye -> do
                      viewerParameters' <- get viewerParameters
                      loadViewer onAxis viewerParameters' eye
                      display
    }
