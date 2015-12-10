{-|
Module      :  Graphics.Rendering.Handa.Projection
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Functions for off-axis projection.
-}


{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Graphics.Rendering.Handa.Projection (
-- * Screens.
  Screen(..)
, aspectRatio
, throwRatio
-- * Projections.
, projection
) where


import Data.AdditiveGroup (AdditiveGroup)
import Data.AffineSpace ((.-.))
import Data.Cross (cross3)
import Data.VectorSpace ((<.>), magnitude, normalized)
import Graphics.Rendering.OpenGL (GLmatrix, MatrixComponent, MatrixOrder(RowMajor), Vector3(..), Vertex3(..), frustum, multMatrix, newMatrix, translate)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances (origin)


-- | Description of a physical screen geometry.
data Screen a =
  Screen
  {
    lowerLeft  :: Vertex3 a -- ^ The lower left corner.
  , lowerRight :: Vertex3 a -- ^ The lower right corner.
  , upperLeft  :: Vertex3 a -- ^ The upper left corner.
  }
    deriving (Eq, Read, Show)


-- | The aspect ratio.
aspectRatio :: (AdditiveGroup a, Floating a, Fractional a)
            => Screen a -- ^ The screen geometry.
            -> a        -- ^ The aspect ratio, namely the screen width divided by its height.
aspectRatio Screen{..} =
  let
    width  = magnitude $ lowerRight .-. lowerLeft
    height = magnitude $ upperLeft  .-. lowerLeft
  in
    width / height


-- | The throw ratio.
throwRatio :: (AdditiveGroup a, Floating a, Num a, Real a)
           => Screen a  -- ^ The screen geometry.
           -> Vertex3 a -- ^ The eye position.
           -> a         -- ^ The throw ratio, name the distance to the screen divided by its width.
throwRatio Screen{..} eye =
  let
    width  = magnitude $ lowerRight .-. lowerLeft
    vn = normalized (lowerRight .-. lowerLeft) `cross3` normalized (upperLeft  .-. lowerLeft)
    throw = - (lowerLeft  .-. eye) <.> vn
  in
    throw / width


-- | Make an off-axis projection for a screen.  This projection is based on the equations in \<<http://csc.lsu.edu/~kooima/pdfs/gen-perspective.pdf>\> .
projection :: forall a . (AdditiveGroup a, Floating a, MatrixComponent a, Num a, Real a)
           => Screen a  -- ^ The screen geometry.
           -> Vertex3 a -- ^ The eye position.
           -> a         -- ^ The distance to the near culling plane.
           -> a         -- ^ The distance to the far culling plane.
           -> IO ()     -- ^ An action for performing the off-axis projection.
projection Screen{..} eye near far =
  do
    let
      -- Orthonomal basis for screen.
      vr = normalized $ lowerRight .-. lowerLeft
      vu = normalized $ upperLeft  .-. lowerLeft
      vn = vr `cross3` vu
      -- Screen corners relative to eye.
      va = lowerLeft  .-. eye
      vb = lowerRight .-. eye
      vc = upperLeft  .-. eye
      -- Distance from eye to screen.
      throw = - va <.> vn
      -- Extent on near clipping plane.
      scaling = near / throw
      left   = realToFrac $ (vr <.> va) * scaling
      right  = realToFrac $ (vr <.> vb) * scaling
      bottom = realToFrac $ (vu <.> va) * scaling
      top    = realToFrac $ (vu <.> vc) * scaling
      -- Matrix transforming world to screen.
      m = [[x, y, z, 0] | Vector3 x y z <- [vr, vu, vn]] ++ [[0, 0, 0, 1]]
    -- Perpendicator projection
    frustum left right bottom top (realToFrac near) (realToFrac far)
    -- Rotate to non-perpendicular.
    multMatrix =<< (newMatrix RowMajor $ concat m :: IO (GLmatrix a))
    -- Move apex of frustum.
    translate $ origin .-. eye
