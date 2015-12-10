{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Graphics.Rendering.Handa.Projection (
  Screen(..)
, aspectRatio
, throwRatio
, projection
) where


import Data.AdditiveGroup (AdditiveGroup)
import Data.AffineSpace ((.-.))
import Data.Cross (cross3)
import Data.VectorSpace ((<.>), magnitude, normalized)
import Graphics.Rendering.OpenGL (GLmatrix, MatrixComponent, MatrixOrder(RowMajor), Vector3(..), Vertex3(..), frustum, multMatrix, newMatrix, translate)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances (origin)


data Screen a =
  Screen
  {
    lowerLeft  :: Vertex3 a
  , lowerRight :: Vertex3 a
  , upperLeft  :: Vertex3 a
  }
    deriving (Eq, Read, Show)


aspectRatio :: (AdditiveGroup a, Floating a, Fractional a) => Screen a -> a
aspectRatio Screen{..} =
  let
    width  = magnitude $ lowerRight .-. lowerLeft
    height = magnitude $ upperLeft  .-. lowerLeft
  in
    width / height


throwRatio :: (AdditiveGroup a, Floating a, Num a, Real a) => Screen a -> Vertex3 a -> a
throwRatio Screen{..} eye =
  let
    width  = magnitude $ lowerRight .-. lowerLeft
    vn = normalized (lowerRight .-. lowerLeft) `cross3` normalized (upperLeft  .-. lowerLeft)
    throw = - (lowerLeft  .-. eye) <.> vn
  in
    throw / width


-- See <http://csc.lsu.edu/~kooima/pdfs/gen-perspective.pdf>.
projection :: forall a . (AdditiveGroup a, Floating a, MatrixComponent a, Num a, Real a)
           => Screen a
           -> Vertex3 a
           -> a
           -> a
           -> IO ()
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
