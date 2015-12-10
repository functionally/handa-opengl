{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Graphics.Rendering.Handa.Projection (
  projection
) where


import Data.AdditiveGroup (AdditiveGroup)
import Data.AffineSpace ((.-.))
import Data.Cross (cross3)
import Data.VectorSpace ((<.>), normalized)
import Graphics.Rendering.OpenGL (GLmatrix, MatrixComponent, MatrixOrder(RowMajor), Vector3(..), Vertex3(..), frustum, multMatrix, newMatrix, translate)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances ()


origin :: Num a => Vertex3 a
origin = Vertex3 0 0 0


-- See <http://csc.lsu.edu/~kooima/pdfs/gen-perspective.pdf>.
projection :: forall a . (AdditiveGroup a, Floating a, MatrixComponent a, Num a, Real a)
           => Vertex3 a
           -> Vertex3 a
           -> Vertex3 a
           -> Vertex3 a
           -> a
           -> a
           -> IO ()
projection lowerLeft lowerRight upperLeft eye near far =
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
      distance = - va <.> vn
      -- Extent on near clipping plane.
      scaling = near / distance
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
