{-|
Module      :  Graphics.Rendering.Handa.Projection
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Functions for off-axis projection.
-}


{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Graphics.Rendering.Handa.Projection (
-- * Screens.
  Screen(..)
, upperRight
, aspectRatio
, throwRatio
-- * Projections.
, OffAxisProjection(..)
, projection
) where


import Data.AdditiveGroup (AdditiveGroup)
import Data.Aeson (FromJSON)
import Data.AffineSpace ((.+^), (.-.))
import Data.Binary (Binary)
import Data.Cross (cross3)
import Data.Data (Data)
import Data.VectorSpace ((<.>), magnitude, normalized)
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (GLmatrix, MatrixComponent, MatrixOrder(RowMajor), Vector3(..), Vertex3(..), frustum, multMatrix, newMatrix, translate)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances (origin)
import Numeric.LinearAlgebra ((<>), (#>), cross, fromColumns, fromList, fromLists, inv, norm_2, scalar, toList, toLists)


-- | Description of a physical screen geometry.
data Screen a =
  Screen
  {
    lowerLeft  :: Vertex3 a -- ^ The lower left corner.
  , lowerRight :: Vertex3 a -- ^ The lower right corner.
  , upperLeft  :: Vertex3 a -- ^ The upper left corner.
  }
    deriving (Binary, Data, Eq, FromJSON, Generic, Read, Show)

instance Functor Screen where
  fmap f Screen{..} =
    Screen
    {
      lowerLeft  = fmap f lowerLeft
    , lowerRight = fmap f lowerRight
    , upperLeft  = fmap f upperLeft
    }


-- | The upper right corner.
upperRight :: (Num a)
           => Screen a
           -> Vertex3 a
upperRight Screen{..} = lowerRight .+^ (upperLeft .-. lowerLeft)


-- | The aspect ratio.
aspectRatio :: (AdditiveGroup a, RealFloat a)
            => Screen a -- ^ The screen geometry.
            -> a        -- ^ The aspect ratio, namely the screen width divided by its height.
aspectRatio Screen{..} =
  let
    width  = magnitude $ lowerRight .-. lowerLeft
    height = magnitude $ upperLeft  .-. lowerLeft
  in
    width / height


-- | The throw ratio.
throwRatio :: (AdditiveGroup a, RealFloat a)
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


-- | The equations to use for off-axis projection.
data OffAxisProjection =
    KooimaOffAxis -- ^ Based on Kooima 2009, \<<http://csc.lsu.edu/~kooima/pdfs/gen-perspective.pdf>\>, which assumes a rectangular screen .
  | VTKOffAxis    -- ^ Based on VTK 6.3.0, \<<https://gitlab.kitware.com/vtk/vtk/blob/v6.3.0/Rendering/Core/vtkCamera.cxx#L414>\>, which does not assume a rectangular screen.
    deriving (Eq, Read, Show)


-- | Make an off-axis projection for a screen.
projection :: forall a . (AdditiveGroup a, MatrixComponent a, RealFloat a)
           => OffAxisProjection -- ^ The off-axis equations to use.
           -> Screen a          -- ^ The screen geometry.
           -> Vertex3 a         -- ^ The eye position.
           -> a                 -- ^ The distance to the near culling plane.
           -> a                 -- ^ The distance to the far culling plane.
           -> IO ()             -- ^ An action for performing the off-axis projection.

-- Based on Kooima 2009, \<<http://csc.lsu.edu/~kooima/pdfs/gen-perspective.pdf>\>, which assumes a rectangular screen .
projection KooimaOffAxis Screen{..} eye near far =
  do
    let
      -- Orthonomal basis for screen.
      vr = normalized $ lowerRight .-. lowerLeft
      vu = normalized $ upperLeft  .-. lowerLeft
      vn = normalized $ vr `cross3` vu
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

-- Based on VTK 6.3.0, \<<https://gitlab.kitware.com/vtk/vtk/blob/v6.3.0/Rendering/Core/vtkCamera.cxx#L414>\>, which does not assume a rectangular screen.
projection VTKOffAxis s@Screen{..} eye near far =
  do
    -- FIXME: Rewrite this using `Graphics.Rendering.OpenGL.CoordTrans` instead of `Numerics.LinearAlgebra`.
    let
      normalize v = v / scalar (norm_2 v)
      fromVertex3 (Vertex3 x y z) = fromList [x, y, z]
      toHomogeneous w = fromList . (++ [w]) . toList
      toPoint = toHomogeneous 1
      toDirection = toHomogeneous 0
      -- The corners and eye in R^3.
      lowerLeft'  = fromVertex3 (realToFrac <$> lowerLeft   )
      lowerRight' = fromVertex3 (realToFrac <$> lowerRight  )
      upperRight' = fromVertex3 (realToFrac <$> upperRight s)
      eye'        = fromVertex3 (realToFrac <$> eye         )
      -- The basis for the screen, not necessarily orthonormal.
      vr = normalize $ lowerRight' - lowerLeft'
      vu = normalize $ upperRight' - lowerRight'
      vn = normalize $ vr `cross` vu
      -- The world to screen matrix.
      w2s =
        inv
          $ fromColumns
          [
            toDirection vr
          , toDirection vu
          , toDirection vn
          , toPoint lowerLeft'
          ]
      -- Eye position and screen edges.
      [ex, ey, ez, _] = toList $ w2s #> toPoint eye'
      [hx, hy, _ , _] = toList $ w2s #> toPoint upperRight'
      [lx, ly, _ , _] = toList $ w2s #> toPoint lowerLeft'
      -- The screen size.
      width  = hx - lx
      height = hy - ly
      -- The front, back, and depth.
      f = ez - realToFrac far
      b = ez - realToFrac near
      depth = b - f
      -- The frustum-like projection.
      p =
        fromLists
          [
            [2 * ez / width,               0, (hx + lx - 2 * ex) / width ,   - ez * (hx + lx) / width           ]
          , [             0, 2 * ez / height, (hy + ly - 2 * ey) / height,   - ez * (hy + ly) / height          ]
          , [             0,               0, (b  + f  - 2 * ez) / depth , b - ez - b * (b + f - 2 * ez) / depth]
          , [             0,               0,                          -1,     ez                               ]
          ]
      -- The overall projection.
      p' = p <> w2s
    multMatrix =<< (newMatrix RowMajor $ map realToFrac $ concat $ toLists p' :: IO (GLmatrix a))
