{-|
Module      :  $Header$
Copyright   :  (c) 2015-17 Brian W Bush
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
, fetchProjection
) where


import Data.AdditiveGroup (AdditiveGroup)
import Data.Aeson.Types (FromJSON, ToJSON)
import Data.AffineSpace ((.+^), (.-.))
import Data.Binary (Binary)
import Data.Cross (cross3)
import Data.Data (Data)
import Data.List.Split (chunksOf)
import Data.VectorSpace ((*^), (<.>), magnitude, normalized)
import GHC.Generics (Generic)
import Graphics.Rendering.OpenGL (GLmatrix, MatrixComponent, MatrixOrder(RowMajor), Vector3(..), Vertex3(..), frustum, get, getMatrixComponents, matrix, multMatrix, newMatrix, translate)
import Graphics.Rendering.OpenGL.GL.Tensor.Instances (origin)


-- | Description of a physical screen geometry.
data Screen a =
  Screen
  {
    lowerLeft  :: Vertex3 a -- ^ The lower left corner.
  , lowerRight :: Vertex3 a -- ^ The lower right corner.
  , upperLeft  :: Vertex3 a -- ^ The upper left corner.
  }
    deriving (Binary, Data, Eq, FromJSON, Generic, Read, Show, ToJSON)

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
    KooimaOffAxis -- ^ Based on Kooima 2009, \<<http://csc.lsu.edu/~kooima/pdfs/gen-perspective.pdf>\>, which assumes a rectangular screen.
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
    -- Perpendicator projection.
    frustum left right bottom top (realToFrac near) (realToFrac far)
    -- Rotate to non-perpendicular.
    multMatrix =<< (newMatrix RowMajor $ concat m :: IO (GLmatrix a))
    -- Move apex of frustum.
    translate $ origin .-. eye

-- Rewrite of VTK 6.3.0, \<<https://gitlab.kitware.com/vtk/vtk/blob/v6.3.0/Rendering/Core/vtkCamera.cxx#L414>\>, which does not assume a rectangular screen, in cleaner notation and using vector algebra.
projection VTKOffAxis s@Screen{..} eye near far =
  do
    let
      -- Orthonomal basis for screen.
      vr = normalized $ lowerRight   .-. lowerLeft
      vu = normalized $ upperRight s .-. lowerRight
      vn = normalized $ vr `cross3` vu
      -- Basis for inverse.
      idet = 1 / (vr <.> vu `cross3` vn)
      ur = idet *^ vu `cross3` vn
      uu = idet *^ vn `cross3` vr
      un = idet *^ vr `cross3` vu
      -- Screen corners relative to eye.
      va = lowerLeft    .-. eye
      vd = upperRight s .-. eye
      -- Distance from eye to screen.
      throw = - va <.> un
      -- Extent on near clipping plane.
      scaling = near / throw
      left   = realToFrac $ (ur <.> va) * scaling
      right  = realToFrac $ (ur <.> vd) * scaling
      bottom = realToFrac $ (uu <.> va) * scaling
      top    = realToFrac $ (uu <.> vd) * scaling
      -- Matrix transforming world to screen.
      m = [[x, y, z, 0] | Vector3 x y z <- [ur, uu, un]] ++ [[0, 0, 0, 1]]
    -- Perpendicator projection.
    frustum left right bottom top (realToFrac near) (realToFrac far)
    -- Rotate to non-perpendicular.
    multMatrix =<< (newMatrix RowMajor $ concat m :: IO (GLmatrix a))
    -- Move apex of frustum.
    translate $ origin .-. eye


-- | Retrieve the current projection matrix.
fetchProjection :: forall a . (MatrixComponent a, RealFloat a)
                => IO [[a]] -- ^ An action to retrieve the projection matrix, in row-major order.
fetchProjection =
  do
    m <- get $ matrix Nothing :: IO (GLmatrix a)
    chunksOf 4 <$> getMatrixComponents RowMajor m
