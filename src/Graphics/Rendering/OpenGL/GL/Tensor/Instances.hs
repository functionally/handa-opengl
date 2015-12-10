{-|
Module      :  Graphics.Rendering.OpenGL.GL.Tensor.Instances
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Instances for vector algebra.
-}


{-# LANGUAGE TypeFamilies         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Graphics.Rendering.OpenGL.GL.Tensor.Instances (
  origin
) where


import Data.AdditiveGroup (AdditiveGroup(..))
import Data.AffineSpace (AffineSpace(..))
import Data.Cross (HasCross3(..))
import Data.VectorSpace (InnerSpace(..), VectorSpace(..))
import Graphics.Rendering.OpenGL (Vector3(..), Vertex3(..))


-- | The origin of the coordinate system.
origin :: Num a => Vertex3 a
origin = Vertex3 0 0 0


instance Num a => AdditiveGroup (Vector3 a) where
  zeroV = Vector3 0 0 0
  Vector3 x y z ^+^ Vector3 x' y' z' = Vector3 (x + x') (y + y') (z + z')
  negateV (Vector3 x y z) = Vector3 (-x) (-y) (-z)


instance Num a => VectorSpace (Vector3 a) where
  type Scalar (Vector3 a) = a
  s *^ Vector3 x y z = Vector3 (s * x) (s * y) (s * z)


instance (AdditiveGroup a, Num a) => InnerSpace (Vector3 a) where
  Vector3 x y z <.> Vector3 x' y' z' = x * x' + y * y' + z * z'


instance Num a => HasCross3 (Vector3 a) where
  Vector3 x y z `cross3` Vector3 x' y' z' = Vector3 (y * z' - z * y') (z * x' - x * z') (x * y' - y * x')


instance Num a => AffineSpace (Vertex3 a) where
  type Diff (Vertex3 a) = Vector3 a
  Vertex3 x y z .-. Vertex3 x' y' z' = Vector3 (x - x') (y - y') (z - z')
  Vertex3 x y z .+^ Vector3 x' y' z' = Vertex3 (x + x') (y + y') (z + z')
