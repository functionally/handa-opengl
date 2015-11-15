{-|
Module      :  Graphics.Rendering.Handa.Util
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Miscellaneous functions for OpenGL rendering.
-}


module Graphics.Rendering.Handa.Util (
  -- * Trigonometry
  degree
, cosd
, sind
, color4
  -- * Analytic Geometry
, vertex3
, vector3
) where


import Graphics.Rendering.OpenGL.GL (Color4(..), ColorComponent, Vector3(..), Vertex3(..), VertexComponent, color, vertex)


-- | One degree of arc.
degree :: Floating a => a
degree = 180 / pi


-- | Cosine with its argument in degrees.
cosd :: Floating a => a -> a
cosd = cos . (/ degree)


-- | Sine with its argument in degrees.
sind :: Floating a => a -> a
sind = sin . (/ degree)


-- | Action for a four-component color from a tuple.
color4 :: ColorComponent a => (a, a, a, a) -> IO ()
color4 (r, g, b, a) = color $ Color4 r g b a


-- | Action for a three-component vertex from a tuple.
vertex3 :: VertexComponent a => (a, a, a) -> IO ()
vertex3 (x, y, z) = vertex $ Vertex3 x y z


-- | Construct a three-component vector from a tuple.
vector3 :: (a, a, a) -> Vector3 a
vector3 (x, y, z) = Vector3 x y z
