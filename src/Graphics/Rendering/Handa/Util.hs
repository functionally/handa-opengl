module Graphics.Rendering.Handa.Util (
  degree
, cosd
, sind
, color4
, vertex3
, vector3
) where


import Graphics.Rendering.OpenGL.GL (Color4(..), ColorComponent, Vector3(..), Vertex3(..), VertexComponent, color, vertex)


degree :: Floating a => a
degree = 180 / pi


cosd :: Floating a => a -> a
cosd = cos . (/ degree)


sind :: Floating a => a -> a
sind = sin . (/ degree)


color4 :: ColorComponent a => (a, a, a, a) -> IO ()
color4 (r, g, b, a) = color $ Color4 r g b a


vertex3 :: VertexComponent a => (a, a, a) -> IO ()
vertex3 (x, y, z) = vertex $ Vertex3 x y z


vector3 :: (a, a, a) -> Vector3 a
vector3 (x, y, z) = Vector3 x y z
