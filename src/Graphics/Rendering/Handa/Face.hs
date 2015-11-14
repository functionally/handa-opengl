module Graphics.Rendering.Handa.Face (
  Face
, faceToEdges
, drawFaces
, drawFrame
, coneFaces
, brickFaces
) where


import Graphics.Rendering.Handa.Util (cosd, sind, vertex3)
import Graphics.Rendering.OpenGL.GL (PrimitiveMode(..), VertexComponent, renderPrimitive)


type Face a = [(a, a, a)]


faceToEdges :: Face a -> [(a, a, a)]
faceToEdges face = concat $ zipWith ((. return) . (:)) (init face) (tail face)


drawFaces :: (Num a, VertexComponent a) => [Face a] -> IO ()
drawFaces = renderPrimitive Quads . mapM_ vertex3 . concat


drawFrame :: (Num a, VertexComponent a) => [Face a] -> IO ()
drawFrame = renderPrimitive Lines . mapM_ vertex3 . concatMap faceToEdges


coneFaces :: (Enum a, Floating a, VertexComponent a) => a -> a -> [Face a]
coneFaces height radius =
  let
    point = (0, 0, 0)
    center = (0, 0, height)
    rim = [(radius * cosd x, radius * sind x, height) | x <- [0,15..360]]
  in 
    [
      [p0, p1, p2]
    |
      (p1, p2) <- zip (tail rim) (init rim)
    , p0 <- [point, center]
    ]


brickFaces :: Num a => a -> a -> a -> [Face a]
brickFaces w h d =
  [[(0, 0, z), (w, 0, z), (w, h, z), (0, h, z)] | z <- [0, d]]
  ++
  [[(0, y, 0), (w, y, 0), (w, y, d), (0, y, d)] | y <- [0, h]]
  ++
  [[(x, 0, 0), (x, h, 0), (x, h, d), (x, 0, d)] | x <- [0, w]]
