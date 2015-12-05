{-|
Module      :  Graphics.Rendering.Handa.Shape
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Functions and types for managing shapes represented as vertex buffer objects (VBOs).
-}


{-# LANGUAGE RecordWildCards #-}


module Graphics.Rendering.Handa.Shape (
  -- * Types
  Shape
  -- * Functions
, makeShape
, remakeShape
, drawShape
) where


import Data.Array.Storable (newListArray, withStorableArray)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable, sizeOf)
import Graphics.Rendering.OpenGL (DataType, BufferObject, BufferTarget(ArrayBuffer), BufferUsage(StaticDraw), Capability(..), ClientArrayType(VertexArray), NumArrayIndices, NumComponents, PrimitiveMode, Stride, VertexArrayDescriptor(..), ($=), arrayPointer, bindBuffer, bufferData, drawArrays, clientState, genObjectNames)


-- | A shape stored as a vertex buffer object (VBO).
data Shape =
  Shape
  {
    vbo         :: BufferObject    -- ^ The handle to the vertex buffer object.
  , components  :: NumComponents   -- ^ The number of components for the shape's vertices.
  , dataType    :: DataType        -- ^ The data type of the shape's vertices.
  , stride      :: Stride          -- ^ The stride, the number of components between consecutive vertices.
  , size        :: NumArrayIndices -- ^ The number of vertices.
  , mode        :: PrimitiveMode   -- ^ The type of primitive.
  , priorAction :: IO ()           -- ^ The display action to be executed prior to rendering the shape.
  }


-- | Construct a shape.
makeShape :: Storable a
          => NumComponents -- ^ The number of components per vertex.
          -> DataType      -- ^ The data type for the vertices' components.
          -> PrimitiveMode -- ^ The type of primitive.
          -> [a]           -- ^ The vertices.
          -> IO ()         -- ^ The display action to be executed prior to rendering the shape.
          -> IO Shape      -- ^  An action for the shape.
makeShape components dataType mode vertices priorAction =
  let
    stride = undefined
    size = undefined
  in do
    vbo <- head <$> genObjectNames 1
    remakeShape Shape{..} vertices


-- | Reconstruct a shape by replacing its vertices.
remakeShape :: Storable a
            => Shape      -- ^ The shape.
            -> [a]        -- ^ The replacement vertices.
            -> IO Shape   -- ^ An action for the updated shape.
remakeShape shape@Shape{..} vertices =
  let
    n = length vertices
    m =
      if null vertices
        then 0
        else sizeOf $ head vertices
    ptrSize = toEnum $ n * m
  in do
    bindBuffer ArrayBuffer $= Just vbo
    vertices' <- newListArray (0, n - 1) vertices
    withStorableArray vertices' $ \ptr -> bufferData ArrayBuffer $= (ptrSize, ptr, StaticDraw)
    bindBuffer ArrayBuffer $= Nothing
    return $ shape {size = toEnum n, stride = toEnum m}


-- | Render a shape.    
drawShape :: Shape -- ^ The shape.
          -> IO () -- ^ An action to render the shape, also executing its prior actions before rendering it.
drawShape Shape{..} =
  do
    clientState VertexArray $= Enabled
    bindBuffer ArrayBuffer $= Just vbo
    arrayPointer VertexArray $= VertexArrayDescriptor components dataType stride nullPtr
    priorAction
    drawArrays mode 0 size
    bindBuffer ArrayBuffer $= Nothing
    clientState VertexArray $= Disabled
