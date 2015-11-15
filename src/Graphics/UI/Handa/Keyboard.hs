{-|
Module      :  Graphics.UI.Handa.Keyboard
Copyright   :  (c) 2015 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable

Functions for utilizing keyboard input.
-}


module Graphics.UI.Handa.Keyboard (
  -- * Functions
  keyboardPosition
) where


import Data.IORef (IORef)
import Graphics.Rendering.OpenGL (Vector3(..), ($~!))
import Graphics.UI.GLUT (Key(..), KeyState(..), KeyboardMouseCallback, SpecialKey(..))


-- | Construct a callback for translating key presses into three dimensional movement.
--
-- *   The plus and minus keys, respectively, increment and decrement the y coordinate.
--
-- *   The right and left keys, respectively, increment and decrement the x coordinate.
--
-- *   The down and up keys, respectively, increment and decrment the z coordinate.
keyboardPosition :: Num a
                 => Vector3 a             -- ^ The amount to increment/decrement the x/y/z coordinate for each key press.
                 -> IORef (Vector3 a)     -- ^ A reference to the current position.
                 -> KeyboardMouseCallback -- ^ The GLUT keyboard/mouse callback.
keyboardPosition (Vector3 ix iy iz) location key Down _ _ =
  do
    let
      Vector3 dx dy dz = case key of
        (Char       '+'     ) -> Vector3   0     iy    0
        (Char       '-'     ) -> Vector3   0   (-iy)   0
        (SpecialKey KeyLeft ) -> Vector3 (-ix)   0     0
        (SpecialKey KeyRight) -> Vector3   ix    0     0
        (SpecialKey KeyUp   ) -> Vector3   0     0   (-iz)
        (SpecialKey KeyDown ) -> Vector3   0     0     iz
        _                     -> Vector3   0     0     0
    location $~! \(Vector3 x y z) -> Vector3 (x + dx) (y + dy) (z + dz)
keyboardPosition _ _ _ _ _ _ = return ()
