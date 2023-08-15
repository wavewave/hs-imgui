{-# LANGUAGE ForeignFunctionInterface #-}

module StorableInstances where

import Foreign.C.Types (CFloat)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import ImGui
import ImGui.ImVec2.Implementation

#include "imgui.h"

instance Storable ImVec2 where
  sizeOf _ = #{size ImVec2}
  alignment _ = alignment (undefined :: CFloat)
  poke p v = do
    x <- imVec2_x_get v
    y <- imVec2_y_get v
    #{poke ImVec2, x} p $ x
    #{poke ImVec2, y} p $ y
  peek p = do
    x <- (#{peek ImVec2, x} p)
    y <- (#{peek ImVec2, y} p)
    newImVec2 x y


