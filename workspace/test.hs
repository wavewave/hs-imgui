{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Extra (whileM)
import Data.String (IsString (..))
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import ImGui
import ImGui.Enum
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "glfw_initialize"
  c_glfw_initialize :: IO GLFWwindow

foreign import ccall unsafe "glfw_finalize"
  c_glfw_finalize :: GLFWwindow -> IO ()

foreign import ccall unsafe "imgui_io_shim"
  c_imgui_io_shim :: ImGuiIO -> IO ()

foreign import ccall unsafe "imgui_main"
  c_imgui_main :: GLFWwindow -> ImGuiIO -> ImVec4 -> Ptr CBool -> Ptr CBool -> IO ()

foreign import ccall unsafe "glfwWindowShouldClose"
  c_glfwWindowShouldClose :: GLFWwindow -> IO CBool

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

main :: IO ()
main = do
  let glsl_version :: CString
      glsl_version = "#version 150"
  window <- c_glfw_initialize
  ctxt <- createContext
  io <- getIO

  -- Setup Dear ImGui style
  -- styleColorsDark
  styleColorsLight

  -- Setup Platform/Renderer backends
  imGui_ImplGlfw_InitForOpenGL window (fromBool True)
  imGui_ImplOpenGL3_Init glsl_version

  c_imgui_io_shim io

  -- Our state
  clear_color <- newImVec4 0.45 0.55 0.60 1.00
  alloca $ \p_showDemoWindow ->
    alloca $ \p_showAnotherWindow -> do
      poke p_showDemoWindow (fromBool True)
      poke p_showAnotherWindow (fromBool False)
      -- main loop
      whileM $ do
        c_imgui_main window io clear_color p_showDemoWindow p_showAnotherWindow
        not . toBool <$> c_glfwWindowShouldClose window

  -- Cleanup
  imGui_ImplOpenGL3_Shutdown
  imGui_ImplGlfw_Shutdown
  destroyContext ctxt
  c_glfw_finalize window
