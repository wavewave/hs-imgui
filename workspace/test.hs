{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.String (IsString (..))
import Foreign.C.String (CString, newCString)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (Ptr)
import ImGui
import ImGui.Enum -- (c_test, c_test2) --  (c_none, c_notitlebar)
import System.IO.Unsafe      ( unsafePerformIO )

foreign import ccall unsafe "glfw_initialize"
  c_glfw_initialize :: IO GLFWwindow

foreign import ccall unsafe "glfw_finalize"
  c_glfw_finalize :: GLFWwindow -> IO ()

foreign import ccall unsafe "imgui_io_shim"
  c_imgui_io_shim :: ImGuiIO -> IO ()

foreign import ccall unsafe "imgui_main"
  c_imgui_main :: GLFWwindow -> ImGuiIO -> IO ()

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
  -- main loop
  c_imgui_main window io

  -- Cleanup
  imGui_ImplOpenGL3_Shutdown
  imGui_ImplGlfw_Shutdown
  destroyContext ctxt
  c_glfw_finalize window
