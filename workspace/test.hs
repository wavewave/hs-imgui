{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr (Ptr)
import ImGui
import ImGui.Enum -- (c_test, c_test2) --  (c_none, c_notitlebar)


foreign import ccall unsafe "glfw_initialize"
  c_glfw_initialize :: IO (Ptr ())

foreign import ccall unsafe "glfw_finalize"
  c_glfw_finalize :: Ptr () -> IO ()

foreign import ccall unsafe "imgui_main"
  c_imgui_main :: Ptr () -> ImGuiIO -> IO ()

main :: IO ()
main = do
  window <- c_glfw_initialize
  ctxt <- createContext
  io <- getIO
  styleColorsLight
  c_imgui_main window io
  destroyContext ctxt
  c_glfw_finalize window
