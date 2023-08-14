{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr (Ptr)
import ImGui
import ImGui.Enum -- (c_test, c_test2) --  (c_none, c_notitlebar)


foreign import ccall unsafe "glfw_initialize"
  c_glfw_initialize :: IO (Ptr ())

foreign import ccall unsafe "glfw_finalize"
  c_glfw_finalize :: Ptr () -> IO ()

foreign import ccall unsafe "imgui_main"
  c_imgui_main :: Ptr () -> IO ()

main :: IO ()
main = do
  print (fromEnum ImGuiWindowFlags_None)
  print (fromEnum ImGuiWindowFlags_NoTitleBar)
  print (fromEnum ImGuiWindowFlags_NoResize)
  print (fromEnum ImGuiWindowFlags_NoMove)
  {- x <- newImGuiTextBuffer
  print c_test
  print c_test2
--   print c_notitlebar
  pure ()
 -}
  window <- c_glfw_initialize
  c_imgui_main window
  c_glfw_finalize window
