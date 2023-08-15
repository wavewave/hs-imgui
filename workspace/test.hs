{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Extra (whenM, whileM)
import Data.String (IsString (..))
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek, poke)
import ImGui
import ImGui.Enum
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "glfw_initialize"
  c_glfw_initialize :: IO GLFWwindow

foreign import ccall unsafe "glfw_finalize"
  c_glfw_finalize :: GLFWwindow -> IO ()

foreign import ccall unsafe "imgui_io_shim"
  c_imgui_io_shim :: ImGuiIO -> IO ()

foreign import ccall unsafe "glfwWindowShouldClose"
  c_glfwWindowShouldClose :: GLFWwindow -> IO CBool

foreign import ccall unsafe "glfwPollEvents"
  c_glfwPollEvents :: IO ()

foreign import ccall unsafe "glfwSwapBuffers"
  c_glfwSwapBuffers :: GLFWwindow -> IO ()

foreign import ccall unsafe "imgui_main"
  c_imgui_main :: GLFWwindow -> ImGuiIO -> ImVec4 -> Ptr CBool -> Ptr CBool -> IO ()

foreign import ccall unsafe "draw_shim"
  c_draw_shim :: GLFWwindow -> ImVec4 -> IO ()

foreign import ccall unsafe "draw_shim2"
  c_draw_shim2 :: IO ()

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
        -- Poll and handle events (inputs, window resize, etc.)
        -- You can read the io.WantCaptureMouse, io.WantCaptureKeyboard flags to tell if dear imgui wants to use your inputs.
        -- - When io.WantCaptureMouse is true, do not dispatch mouse input data to your main application, or clear/overwrite your copy of the mouse data.
        -- - When io.WantCaptureKeyboard is true, do not dispatch keyboard input data to your main application, or clear/overwrite your copy of the keyboard data.
        -- Generally you may always pass all inputs to dear imgui, and hide them from your application based on those two flags
        c_glfwPollEvents
        -- Start the Dear ImGui frame
        imGui_ImplOpenGL3_NewFrame
        imGui_ImplGlfw_NewFrame
        newFrame

        -- 1. Show the big demo window (Most of the sample code is in ImGui::ShowDemoWindow()! You can browse its code to learn more about Dear ImGui!).
        whenM (toBool <$> peek p_showDemoWindow) $
          showDemoWindow p_showDemoWindow

        -- 2.
        c_imgui_main window io clear_color p_showDemoWindow p_showAnotherWindow

        -- 3. Show another simple window.
        whenM (toBool <$> peek p_showAnotherWindow) $ do
          -- Pass a pointer to our bool variable (the window will have a closing button that will clear the bool when clicked)
          begin ("Another Window"::CString) p_showAnotherWindow
          textUnformatted ("Hello from another window!"::CString)
          whenM (toBool <$> button ("Close Me"::CString)) $
            poke p_showAnotherWindow (fromBool False)
          end

        render

        c_draw_shim window clear_color
        imGui_ImplOpenGL3_RenderDrawData =<< getDrawData
        c_glfwSwapBuffers window

        not . toBool <$> c_glfwWindowShouldClose window

  -- Cleanup
  imGui_ImplOpenGL3_Shutdown
  imGui_ImplGlfw_Shutdown
  destroyContext ctxt
  c_glfw_finalize window
