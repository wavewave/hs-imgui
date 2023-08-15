{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Extra (whenM, whileM)
import Data.Bits ((.|.))
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.String (IsString (..))
import FFICXX.Runtime.Cast (get_fptr)
import Foreign.C.String (CString, newCString, withCString)
import Foreign.C.Types (CBool (..), CFloat)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import ImGui
import ImGui.Enum
import ImGui.ImGuiIO.Implementation
  ( imGuiIO_ConfigFlags_get,
    imGuiIO_ConfigFlags_set,
    imGuiIO_Framerate_get
  )
import ImGui.ImVec4.Implementation (imVec4_x_get, imVec4_y_get, imVec4_z_get, imVec4_w_get)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

foreign import ccall unsafe "glfw_initialize"
  c_glfw_initialize :: IO GLFWwindow

foreign import ccall unsafe "glfwWindowShouldClose"
  c_glfwWindowShouldClose :: GLFWwindow -> IO CBool

foreign import ccall unsafe "glfwPollEvents"
  c_glfwPollEvents :: IO ()

foreign import ccall unsafe "glfwSwapBuffers"
  c_glfwSwapBuffers :: GLFWwindow -> IO ()

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

  flags <- imGuiIO_ConfigFlags_get io
  -- Enable Keyboard Controls and Gamepad Controls
  let flags' =
        flags
          .|. fromIntegral (fromEnum ImGuiConfigFlags_NavEnableKeyboard)
          .|. fromIntegral (fromEnum ImGuiConfigFlags_NavEnableGamepad)
  imGuiIO_ConfigFlags_set io flags'

  -- Our state
  clear_color <- newImVec4 0.45 0.55 0.60 1.00
  alloca $ \p_showDemoWindow ->
    alloca $ \p_showAnotherWindow -> do
      allocaArray 3 $ \p_f -> do
        poke p_showDemoWindow (fromBool True)
        poke p_showAnotherWindow (fromBool False)
        ref_counter <- newIORef (0 :: Int)

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

          -- 2. Show a simple window that we create ourselves. We use a Begin/End pair to create a named window.

          -- Create a window called "Hello, world!" and append into it.
          begin ("Hello, world!" :: CString) nullPtr
          -- Display some text (you can use a format strings too)
          textUnformatted ("This is some useful text." :: CString)
          -- Edit bools storing our window open/close state
          checkbox ("Demo Window" :: CString) p_showDemoWindow
          checkbox ("Another Window" :: CString) p_showAnotherWindow

          -- Edit 1 float using a slider from 0.0f to 1.0f
          sliderFloat ("float" :: CString) p_f 0.0 1.0

          -- Edit 3 floats representing a color
          let p_clear_color = get_fptr clear_color
          colorEdit3 ("clear color" :: CString) (castPtr p_clear_color)

          -- Buttons return true when clicked (most widgets return true when edited/activated)
          whenM (toBool <$> button ("Button" :: CString)) $
            modifyIORef' ref_counter (+ 1)
          sameLine
          counter <- readIORef ref_counter
          withCString (printf "counter = %d" counter) $ \c_str ->
            textUnformatted c_str
          framerate :: Float <- realToFrac <$> imGuiIO_Framerate_get io
          withCString (printf "Application average %.3f ms/frame (%.1f FPS)" (1000.0 / framerate) framerate) $ \c_str ->
            textUnformatted c_str
          end

          -- 3. Show another simple window.
          whenM (toBool <$> peek p_showAnotherWindow) $ do
            -- Pass a pointer to our bool variable (the window will have a closing button that will clear the bool when clicked)
            begin ("Another Window" :: CString) p_showAnotherWindow
            textUnformatted ("Hello from another window!" :: CString)
            whenM (toBool <$> button ("Close Me" :: CString)) $
              poke p_showAnotherWindow (fromBool False)
            end

          render

          -- c_draw_shim window clear_color
          alloca $ \p_dispW ->
            alloca $ \p_dispH -> do
              glfwGetFramebufferSize window p_dispW p_dispH
              dispW <- peek p_dispW
              dispH <- peek p_dispH
              glViewport 0 0 dispW dispH
              x <- imVec4_x_get clear_color
              y <- imVec4_y_get clear_color
              z <- imVec4_z_get clear_color
              w <- imVec4_w_get clear_color
              glClearColor (x*w) (y*w) (z*w) w
              glClear 0x4000 {- GL_COLOR_BUFFER_BIT -}
          imGui_ImplOpenGL3_RenderDrawData =<< getDrawData
          c_glfwSwapBuffers window

          not . toBool <$> c_glfwWindowShouldClose window

  -- Cleanup
  imGui_ImplOpenGL3_Shutdown
  imGui_ImplGlfw_Shutdown
  destroyContext ctxt

  glfwDestroyWindow window
  glfwTerminate

