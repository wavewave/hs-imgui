{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Extra (whileM)
import Data.Bits ((.|.))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.String (IsString (..))
import FFICXX.Runtime.Cast (Castable (..), cast_fptr_to_obj)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CFloat)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable (peek))
import ImGui
import ImGui.Enum
import ImGui.ImGuiIO.Implementation
  ( imGuiIO_ConfigFlags_get,
    imGuiIO_ConfigFlags_set,
    imGuiIO_Framerate_get,
  )
import ImGui.ImGuiViewport.Implementation
  ( imGuiViewport_WorkPos_get,
    imGuiViewport_WorkSize_get,
  )
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import ImGui.ImVec4.Implementation (imVec4_w_get, imVec4_x_get, imVec4_y_get, imVec4_z_get)
import STD.Deletable (delete)
import System.IO.Unsafe (unsafePerformIO)

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

eachFrameRender :: GLFWwindow -> (IORef CFloat, IORef CFloat) -> IO ()
eachFrameRender window (w_ref, h_ref) = do
  glfwPollEvents
  -- Start the Dear ImGui frame
  imGui_ImplOpenGL3_NewFrame
  imGui_ImplGlfw_NewFrame
  newFrame

  viewport <- getMainViewport
  let flags =
        fromIntegral $
          fromEnum ImGuiWindowFlags_NoDecoration
            .|. fromEnum ImGuiWindowFlags_NoMove
            .|. fromEnum ImGuiWindowFlags_NoSavedSettings
  pos <- imGuiViewport_WorkPos_get viewport
  size <- imGuiViewport_WorkSize_get viewport
  zero <- newImVec2 0 0
  setNextWindowPos pos 0 zero
  setNextWindowSize size 0
  begin ("fullscreen" :: CString) nullPtr flags
  (w, h) <- (,) <$> readIORef w_ref <*> readIORef h_ref
  pushStyleVar2 (fromIntegral (fromEnum ImGuiStyleVar_ItemSpacing)) zero
  --
  child1_size <- newImVec2 w h
  beginChild ("child1" :: CString) child1_size (fromBool True) 0
  endChild
  delete child1_size
  --
  sameLine_
  --
  vsplitter_size <- newImVec2 8.0 h
  invisibleButton ("vsplitter" :: CString) vsplitter_size 0
  delete vsplitter_size
  --
  sameLine_
  --
  child2_size <- newImVec2 0 h
  beginChild ("child2" :: CString) child2_size (fromBool True) 0
  endChild
  delete child2_size
  --
  hsplitter_size <- newImVec2 (-1.0) 8.0
  invisibleButton ("hsplitter" :: CString) hsplitter_size 0
  delete hsplitter_size
  --
  beginChild ("child3" :: CString) zero (fromBool True) 0
  endChild
  --
  popStyleVar_
  end
  delete zero
  render

main :: IO ()
main = do
  let glsl_version :: CString
      glsl_version = "#version 150"
  glfwInit
  glfwWindowHint (0x22002 {- GLFW_CONTEXT_VERSION_MAJOR -}) 3
  glfwWindowHint (0x22003 {- GLFW_CONTEXT_VERSION_MINOR -}) 2
  -- 3.2+ only
  glfwWindowHint (0x22008 {- GLFW_OPENGL_PROFILE -}) (0x32001 {- GLFW_OPENGL_CORE_PROFILE -})
  -- Required on Mac
  glfwWindowHint (0x22006 {- GLFW_OPENGL_FORWARD_COMPAT -}) (1 {- GL_TRUE -})
  window :: GLFWwindow <-
    glfwCreateWindow
      1280
      720
      ("Dear ImGui GLFW+OpenGL3 example" :: CString)
      (cast_fptr_to_obj nullPtr :: GLFWmonitor)
      (cast_fptr_to_obj nullPtr :: GLFWwindow)
  glfwMakeContextCurrent window
  -- Enable vsync
  glfwSwapInterval 1
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
  ref_w <- newIORef 200
  ref_h <- newIORef 300

  -- main loop
  whileM $ do
    eachFrameRender window (ref_w, ref_h)
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
        glClearColor (x * w) (y * w) (z * w) w
        glClear 0x4000 {- GL_COLOR_BUFFER_BIT -}
    imGui_ImplOpenGL3_RenderDrawData =<< getDrawData
    glfwSwapBuffers window

    not . toBool <$> glfwWindowShouldClose window

  -- Cleanup
  imGui_ImplOpenGL3_Shutdown
  imGui_ImplGlfw_Shutdown
  destroyContext ctxt

  glfwDestroyWindow window
  glfwTerminate
