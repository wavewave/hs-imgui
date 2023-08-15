{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Extra (whenM, whileM)
import Data.Bits ((.|.))
import Data.Foldable (for_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.String (IsString (..))
import FFICXX.Runtime.Cast (FPtr (..))
import Foreign.C.String (CString, newCString, withCString)
import Foreign.C.Types (CBool (..), CFloat, CUInt (..))
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
    imGuiIO_Framerate_get,
  )
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import ImGui.ImVec4.Implementation (imVec4_w_get, imVec4_x_get, imVec4_y_get, imVec4_z_get)
import STD.Deletable (delete)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

foreign import ccall unsafe "toImU32"
  c_toImU32 :: ImColor -> IO CUInt

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

-- Demonstrate using the low-level ImDrawList to draw custom shapes.
showExampleAppCustomRendering :: ImVec4 -> IO ()
showExampleAppCustomRendering colf = do
  begin ("Custom rendering" :: CString) nullPtr
  beginTabBar ("##TabBar" :: CString)
  whenM (toBool <$> beginTabItem ("Primitives" :: CString)) $ do
    fontSize <- getFontSize
    pushItemWidth (-fontSize * 15)
    draw_list <- getWindowDrawList

    -- Draw gradients
    -- (note that those are currently exacerbating our sRGB/Linear issues)
    -- Calling ImGui::GetColorU32() multiplies the given colors by the current Style Alpha, but you may pass the IM_COL32() directly as well..

    {- textUnformatted ("Gradients"::CString)
    w <- calcItemWidth
    h <- getFrameHeight
    gradient_size <- newImVec2 w h
    ...
    -}

    -- Draw a bunch of primitives
    textUnformatted ("All primitives" :: CString)
    {- let sz = 36.0
        thickness = 3.0
        int ngon_side = 6
        circle_segments_override = False
        ..
     -}
    let sz = 36.0
        circle_segments = 0
        thickness = 3.0
        ngon_sides = 6
        spacing = 10
        corners_tl_br =
          fromEnum ImDrawFlags_RoundCornersTopLeft
            .|. fromEnum ImDrawFlags_RoundCornersBottomRight
        rounding = sz / 5.0
    colorEdit4 ("Color" :: CString) (castPtr (get_fptr colf))
    p <- getCursorScreenPos
    px <- imVec2_x_get p
    py <- imVec2_y_get p
    let x = px + 4
        y = py + 4
    col_ <- newImColor colf
    col <- c_toImU32 col_
    for_ [0, 1] $ \n -> do
      let th
            | n == 0 = 1
            | otherwise = thickness
      let ngon (x', y') = do
            v <- newImVec2 (x' + sz * 0.5) (y' + sz * 0.5)
            imDrawList_AddNgon draw_list v (sz * 0.5) col ngon_sides th
            delete v
          circle (x', y') = do
            v <- newImVec2 (x' + sz * 0.5) (y' + sz * 0.5)
            imDrawList_AddCircle draw_list v (sz * 0.5) col circle_segments th
            delete v
          rect rnd flag (x', y') = do
            v1 <- newImVec2 x' y'
            v2 <- newImVec2 (x' + sz) (y' + sz)
            imDrawList_AddRect draw_list v1 v2 col rnd flag th
            delete v1
            delete v2
          actions =
            [ ngon,
              circle,
              rect 0.0 (fromIntegral (fromEnum ImDrawFlags_None)),
              rect rounding (fromIntegral (fromEnum ImDrawFlags_None)),
              rect rounding (fromIntegral corners_tl_br)
            ]

      let y' = y + n * (sz + spacing)
      for_ (zip [0 ..] actions) $ \(m, action) -> do
        let x' = x + m * (sz + spacing)
        action (x', y')

    popItemWidth
    endTabItem
  whenM (toBool <$> beginTabItem ("Canvas" :: CString)) $ do
    endTabItem
  whenM (toBool <$> beginTabItem ("BG/FG draw list" :: CString)) $ do
    endTabItem
  endTabBar
  end

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
  colf <- newImVec4 1.0 1.0 0.4 1.0

  -- main loop
  whileM $ do
    glfwPollEvents
    -- Start the Dear ImGui frame
    imGui_ImplOpenGL3_NewFrame
    imGui_ImplGlfw_NewFrame
    newFrame

    showExampleAppCustomRendering colf

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
