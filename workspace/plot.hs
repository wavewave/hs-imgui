{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Extra (whenM, whileM)
import Data.Bits ((.|.))
import Data.Foldable (for_, traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.String (IsString (..))
import FFICXX.Runtime.Cast (FPtr (..))
import FFICXX.Runtime.TH (IsCPrimitive (..), TemplateParamInfo (..))
import Foreign.C.String (CString, newCString, withCString)
import Foreign.C.Types (CBool (..), CDouble (..), CFloat, CInt (..), CUInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable (..))
import ImGui
import ImGui.Enum
import ImGui.ImGuiIO.Implementation
  ( imGuiIO_ConfigFlags_get,
    imGuiIO_ConfigFlags_set,
    imGuiIO_Framerate_get,
  )
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import ImGui.ImVec4.Implementation (imVec4_w_get, imVec4_x_get, imVec4_y_get, imVec4_z_get)
import qualified ImPlot
import ImPlot.Enum (ImPlotAxisFlags_ (..))
import qualified ImPlot.TH as TH
import ImPlot.Template
import STD.Deletable (delete)
import StorableInstances ()
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

-- this is a hack. but it's the best up to now.
TH.genPlotLineInstanceFor
  CPrim
  ( [t|Ptr CDouble|],
    TPInfo
      { tpinfoCxxType = "double",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "double"
      }
  )

-- this is a hack. but it's the best up to now.
TH.genPlotLineInstanceFor
  CPrim
  ( [t|Ptr CFloat|],
    TPInfo
      { tpinfoCxxType = "float",
        tpinfoCxxHeaders = [],
        tpinfoCxxNamespaces = [],
        tpinfoSuffix = "float"
      }
  )

showFramerate :: ImGuiIO -> IO ()
showFramerate io = do
  begin ("Framerate monitor" :: CString) nullPtr
  framerate :: Float <- realToFrac <$> imGuiIO_Framerate_get io
  withCString (printf "Application average %.3f ms/frame (%.1f FPS)" (1000.0 / framerate) framerate) $ \c_str ->
    textUnformatted c_str
  end

demoLinePlots :: (Ptr CFloat, Ptr CFloat) -> (Ptr CDouble, Ptr CDouble) -> IO ()
demoLinePlots (px1, py1) (px2, py2) = do
  begin ("Line plots" :: CString) nullPtr
  whenM (toBool <$> ImPlot.beginPlot ("Line Plots" :: CString)) $ do
    ImPlot.setupAxes
      ("x" :: CString)
      ("y" :: CString)
      (fromIntegral (fromEnum ImPlotAxisFlags_None))
      (fromIntegral (fromEnum ImPlotAxisFlags_None))
    t <- getTime
    for_ [0 .. 1000] $ \i -> do
      let x = fromIntegral i * 0.001
      pokeElemOff px1 i x
      pokeElemOff py1 i (0.5 + 0.5 * sin (50.0 * (x + realToFrac t / 10.0)))
    plotLine "f(x)" px1 py1 1001
    for_ [0 .. 19] $ \i -> do
      let x = fromIntegral i / 19.0
      pokeElemOff px2 i x
      pokeElemOff py2 i (x * x)
    plotLine "g(x)" px2 py2 20
    ImPlot.endPlot
  end

demoTables :: IO ()
demoTables = do
  begin ("Table of plots" :: CString) nullPtr
  let flags =
        fromIntegral $
          fromEnum ImGuiTableFlags_BordersOuter
            .|. fromEnum ImGuiTableFlags_BordersV
            .|. fromEnum ImGuiTableFlags_RowBg
            .|. fromEnum ImGuiTableFlags_Resizable
            .|. fromEnum ImGuiTableFlags_Reorderable
  beginTable ("##table" :: CString) 1 (fromIntegral flags)
  whenM (toBool <$> ImPlot.beginPlot ("1"::CString)) $ do
    ImPlot.endPlot

  endTable
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
      ("ImPlot Haskell Demo" :: CString)
      (cast_fptr_to_obj nullPtr :: GLFWmonitor)
      (cast_fptr_to_obj nullPtr :: GLFWwindow)
  glfwMakeContextCurrent window
  -- Enable vsync
  glfwSwapInterval 1
  ctxt <- createContext
  ImPlot.createImPlotContext
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

  allocaArray 1001 $ \(px1 :: Ptr CFloat) ->
    allocaArray 1001 $ \(py1 :: Ptr CFloat) ->
      allocaArray 20 $ \(px2 :: Ptr CDouble) ->
        allocaArray 20 $ \(py2 :: Ptr CDouble) -> do
          -- main loop
          whileM $ do
            glfwPollEvents
            -- Start the Dear ImGui frame
            imGui_ImplOpenGL3_NewFrame
            imGui_ImplGlfw_NewFrame
            newFrame

            showFramerate io
            demoLinePlots (px1, py1) (px2, py2)
            demoTables

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
