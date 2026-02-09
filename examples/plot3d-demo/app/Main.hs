{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Control.Monad.Extra (whenM, whileM)
import Data.Bits ((.|.))
import Data.Foldable (for_, traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
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
    imGuiIO_DeltaTime_get,
    imGuiIO_Framerate_get,
  )
import ImGui.ImVec2.Implementation (imVec2_x_get, imVec2_y_get)
import ImGui.ImVec4.Implementation (imVec4_w_get, imVec4_x_get, imVec4_y_get, imVec4_z_get)
import ImPlot qualified
import ImPlot3D qualified
import ImPlot3D.Enum
import ImPlot3D.TH qualified
import ImPlot3D.Template
import STD.Deletable (delete)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRIO)
import Text.Printf (printf)

instance IsString CString where
  fromString s = unsafePerformIO $ newCString s

-- this is a hack. but it's the best up to now.
ImPlot3D.TH.genPlotLine3DInstanceFor
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
  begin ("Framerate monitor" :: CString) nullPtr 0
  framerate :: Float <- realToFrac <$> imGuiIO_Framerate_get io
  withCString (printf "Application average %.3f ms/frame (%.1f FPS)" (1000.0 / framerate) framerate) $ \c_str ->
    textUnformatted c_str
  end

imPlot3DDemo :: (Ptr CFloat, Ptr CFloat, Ptr CFloat) -> IO ()
imPlot3DDemo (px1, py1, pz1) = do
  begin ("ImPlot3D Demo" :: CString) nullPtr 0
  whenM (toBool <$> beginMenuBar) $ do
    whenM (toBool <$> beginMenu ("Tools" :: CString) (fromBool True)) $ do
      menuItem_ ("Metrics" :: CString) ("" :: CString) (fromBool False) (fromBool True)
      endMenu
    endMenuBar
  textUnformatted ("ImPlot3D says olá!" :: CString)
  size <- newImVec2 (-1) 0
  whenM (toBool <$> ImPlot3D.beginPlot3D ("Line Plots" :: CString) size (fromIntegral (fromEnum ImPlot3DFlags_None))) $ do
    ImPlot3D.setupAxes3D ("x" :: CString) ("y" :: CString) ("z" :: CString)
    t <- getTime
    for_ [0 .. 1000] $ \i -> do
      let x = fromIntegral i * 0.001
      pokeElemOff px1 i x
      pokeElemOff py1 i (0.5 + 0.5 * cos (50.0 * (x + realToFrac t / 10.0)))
      pokeElemOff pz1 i (0.5 + 0.5 * sin (50.0 * (x + realToFrac t / 10.0)))
    ImPlot3D.plotLine3D "f(x)" px1 py1 pz1 1001

    ImPlot3D.endPlot3D
  delete size
  end

data Resource = Resource
  { res_array1 :: (Ptr CFloat, Ptr CFloat, Ptr CFloat),
    res_disp :: (Ptr CInt, Ptr CInt)
  }

withRes :: (Resource -> IO a) -> IO a
withRes f =
  allocaArray 1001 $ \(px1 :: Ptr CFloat) ->
    allocaArray 1001 $ \(py1 :: Ptr CFloat) ->
      allocaArray 1001 $ \(pz1 :: Ptr CFloat) ->
        alloca $ \(p_dispW :: Ptr CInt) ->
          alloca $ \(p_dispH :: Ptr CInt) ->
            let res = Resource (px1, py1, pz1) (p_dispW, p_dispH)
             in f res

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
      ("ImPlot3D Haskell Demo" :: CString)
      (cast_fptr_to_obj nullPtr :: GLFWmonitor)
      (cast_fptr_to_obj nullPtr :: GLFWwindow)
  glfwMakeContextCurrent window
  -- Enable vsync
  glfwSwapInterval 1
  ctxt <- createContext
  ImPlot.createImPlotContext
  ImPlot3D.createContext
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

  clear_color <- newImVec4 0.45 0.55 0.60 1.00

  withRes $ \res -> do
   -- main loop
   whileM $ do
     glfwPollEvents
     -- Start the Dear ImGui frame
     imGui_ImplOpenGL3_NewFrame
     imGui_ImplGlfw_NewFrame
     newFrame

     showFramerate io
     imPlot3DDemo (res_array1 res)
     render

     let (p_dispW, p_dispH) = res_disp res
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
