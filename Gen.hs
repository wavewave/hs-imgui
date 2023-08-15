{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Tree (drawForest)
import FFICXX.Generate.Builder (simpleBuilder)
import FFICXX.Generate.Code.Primitive
  ( bool,
    bool_,
    charpp,
    cppclass,
    cppclass_,
    cppclassref_,
    cstring,
    cstring_,
    double,
    double_,
    float,
    int,
    int_,
    star,
    uint,
    uint_,
    void_,
    voidp,
  )
import FFICXX.Generate.Config
  ( FFICXXConfig (..),
    SimpleBuilderConfig (..),
  )
import FFICXX.Generate.Dependency.Graph
  ( constructDepGraph,
    findDepCycles,
    gatherHsBootSubmodules,
    locateInDepCycles,
  )
import FFICXX.Generate.Type.Cabal (BuildType (..), Cabal (..), CabalName (..))
import FFICXX.Generate.Type.Class
  ( Arg (..),
    CTypes (..),
    Class (..),
    EnumType (..),
    Function (..),
    ProtectedMethod (..),
    TLOrdinary (..),
    TopLevel (..),
    Variable (..),
  )
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
    ModuleUnitMap (..),
    modImports,
  )
import FFICXX.Generate.Type.Module (TemplateClassImportHeader (..))
import FFICXX.Generate.Util.DepGraph (drawDepGraph)
import qualified Options.Applicative as OA
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (IOMode (..), hPutStrLn, stdout, withFile)

------------------------
-- import from stdcxx --
------------------------

stdcxx_cabal :: Cabal
stdcxx_cabal =
  Cabal
    { cabal_pkgname = CabalName "stdcxx",
      cabal_version = "0.8.0.0",
      cabal_cheaderprefix = "STD",
      cabal_moduleprefix = "STD",
      cabal_additional_c_incs = [],
      cabal_additional_c_srcs = [],
      cabal_additional_pkgdeps = [],
      cabal_license = Nothing,
      cabal_licensefile = Nothing,
      cabal_extraincludedirs = [],
      cabal_extralibdirs = [],
      cabal_extrafiles = [],
      cabal_pkg_config_depends = [],
      cabal_buildType = Simple
    }

deletable :: Class
deletable =
  AbstractClass
    { class_cabal = stdcxx_cabal,
      class_name = "Deletable",
      class_parents = [],
      class_protected = Protected [],
      class_alias = Nothing,
      class_funcs = [Destructor Nothing],
      class_vars = [],
      class_tmpl_funcs = []
    }

-----------------
-- start hgdal --
-----------------

cabal =
  Cabal
    { cabal_pkgname = CabalName "imgui",
      cabal_version = "1.0.0.0",
      cabal_cheaderprefix = "ImGui",
      cabal_moduleprefix = "ImGui",
      cabal_additional_c_incs = [],
      cabal_additional_c_srcs = [],
      cabal_additional_pkgdeps = [CabalName "stdcxx"],
      cabal_license = Just "BSD-3-Clause",
      cabal_licensefile = Just "LICENSE",
      cabal_extraincludedirs = [],
      cabal_extralibdirs = [],
      cabal_extrafiles = [],
      cabal_pkg_config_depends = ["libimgui", "glfw3"],
      cabal_buildType = Simple
    }

gLFWwindow :: Class
gLFWwindow =
  Class
    cabal
    "GLFWwindow"
    []
    mempty
    Nothing
    []
    []
    []
    False

imDrawData :: Class
imDrawData =
  Class
    cabal
    "ImDrawData"
    [deletable]
    mempty
    Nothing
    [ Constructor [] Nothing,
      NonVirtual void_ "Clear" [] Nothing
    ]
    []
    []
    False

imGuiContext :: Class
imGuiContext =
  Class
    cabal
    "ImGuiContext"
    []
    mempty
    Nothing
    []
    []
    []
    False

imGuiIO :: Class
imGuiIO =
  Class
    cabal
    "ImGuiIO"
    []
    mempty
    Nothing
    []
    [ Variable (int "ConfigFlags"),
      Variable (float "Framerate")
    ]
    []
    False

imGuiTextBuffer :: Class
imGuiTextBuffer =
  Class
    cabal
    "ImGuiTextBuffer"
    [deletable]
    mempty
    Nothing
    [ Constructor [] Nothing,
      NonVirtual void_ "clear" [] Nothing
    ]
    []
    []
    False

imGuiWindowFlags_ :: EnumType
imGuiWindowFlags_ =
  EnumType
    { enum_name = "ImGuiWindowFlags_",
      enum_cases =
        [ "ImGuiWindowFlags_None",
          "ImGuiWindowFlags_NoTitleBar",
          "ImGuiWindowFlags_NoResize",
          "ImGuiWindowFlags_NoMove",
          "ImGuiWindowFlags_NoScrollbar",
          "ImGuiWindowFlags_NoScrollWithMouse",
          "ImGuiWindowFlags_NoCollapse",
          "ImGuiWindowFlags_AlwaysAutoResize",
          "ImGuiWindowFlags_NoBackground",
          "ImGuiWindowFlags_NoSavedSettings",
          "ImGuiWindowFlags_NoMouseInputs",
          "ImGuiWindowFlags_MenuBar",
          "ImGuiWindowFlags_HorizontalScrollbar",
          "ImGuiWindowFlags_NoFocusOnAppearing",
          "ImGuiWindowFlags_NoBringToFrontOnFocus",
          "ImGuiWindowFlags_AlwaysVerticalScrollbar",
          "ImGuiWindowFlags_AlwaysHorizontalScrollbar",
          "ImGuiWindowFlags_AlwaysUseWindowPadding",
          "ImGuiWindowFlags_NoNavInputs",
          "ImGuiWindowFlags_NoNavFocus",
          "ImGuiWindowFlags_UnsavedDocument",
          "ImGuiWindowFlags_NoNav",
          "ImGuiWindowFlags_NoDecoration",
          "ImGuiWindowFlags_NoInputs",
          "ImGuiWindowFlags_NavFlattened",
          "ImGuiWindowFlags_ChildWindow",
          "ImGuiWindowFlags_Tooltip",
          "ImGuiWindowFlags_Popup",
          "ImGuiWindowFlags_Modal",
          "ImGuiWindowFlags_ChildMenu"
        ],
      enum_header = "imgui.h"
    }

imVec4 :: Class
imVec4 =
  Class
    cabal
    "ImVec4"
    [deletable]
    mempty
    Nothing
    [ Constructor [] (Just "newImVec4_"),
      Constructor [float "x", float "y", float "z", float "w"] Nothing
    ]
    [ Variable (float "x"),
      Variable (float "y"),
      Variable (float "z"),
      Variable (float "w")
    ]
    []
    False

classes =
  [ gLFWwindow,
    imDrawData,
    imGuiContext,
    imGuiIO,
    imGuiTextBuffer,
    imVec4
  ]

enums =
  [ imGuiWindowFlags_
  ]

toplevelfunctions :: [TopLevel]
toplevelfunctions =
  [ TLOrdinary (TopLevelFunction bool_ "Begin" [cstring "name", star CTBool "p_open"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "Button" [cstring "label"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "Checkbox" [cstring "label", star CTBool "v"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "ColorEdit3" [cstring "label", star CTFloat "col"] Nothing),
    TLOrdinary (TopLevelFunction (cppclass_ imGuiContext) "CreateContext" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "DestroyContext" [cppclass imGuiContext "ctx"] Nothing),
    TLOrdinary (TopLevelFunction void_ "End" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclass_ imDrawData) "GetDrawData" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclassref_ imGuiIO) "GetIO" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "NewFrame" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "Render" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "SameLine" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "ShowDemoWindow" [star CTBool "p_open"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "SliderFloat" [cstring "label", star CTFloat "v", float "v_min", float "v_max"] Nothing),
    TLOrdinary (TopLevelFunction void_ "StyleColorsDark" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "StyleColorsLight" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "TextUnformatted" [cstring "text"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "ImGui_ImplGlfw_InitForOpenGL" [cppclass gLFWwindow "window", bool "install_callbacks"] Nothing),
    TLOrdinary (TopLevelFunction void_ "ImGui_ImplGlfw_NewFrame" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "ImGui_ImplGlfw_Shutdown" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "ImGui_ImplOpenGL3_Init" [cstring "glsl_version"] Nothing),
    TLOrdinary (TopLevelFunction void_ "ImGui_ImplOpenGL3_NewFrame" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "ImGui_ImplOpenGL3_RenderDrawData" [cppclass imDrawData "draw_data"] Nothing),
    TLOrdinary (TopLevelFunction void_ "ImGui_ImplOpenGL3_Shutdown" [] Nothing),
    -- GLFW functions
    TLOrdinary (TopLevelFunction void_ "glfwDestroyWindow" [cppclass gLFWwindow "window"] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwGetFramebufferSize" [cppclass gLFWwindow "window", star CTInt "width", star CTInt "height" ] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwTerminate" [] Nothing),
    -- GL functions
    TLOrdinary (TopLevelFunction void_ "glClear" [int "mask"] Nothing),
    TLOrdinary (TopLevelFunction void_ "glClearColor" [float "red", float "green", float "blue", float "alpha"] Nothing),
    TLOrdinary (TopLevelFunction void_ "glViewport" [int "x", int "y", int "width", int "height"] Nothing)

  ]

templates = []

headers =
  [ ( MU_TopLevel,
      ModuleUnitImports
        { muimports_namespaces = ["ImGui"],
          muimports_headers =
            [ "GLFW/glfw3.h",
              "imgui.h",
              "backends/imgui_impl_glfw.h",
              "backends/imgui_impl_opengl3.h"
            ]
        }
    ),
    modImports "GLFWwindow" [] ["backends/imgui_impl_glfw.h"],
    modImports "ImDrawData" [] ["imgui.h"],
    modImports "ImGuiContext" [] ["imgui.h"],
    modImports "ImGuiIO" [] ["imgui.h"],
    modImports "ImGuiTextBuffer" [] ["imgui.h"],
    modImports "ImVec4" [] ["imgui.h"]
  ]

extraLib = []

extraDep = []

data CLIMode
  = Gen (Maybe FilePath)
  | DepGraph (Maybe FilePath)

genMode :: OA.Mod OA.CommandFields CLIMode
genMode =
  OA.command "gen" $
    OA.info
      ( Gen
          <$> OA.optional
            (OA.strOption (OA.long "template" <> OA.short 't' <> OA.help "template directory"))
      )
      (OA.progDesc "Generate source code")

depGraphMode :: OA.Mod OA.CommandFields CLIMode
depGraphMode =
  OA.command "depgraph" $
    OA.info
      ( DepGraph
          <$> OA.optional
            (OA.strOption (OA.long "dotfile" <> OA.short 'f' <> OA.help "output dot file"))
      )
      (OA.progDesc "Generate dependency graph")

optsParser :: OA.ParserInfo CLIMode
optsParser =
  OA.info
    (OA.subparser (genMode <> depGraphMode) OA.<**> OA.helper)
    OA.fullDesc

main :: IO ()
main = do
  mode <- OA.execParser optsParser
  case mode of
    Gen mtmplDir -> do
      let tmplDir = fromMaybe "../template" mtmplDir
      cwd <- getCurrentDirectory
      let fficfg =
            FFICXXConfig
              { fficxxconfig_workingDir = cwd </> "tmp" </> "working",
                fficxxconfig_installBaseDir = cwd </> "imgui",
                fficxxconfig_staticFileDir = tmplDir
              }
          sbcfg =
            SimpleBuilderConfig
              { sbcTopModule = "ImGui",
                sbcModUnitMap = ModuleUnitMap (HM.fromList headers),
                sbcCabal = cabal,
                sbcClasses = classes,
                sbcEnums = enums,
                sbcTopLevels = toplevelfunctions,
                sbcTemplates = templates,
                sbcExtraLibs = extraLib,
                sbcCxxOpts = ["-std=c++17"],
                sbcExtraDeps = extraDep,
                sbcStaticFiles = ["LICENSE"]
              }
      simpleBuilder fficfg sbcfg
    DepGraph mdotFile -> do
      let allclasses = fmap Right classes <> fmap (Left . tcihTClass) templates
          drawAction h = do
            hPutStrLn h $
              drawDepGraph allclasses toplevelfunctions
      case mdotFile of
        Nothing -> drawAction stdout
        Just dotFile -> withFile dotFile WriteMode drawAction
