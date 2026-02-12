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
    cppclasscopy_,
    cppclassref,
    cppclassref_,
    cstring,
    cstring_,
    double,
    double_,
    float,
    float_,
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
    TLTemplate (..),
    TopLevel (..),
    Types (..),
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
import FFICXX.Runtime.Types (FFISafety (..))
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

--
-- from imgui
--

imgui_cabal =
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

imVec2 :: Class
imVec2 =
  Class
    imgui_cabal
    "ImVec2"
    [deletable]
    mempty
    Nothing
    []
    []
    []
    False

--
-- from implot
--

implot_cabal =
  Cabal
    { cabal_pkgname = CabalName "implot",
      cabal_version = "1.0.0.0",
      cabal_cheaderprefix = "ImPlot",
      cabal_moduleprefix = "ImPlot",
      cabal_additional_c_incs = [],
      cabal_additional_c_srcs = [],
      cabal_additional_pkgdeps = [CabalName "stdcxx", CabalName "imgui"],
      cabal_license = Just "BSD-3-Clause",
      cabal_licensefile = Just "LICENSE",
      cabal_extraincludedirs = [],
      cabal_extralibdirs = [],
      cabal_extrafiles = [],
      cabal_pkg_config_depends = ["libimplot"],
      cabal_buildType = Simple
    }

------------------
-- start implot3d --
------------------

cabal =
  Cabal
    { cabal_pkgname = CabalName "implot3d",
      cabal_version = "1.0.0.0",
      cabal_cheaderprefix = "ImPlot3D",
      cabal_moduleprefix = "ImPlot3D",
      cabal_additional_c_incs = [],
      cabal_additional_c_srcs = [],
      cabal_additional_pkgdeps = [CabalName "stdcxx", CabalName "imgui", CabalName "implot"],
      cabal_license = Just "BSD-3-Clause",
      cabal_licensefile = Just "LICENSE",
      cabal_extraincludedirs = [],
      cabal_extralibdirs = [],
      cabal_extrafiles = [],
      cabal_pkg_config_depends = ["libimplot3d"],
      cabal_buildType = Simple
    }

imPlot3DFlags_ :: EnumType
imPlot3DFlags_ =
  EnumType
    { enum_name = "ImPlot3DFlags_",
      enum_cases =
        [ "ImPlot3DFlags_None",
          "ImPlot3DFlags_NoTitle",
          "ImPlot3DFlags_NoLegend",
          "ImPlot3DFlags_NoMouseText",
          "ImPlot3DFlags_NoClip",
          "ImPlot3DFlags_NoMenus",
          "ImPlot3DFlags_Equal",
          "ImPlot3DFlags_NoRotate",
          "ImPlot3DFlags_NoPan",
          "ImPlot3DFlags_NoZoom",
          "ImPlot3DFlags_NoInputs",
          "ImPlot3DFlags_CanvasOnly"
        ],
      enum_header = "implot3d.h"
    }

imPlot3DAxisFlags_ :: EnumType
imPlot3DAxisFlags_ =
  EnumType
    { enum_name = "ImPlot3DAxisFlags_",
      enum_cases =
        [ "ImPlot3DAxisFlags_None",
          "ImPlot3DAxisFlags_NoLabel",
          "ImPlot3DAxisFlags_NoGridLines",
          "ImPlot3DAxisFlags_NoTickMarks",
          "ImPlot3DAxisFlags_NoTickLabels",
          "ImPlot3DAxisFlags_LockMin",
          "ImPlot3DAxisFlags_LockMax",
          "ImPlot3DAxisFlags_AutoFit",
          "ImPlot3DAxisFlags_Invert",
          "ImPlot3DAxisFlags_PanStretch",
          "ImPlot3DAxisFlags_Lock",
          "ImPlot3DAxisFlags_NoDecorations"
        ],
      enum_header = "implot3d.h"
    }

imPlot3DColormap_ :: EnumType
imPlot3DColormap_ =
  EnumType
    { enum_name = "ImPlot3DColormap_",
      enum_cases =
        [ "ImPlot3DColormap_Deep",
          "ImPlot3DColormap_Dark",
          "ImPlot3DColormap_Pastel",
          "ImPlot3DColormap_Paired",
          "ImPlot3DColormap_Viridis",
          "ImPlot3DColormap_Plasma",
          "ImPlot3DColormap_Hot",
          "ImPlot3DColormap_Cool",
          "ImPlot3DColormap_Pink",
          "ImPlot3DColormap_Jet",
          "ImPlot3DColormap_Twilight",
          "ImPlot3DColormap_RdBu",
          "ImPlot3DColormap_BrBG",
          "ImPlot3DColormap_PiYG",
          "ImPlot3DColormap_Spectral",
          "ImPlot3DColormap_Greys"
        ],
      enum_header = "implot3d.h"
    }

imPlot3DLineFlags_ :: EnumType
imPlot3DLineFlags_ =
  EnumType
    { enum_name = "ImPlot3DLineFlags_",
      enum_cases =
        [ "ImPlot3DLineFlags_None",
          "ImPlot3DLineFlags_NoLegend",
          "ImPlot3DLineFlags_NoFit",
          "ImPlot3DLineFlags_Segments",
          "ImPlot3DLineFlags_Loop",
          "ImPlot3DLineFlags_SkipNaN"
        ],
      enum_header = "implot3d.h"
    }

imPlot3DSurfaceFlags_ :: EnumType
imPlot3DSurfaceFlags_ =
  EnumType
    { enum_name = "ImPlot3DSurfaceFlags_",
      enum_cases =
        [ "ImPlot3DSurfaceFlags_None",
          "ImPlot3DSurfaceFlags_NoLegend",
          "ImPlot3DSurfaceFlags_NoFit",
          "ImPlot3DSurfaceFlags_NoLines",
          "ImPlot3DSurfaceFlags_NoFill",
          "ImPlot3DSurfaceFlags_NoMarkers"
        ],
      enum_header = "implot3d.h"
    }

classes = []

enums =
  [ imPlot3DFlags_,
    imPlot3DAxisFlags_,
    imPlot3DColormap_,
    imPlot3DLineFlags_,
    imPlot3DSurfaceFlags_
  ]

toplevelfunctions :: [TopLevel]
toplevelfunctions =
  [ TLOrdinary (TopLevelFunction FFIUnsafe void_ "CreateContext" [] (Just "createContext")),
    TLOrdinary (TopLevelFunction FFIUnsafe void_ "ShowDemoWindow" [star CTBool "p_open"] (Just "showDemoWindow")),
    TLOrdinary (TopLevelFunction FFIUnsafe bool_ "BeginPlot" [cstring "title_id", cppclassref imVec2 "size", int "flags"] (Just "beginPlot3D")),
    TLOrdinary (TopLevelFunction FFIUnsafe void_ "EndPlot" [] (Just "EndPlot3D")),
    TLOrdinary (TopLevelFunction FFIUnsafe void_ "PushColormap" [int "cmap"] Nothing),
    TLOrdinary (TopLevelFunction FFIUnsafe void_ "SetupAxes" [cstring "x_label", cstring "y_label", cstring "z_label", int "x_flags", int "y_flags", int "z_flags"] (Just "setupAxes3D")),
    TLOrdinary (TopLevelFunction FFIUnsafe void_ "SetupAxesLimits" [double "x_min", double "y_min", double "z_min", double "x_max", double "y_max", double "z_max"] Nothing),
    TLTemplate
      ( TopLevelTemplateFunction
          { topleveltfunc_safety = FFIUnsafe,
            topleveltfunc_params = ["t1"],
            topleveltfunc_ret = void_,
            topleveltfunc_name = "plotLine3D",
            topleveltfunc_oname = "ImPlot3D::PlotLine",
            topleveltfunc_args =
              [ cstring "label_id",
                Arg (TemplateParamPointer "t1") "xs",
                Arg (TemplateParamPointer "t1") "ys",
                Arg (TemplateParamPointer "t1") "zs",
                int "count",
                int "flags",
                int "offset",
                int "stride"
              ]
          }
      ),
    TLTemplate
      ( TopLevelTemplateFunction
          { topleveltfunc_safety = FFIUnsafe,
            topleveltfunc_params = ["t1"],
            topleveltfunc_ret = void_,
            topleveltfunc_name = "plotSurface",
            topleveltfunc_oname = "ImPlot3D::PlotSurface",
            topleveltfunc_args =
              [ cstring "label_id",
                Arg (TemplateParamPointer "t1") "xs",
                Arg (TemplateParamPointer "t1") "ys",
                Arg (TemplateParamPointer "t1") "zs",
                int "x_count",
                int "y_count",
                double "scale_min",
                double "scale_max",
                int "flags",
                int "offset",
                int "stride"
              ]
          }
      )
  ]

templates = []

headers =
  [ ( MU_TopLevel,
      ModuleUnitImports
        { muimports_namespaces = ["ImPlot3D"],
          muimports_headers =
            [ "implot3d.h"
            ]
        }
    )
  ]

extraLib = []

extraDep = []

--
--
--

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
                fficxxconfig_installBaseDir = cwd </> "implot3d",
                fficxxconfig_staticFileDir = tmplDir
              }
          sbcfg =
            SimpleBuilderConfig
              { sbcTopModule = "ImPlot3D",
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
