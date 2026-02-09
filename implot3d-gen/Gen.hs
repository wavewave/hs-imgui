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

classes = []

enums =
  [ imPlot3DFlags_
  ]

toplevelfunctions :: [TopLevel]
toplevelfunctions =
  [ TLOrdinary (TopLevelFunction FFIUnsafe void_ "CreateContext" [] (Just "createContext")),
    TLOrdinary (TopLevelFunction FFIUnsafe void_ "ShowDemoWindow" [star CTBool "p_open"] (Just "showDemoWindow")),
    TLOrdinary (TopLevelFunction FFIUnsafe bool_ "BeginPlot" [cstring "title_id", cppclassref imVec2 "size", int "flags"] (Just "beginPlot3D")),
    TLOrdinary (TopLevelFunction FFIUnsafe void_ "EndPlot" [] (Just "EndPlot3D")),
    TLOrdinary (TopLevelFunction FFIUnsafe void_ "SetupAxes" [cstring "x_label", cstring "y_label", cstring "z_label"] (Just "setupAxes3D")),
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
                int "count"
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
