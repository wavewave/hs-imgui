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
    CPPTypes (..),
    CTypes (..),
    Class (..),
    EnumType (..),
    Function (..),
    IsConst (..),
    ProtectedMethod (..),
    TLOrdinary (..),
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
-- start imgui --
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

gLFWmonitor :: Class
gLFWmonitor =
  Class
    cabal
    "GLFWmonitor"
    []
    mempty
    Nothing
    []
    []
    []
    False

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

imColor :: Class
imColor =
  Class
    cabal
    "ImColor"
    [deletable]
    mempty
    Nothing
    [ Constructor [] (Just "newImColor_"),
      Constructor [cppclassref imVec4 "col"] Nothing
    ]
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

imDrawList :: Class
imDrawList =
  Class
    cabal
    "ImDrawList"
    [deletable]
    mempty
    Nothing
    [ NonVirtual void_ "AddLine" [cppclassref imVec2 "p1", cppclassref imVec2 "p2", uint "col", float "thickness"] Nothing,
      NonVirtual void_ "AddRect" [cppclassref imVec2 "p_min", cppclassref imVec2 "p_max", uint "col", float "rounding", int "flags", float "thickness"] Nothing,
      NonVirtual void_ "AddRectFilled" [cppclassref imVec2 "p_min", cppclassref imVec2 "p_max", uint "col", float "rounding", int "flags"] Nothing,
      NonVirtual void_ "AddRectFilledMultiColor" [cppclassref imVec2 "p_min", cppclassref imVec2 "p_max", uint "col_upr_left", uint "col_upr_right", uint "col_bot_right", uint "col_bot_left"] Nothing,
      NonVirtual void_ "AddTriangle" [cppclassref imVec2 "p1", cppclassref imVec2 "p2", cppclassref imVec2 "p3", uint "col", float "thickness"] Nothing,
      NonVirtual void_ "AddTriangleFilled" [cppclassref imVec2 "p1", cppclassref imVec2 "p2", cppclassref imVec2 "p3", uint "col"] Nothing,
      NonVirtual void_ "AddCircle" [cppclassref imVec2 "center", float "radius", uint "col", int "num_segments", float "thickness"] Nothing,
      NonVirtual void_ "AddCircleFilled" [cppclassref imVec2 "center", float "radius", uint "col", int "num_segments"] Nothing,
      NonVirtual void_ "AddNgon" [cppclassref imVec2 "center", float "radius", uint "col", int "num_segments", float "thickness"] Nothing,
      NonVirtual void_ "AddNgonFilled" [cppclassref imVec2 "center", float "radius", uint "col", int "num_segments"] Nothing,
      NonVirtual void_ "AddText" [cppclassref imVec2 "pos", uint "col", cstring "text_begin"] Nothing,
      -- first arg is ImVec2* points
      NonVirtual void_ "AddPolyline" [cppclass imVec2 "points", int "num_points", uint "col", int "flags", float "thinkness"] Nothing,
      NonVirtual void_ "AddBezierCubic" [cppclassref imVec2 "p1", cppclassref imVec2 "p2", cppclassref imVec2 "p3", cppclassref imVec2 "p4", uint "col", float "thickness", int "num_segments"] Nothing,
      NonVirtual void_ "AddBezierQuadratic" [cppclassref imVec2 "p1", cppclassref imVec2 "p2", cppclassref imVec2 "p3", uint "col", float "thickness", int "num_segments"] Nothing
    ]
    []
    []
    False

imFont :: Class
imFont =
  Class
    cabal
    "ImFont"
    [deletable]
    mempty
    Nothing
    [ Constructor [] Nothing
    ]
    [ Variable (float "Scale")
    ]
    []
    False

imFontAtlas :: Class
imFontAtlas =
  Class
    cabal
    "ImFontAtlas"
    [deletable]
    mempty
    Nothing
    [ Constructor [] Nothing,
      NonVirtual (cppclass_ imFont) "AddFontDefault" [] Nothing,
      NonVirtual (cppclass_ imFont) "AddFontFromFileTTF" [cstring "filename", float "size_pixels"] Nothing
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
      Variable (float "DeltaTime"),
      Variable (float "Framerate"),
      Variable (cppclass imFontAtlas "Fonts")
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

imDrawFlags_ :: EnumType
imDrawFlags_ =
  EnumType
    { enum_name = "ImDrawFlags_",
      enum_cases =
        [ "ImDrawFlags_None",
          "ImDrawFlags_Closed",
          "ImDrawFlags_RoundCornersTopLeft",
          "ImDrawFlags_RoundCornersTopRight",
          "ImDrawFlags_RoundCornersBottomLeft",
          "ImDrawFlags_RoundCornersBottomRight",
          "ImDrawFlags_RoundCornersNone",
          "ImDrawFlags_RoundCornersTop",
          "ImDrawFlags_RoundCornersBottom",
          "ImDrawFlags_RoundCornersLeft",
          "ImDrawFlags_RoundCornersRight",
          "ImDrawFlags_RoundCornersAll",
          "ImDrawFlags_RoundCornersDefault_",
          "ImDrawFlags_RoundCornersMask_"
        ],
      enum_header = "imgui.h"
    }

imGuiConfigFlags_ :: EnumType
imGuiConfigFlags_ =
  EnumType
    { enum_name = "ImGuiConfigFlags_",
      enum_cases =
        [ "ImGuiConfigFlags_None",
          "ImGuiConfigFlags_NavEnableKeyboard",
          "ImGuiConfigFlags_NavEnableGamepad",
          "ImGuiConfigFlags_NavEnableSetMousePos",
          "ImGuiConfigFlags_NavNoCaptureKeyboard",
          "ImGuiConfigFlags_NoMouse",
          "ImGuiConfigFlags_NoMouseCursorChange",
          "ImGuiConfigFlags_IsSRGB",
          "ImGuiConfigFlags_IsTouchScreen"
        ],
      enum_header = "imgui.h"
    }

imGuiHoveredFlags_ :: EnumType
imGuiHoveredFlags_ =
  EnumType
    { enum_name = "ImGuiHoveredFlags_",
      enum_cases =
        [ "ImGuiHoveredFlags_None",
          "ImGuiHoveredFlags_ChildWindows",
          "ImGuiHoveredFlags_RootWindow",
          "ImGuiHoveredFlags_AnyWindow",
          "ImGuiHoveredFlags_NoPopupHierarchy",
          "ImGuiHoveredFlags_AllowWhenBlockedByPopup",
          "ImGuiHoveredFlags_AllowWhenBlockedByActiveItem",
          "ImGuiHoveredFlags_AllowWhenOverlapped",
          "ImGuiHoveredFlags_AllowWhenDisabled",
          "ImGuiHoveredFlags_NoNavOverride",
          "ImGuiHoveredFlags_RectOnly",
          "ImGuiHoveredFlags_RootAndChildWindows",
          "ImGuiHoveredFlags_DelayNormal",
          "ImGuiHoveredFlags_DelayShort",
          "ImGuiHoveredFlags_NoSharedDelay"
        ],
      enum_header = "imgui.h"
    }

imGuiInputTextFlags_ :: EnumType
imGuiInputTextFlags_ =
  EnumType
    { enum_name = "ImGuiInputTextFlags_",
      enum_cases =
        [ "ImGuiInputTextFlags_None",
          "ImGuiInputTextFlags_CharsDecimal",
          "ImGuiInputTextFlags_CharsHexadecimal",
          "ImGuiInputTextFlags_CharsUppercase",
          "ImGuiInputTextFlags_CharsNoBlank",
          "ImGuiInputTextFlags_AutoSelectAll",
          "ImGuiInputTextFlags_EnterReturnsTrue",
          "ImGuiInputTextFlags_CallbackCompletion",
          "ImGuiInputTextFlags_CallbackHistory",
          "ImGuiInputTextFlags_CallbackAlways",
          "ImGuiInputTextFlags_CallbackCharFilter",
          "ImGuiInputTextFlags_AllowTabInput",
          "ImGuiInputTextFlags_CtrlEnterForNewLine",
          "ImGuiInputTextFlags_NoHorizontalScroll",
          "ImGuiInputTextFlags_AlwaysOverwrite",
          "ImGuiInputTextFlags_ReadOnly",
          "ImGuiInputTextFlags_Password",
          "ImGuiInputTextFlags_NoUndoRedo",
          "ImGuiInputTextFlags_CharsScientific",
          "ImGuiInputTextFlags_CallbackResize",
          "ImGuiInputTextFlags_CallbackEdit",
          "ImGuiInputTextFlags_EscapeClearsAll"
        ],
      enum_header = "imgui.h"
    }

imGuiTableFlags_ :: EnumType
imGuiTableFlags_ =
  EnumType
    { enum_name = "ImGuiTableFlags_",
      enum_cases =
        [ "ImGuiTableFlags_None",
          "ImGuiTableFlags_Resizable",
          "ImGuiTableFlags_Reorderable",
          "ImGuiTableFlags_Hideable",
          "ImGuiTableFlags_Sortable",
          "ImGuiTableFlags_NoSavedSettings",
          "ImGuiTableFlags_ContextMenuInBody",
          "ImGuiTableFlags_RowBg",
          "ImGuiTableFlags_BordersInnerH",
          "ImGuiTableFlags_BordersOuterH",
          "ImGuiTableFlags_BordersInnerV",
          "ImGuiTableFlags_BordersOuterV",
          "ImGuiTableFlags_BordersH",
          "ImGuiTableFlags_BordersV",
          "ImGuiTableFlags_BordersInner",
          "ImGuiTableFlags_BordersOuter",
          "ImGuiTableFlags_Borders",
          "ImGuiTableFlags_NoBordersInBody",
          "ImGuiTableFlags_NoBordersInBodyUntilResize",
          "ImGuiTableFlags_SizingFixedFit",
          "ImGuiTableFlags_SizingFixedSame",
          "ImGuiTableFlags_SizingStretchProp",
          "ImGuiTableFlags_SizingStretchSame",
          "ImGuiTableFlags_NoHostExtendX",
          "ImGuiTableFlags_NoHostExtendY",
          "ImGuiTableFlags_NoKeepColumnsVisible",
          "ImGuiTableFlags_PreciseWidths",
          "ImGuiTableFlags_NoClip",
          "ImGuiTableFlags_PadOuterX",
          "ImGuiTableFlags_NoPadOuterX",
          "ImGuiTableFlags_NoPadInnerX",
          "ImGuiTableFlags_ScrollX",
          "ImGuiTableFlags_ScrollY",
          "ImGuiTableFlags_SortMulti",
          "ImGuiTableFlags_SortTristate",
          "ImGuiTableFlags_SizingMask_"
        ],
      enum_header = "imgui.h"
    }

imGuiTableColumnFlags_ :: EnumType
imGuiTableColumnFlags_ =
  EnumType
    { enum_name = "ImGuiTableColumnFlags_",
      enum_cases =
        [ "ImGuiTableColumnFlags_None",
          "ImGuiTableColumnFlags_Disabled",
          "ImGuiTableColumnFlags_DefaultHide",
          "ImGuiTableColumnFlags_DefaultSort",
          "ImGuiTableColumnFlags_WidthStretch",
          "ImGuiTableColumnFlags_WidthFixed",
          "ImGuiTableColumnFlags_NoResize",
          "ImGuiTableColumnFlags_NoReorder",
          "ImGuiTableColumnFlags_NoHide",
          "ImGuiTableColumnFlags_NoClip",
          "ImGuiTableColumnFlags_NoSort",
          "ImGuiTableColumnFlags_NoSortAscending",
          "ImGuiTableColumnFlags_NoSortDescending",
          "ImGuiTableColumnFlags_NoHeaderLabel",
          "ImGuiTableColumnFlags_NoHeaderWidth",
          "ImGuiTableColumnFlags_PreferSortAscending",
          "ImGuiTableColumnFlags_PreferSortDescending",
          "ImGuiTableColumnFlags_IndentEnable",
          "ImGuiTableColumnFlags_IndentDisable",
          "ImGuiTableColumnFlags_IsEnabled",
          "ImGuiTableColumnFlags_IsVisible",
          "ImGuiTableColumnFlags_IsSorted",
          "ImGuiTableColumnFlags_IsHovered",
          "ImGuiTableColumnFlags_WidthMask_",
          "ImGuiTableColumnFlags_IndentMask_",
          "ImGuiTableColumnFlags_StatusMask_",
          "ImGuiTableColumnFlags_NoDirectResize_"
        ],
      enum_header = "imgui.h"
    }

imGuiTableRowFlags_ :: EnumType
imGuiTableRowFlags_ =
  EnumType
    { enum_name = "ImGuiTableRowFlags_",
      enum_cases =
        [ "ImGuiTableRowFlags_None",
          "ImGuiTableRowFlags_Headers"
        ],
      enum_header = "imgui.h"
    }

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

imGuiCond_ :: EnumType
imGuiCond_ =
  EnumType
    { enum_name = "ImGuiCond_",
      enum_cases =
        [ "ImGuiCond_None",
          "ImGuiCond_Always",
          "ImGuiCond_Once",
          "ImGuiCond_FirstUseEver",
          "ImGuiCond_Appearing"
        ],
      enum_header = "imgui.h"
    }

imGuiKey :: EnumType
imGuiKey =
  EnumType
    { enum_name = "ImGuiKey",
      enum_cases =
        [ "ImGuiKey_None",
          "ImGuiKey_Tab",
          "ImGuiKey_LeftArrow",
          "ImGuiKey_RightArrow",
          "ImGuiKey_UpArrow",
          "ImGuiKey_DownArrow",
          "ImGuiKey_PageUp",
          "ImGuiKey_PageDown",
          "ImGuiKey_Home",
          "ImGuiKey_End",
          "ImGuiKey_Insert",
          "ImGuiKey_Delete",
          "ImGuiKey_Backspace",
          "ImGuiKey_Space",
          "ImGuiKey_Enter",
          "ImGuiKey_Escape",
          "ImGuiKey_LeftCtrl",
          "ImGuiKey_LeftShift",
          "ImGuiKey_LeftAlt",
          "ImGuiKey_LeftSuper",
          "ImGuiKey_RightCtrl",
          "ImGuiKey_RightShift",
          "ImGuiKey_RightAlt",
          "ImGuiKey_RightSuper",
          "ImGuiKey_Menu",
          "ImGuiKey_0",
          "ImGuiKey_1",
          "ImGuiKey_2",
          "ImGuiKey_3",
          "ImGuiKey_4",
          "ImGuiKey_5",
          "ImGuiKey_6",
          "ImGuiKey_7",
          "ImGuiKey_8",
          "ImGuiKey_9",
          "ImGuiKey_A",
          "ImGuiKey_B",
          "ImGuiKey_C",
          "ImGuiKey_D",
          "ImGuiKey_E",
          "ImGuiKey_F",
          "ImGuiKey_G",
          "ImGuiKey_H",
          "ImGuiKey_I",
          "ImGuiKey_J",
          "ImGuiKey_K",
          "ImGuiKey_L",
          "ImGuiKey_M",
          "ImGuiKey_N",
          "ImGuiKey_O",
          "ImGuiKey_P",
          "ImGuiKey_Q",
          "ImGuiKey_R",
          "ImGuiKey_S",
          "ImGuiKey_T",
          "ImGuiKey_U",
          "ImGuiKey_V",
          "ImGuiKey_W",
          "ImGuiKey_X",
          "ImGuiKey_Y",
          "ImGuiKey_Z",
          "ImGuiKey_F1",
          "ImGuiKey_F2",
          "ImGuiKey_F3",
          "ImGuiKey_F4",
          "ImGuiKey_F5",
          "ImGuiKey_F6",
          "ImGuiKey_F7",
          "ImGuiKey_F8",
          "ImGuiKey_F9",
          "ImGuiKey_F10",
          "ImGuiKey_F11",
          "ImGuiKey_F12",
          "ImGuiKey_Apostrophe",
          "ImGuiKey_Comma",
          "ImGuiKey_Minus",
          "ImGuiKey_Period",
          "ImGuiKey_Slash",
          "ImGuiKey_Semicolon",
          "ImGuiKey_Equal",
          "ImGuiKey_LeftBracket",
          "ImGuiKey_Backslash",
          "ImGuiKey_RightBracket",
          "ImGuiKey_GraveAccent",
          "ImGuiKey_CapsLock",
          "ImGuiKey_ScrollLock",
          "ImGuiKey_NumLock",
          "ImGuiKey_PrintScreen",
          "ImGuiKey_Pause",
          "ImGuiKey_Keypad0",
          "ImGuiKey_Keypad1",
          "ImGuiKey_Keypad2",
          "ImGuiKey_Keypad3",
          "ImGuiKey_Keypad4",
          "ImGuiKey_Keypad5",
          "ImGuiKey_Keypad6",
          "ImGuiKey_Keypad7",
          "ImGuiKey_Keypad8",
          "ImGuiKey_Keypad9",
          "ImGuiKey_KeypadDecimal",
          "ImGuiKey_KeypadDivide",
          "ImGuiKey_KeypadMultiply",
          "ImGuiKey_KeypadSubtract",
          "ImGuiKey_KeypadAdd",
          "ImGuiKey_KeypadEnter",
          "ImGuiKey_KeypadEqual",
          --
          "ImGuiKey_COUNT",
          --
          "ImGuiMod_None",
          "ImGuiMod_Ctrl",
          "ImGuiMod_Shift",
          "ImGuiMod_Alt",
          "ImGuiMod_Super",
          "ImGuiMod_Shortcut",
          "ImGuiMod_Mask_"
        ],
      enum_header = "imgui.h"
    }

imGuiMouseButton_ :: EnumType
imGuiMouseButton_ =
  EnumType
    { enum_name = "ImGuiMouseButton_",
      enum_cases =
        [ "ImGuiMouseButton_Left",
          "ImGuiMouseButton_Right",
          "ImGuiMouseButton_Middle",
          "ImGuiMouseButton_COUNT"
        ],
      enum_header = "imgui.h"
    }

imGuiStyleVar_ :: EnumType
imGuiStyleVar_ =
  EnumType
    { enum_name = "ImGuiStyleVar_",
      enum_cases =
        [ "ImGuiStyleVar_Alpha",
          "ImGuiStyleVar_DisabledAlpha",
          "ImGuiStyleVar_WindowPadding",
          "ImGuiStyleVar_WindowRounding",
          "ImGuiStyleVar_WindowBorderSize",
          "ImGuiStyleVar_WindowMinSize",
          "ImGuiStyleVar_WindowTitleAlign",
          "ImGuiStyleVar_ChildRounding",
          "ImGuiStyleVar_ChildBorderSize",
          "ImGuiStyleVar_PopupRounding",
          "ImGuiStyleVar_PopupBorderSize",
          "ImGuiStyleVar_FramePadding",
          "ImGuiStyleVar_FrameRounding",
          "ImGuiStyleVar_FrameBorderSize",
          "ImGuiStyleVar_ItemSpacing",
          "ImGuiStyleVar_ItemInnerSpacing",
          "ImGuiStyleVar_IndentSpacing",
          "ImGuiStyleVar_CellPadding",
          "ImGuiStyleVar_ScrollbarSize",
          "ImGuiStyleVar_ScrollbarRounding",
          "ImGuiStyleVar_GrabMinSize",
          "ImGuiStyleVar_GrabRounding",
          "ImGuiStyleVar_TabRounding",
          "ImGuiStyleVar_ButtonTextAlign",
          "ImGuiStyleVar_SelectableTextAlign",
          "ImGuiStyleVar_SeparatorTextBorderSize",
          "ImGuiStyleVar_SeparatorTextAlign",
          "ImGuiStyleVar_SeparatorTextPadding",
          "ImGuiStyleVar_COUNT"
        ],
      enum_header = "imgui.h"
    }

imVec2 :: Class
imVec2 =
  Class
    cabal
    "ImVec2"
    [deletable]
    mempty
    Nothing
    [ Constructor [] (Just "newImVec2_"),
      Constructor [float "x", float "y"] Nothing
    ]
    [ Variable (float "x"),
      Variable (float "y")
    ]
    []
    False

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
  [ gLFWmonitor,
    gLFWwindow,
    imColor,
    imDrawData,
    imDrawList,
    imFont,
    imFontAtlas,
    imGuiContext,
    imGuiIO,
    imGuiTextBuffer,
    imVec2,
    imVec4
  ]

enums =
  [ imDrawFlags_,
    imGuiConfigFlags_,
    imGuiHoveredFlags_,
    imGuiInputTextFlags_,
    imGuiTableFlags_,
    imGuiTableColumnFlags_,
    imGuiTableRowFlags_,
    imGuiWindowFlags_,
    imGuiCond_,
    imGuiKey,
    imGuiMouseButton_,
    imGuiStyleVar_
  ]

toplevelfunctions :: [TopLevel]
toplevelfunctions =
  [ -- Context
    TLOrdinary (TopLevelFunction (cppclass_ imGuiContext) "CreateContext" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "DestroyContext" [cppclass imGuiContext "ctx"] Nothing),
    -- top-level window
    TLOrdinary (TopLevelFunction bool_ "Begin" [cstring "name", star CTBool "p_open", int "flags"] Nothing),
    TLOrdinary (TopLevelFunction void_ "End" [] Nothing),
    -- child window
    TLOrdinary (TopLevelFunction bool_ "BeginChild" [cstring "str_id", cppclassref imVec2 "size", bool "border", int "flags"] Nothing),
    TLOrdinary (TopLevelFunction void_ "EndChild" [] Nothing),
    -- tab bar
    TLOrdinary (TopLevelFunction bool_ "BeginTabBar" [cstring "str_id"] Nothing),
    TLOrdinary (TopLevelFunction void_ "EndTabBar" [] Nothing),
    -- tab item
    TLOrdinary (TopLevelFunction bool_ "BeginTabItem" [cstring "label"] Nothing),
    TLOrdinary (TopLevelFunction void_ "EndTabItem" [] Nothing),
    -- Widgets: Button, Checkbox
    TLOrdinary (TopLevelFunction bool_ "Button" [cstring "label"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "Checkbox" [cstring "label", star CTBool "v"] Nothing),
    -- Widgets: ColorEdit
    TLOrdinary (TopLevelFunction bool_ "ColorEdit3" [cstring "label", star CTFloat "col"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "ColorEdit4" [cstring "label", star CTFloat "col"] Nothing),
    -- Widgets: Input with Keyboard
    TLOrdinary (TopLevelFunction bool_ "InputText" [cstring "label", star CTChar "buf", uint "buf_size", int "flags"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "InputTextMultiline" [cstring "label", star CTChar "buf", uint "buf_size", cppclassref imVec2 "size", int "flags"] Nothing),
    --
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetCursorScreenPos" [] Nothing),
    -- draw data/list
    TLOrdinary (TopLevelFunction (cppclass_ imDrawData) "GetDrawData" [] Nothing),
    -- general io
    TLOrdinary (TopLevelFunction (cppclassref_ imGuiIO) "GetIO" [] Nothing),
    TLOrdinary (TopLevelFunction double_ "GetTime" [] Nothing),
    -- query on item
    TLOrdinary (TopLevelFunction bool_ "IsItemHovered" [int "flags"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsItemActive" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsItemFocused" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsItemClicked" [int "mouse_button"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsItemVisible" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsItemEdited" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsItemActivated" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsItemDeactivated" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsItemDeactivatedAfterEdit" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsItemToggledOpen" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsAnyItemHovered" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsAnyItemActive" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsAnyItemFocused" [] Nothing),
    TLOrdinary (TopLevelFunction uint_ "GetItemID" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetItemRectMin" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetItemRectMax" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetItemRectSize" [] Nothing),
    -- Inputs Utilities: Keyboard/Mouse/Gamepad
    -- keyboard
    TLOrdinary (TopLevelFunction bool_ "IsKeyDown" [Arg (CPT (CPTEnum imGuiKey) NoConst) "key"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsKeyPressed" [Arg (CPT (CPTEnum imGuiKey) NoConst) "key", bool "repeat"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsKeyReleased" [Arg (CPT (CPTEnum imGuiKey) NoConst) "key"] Nothing),
    TLOrdinary (TopLevelFunction int_ "GetKeyPressedAmount" [Arg (CPT (CPTEnum imGuiKey) NoConst) "key", float "repeat_delay", float "rate"] Nothing),
    TLOrdinary (TopLevelFunction cstring_ "GetKeyName" [Arg (CPT (CPTEnum imGuiKey) NoConst) "key"] Nothing),
    TLOrdinary (TopLevelFunction void_ "SetNextFrameWantCaptureKeyboard" [bool "want_capture_keyboard"] Nothing),
    -- mouse
    TLOrdinary (TopLevelFunction bool_ "IsMouseDown" [int "button"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsMouseClicked" [int "button", bool "repeat"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsMouseClicked" [int "button"] (Just "isMouseClicked_")),
    TLOrdinary (TopLevelFunction bool_ "IsMouseReleased" [int "button"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsMouseDoubleClicked" [int "button"] Nothing),
    TLOrdinary (TopLevelFunction int_ "GetMouseClickedCount" [int "button"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsMouseHoveringRect" [cppclassref imVec2 "r_min", cppclassref imVec2 "r_max", bool "clip"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsMousePosValid" [cppclass imVec2 "mouse_pos"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsAnyMouseDown" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetMousePos" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetMousePosOnOpeningCurrentPopup" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsMouseDragging" [int "button", float "lock_threshold"] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetMouseDragDelta" [int "button", float "lock_threshold"] Nothing),
    TLOrdinary (TopLevelFunction void_ "ResetMouseDragDelta" [int "button"] Nothing),
    -- windows utilities
    TLOrdinary (TopLevelFunction bool_ "IsWindowAppearing" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsWindowCollapsed" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsWindowFocused" [int "flags"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "IsWindowHovered" [int "flags"] Nothing),
    TLOrdinary (TopLevelFunction (cppclass_ imDrawList) "GetWindowDrawList" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetWindowPos" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetWindowSize" [] Nothing),
    TLOrdinary (TopLevelFunction float_ "GetWindowWidth" [] Nothing),
    TLOrdinary (TopLevelFunction float_ "GetWindowHeight" [] Nothing),
    -- window manipulation
    TLOrdinary (TopLevelFunction void_ "SetNextWindowPos" [cppclassref imVec2 "pos", int "cond", cppclassref imVec2 "pivot"] Nothing),
    TLOrdinary (TopLevelFunction void_ "SetNextWindowSize" [cppclassref imVec2 "size", int "cond"] Nothing),
    -- content region
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetContentRegionAvail" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetContentRegionMax" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetWindowContentRegionMin" [] Nothing),
    TLOrdinary (TopLevelFunction (cppclasscopy_ imVec2) "GetWindowContentRegionMax" [] Nothing),
    -- frame
    TLOrdinary (TopLevelFunction void_ "NewFrame" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "PopItemWidth" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "PushItemWidth" [float "item_width"] Nothing),
    TLOrdinary (TopLevelFunction float_ "CalcItemWidth" [] Nothing),
    TLOrdinary (TopLevelFunction float_ "GetFrameHeight" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "Render" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "SameLine" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "Dummy" [cppclassref imVec2 "size"] Nothing),
    TLOrdinary (TopLevelFunction void_ "ShowDemoWindow" [star CTBool "p_open"] Nothing),
    TLOrdinary (TopLevelFunction bool_ "SliderFloat" [cstring "label", star CTFloat "v", float "v_min", float "v_max"] Nothing),
    TLOrdinary (TopLevelFunction void_ "TextUnformatted" [cstring "text"] Nothing),
    -- Color
    TLOrdinary (TopLevelFunction void_ "StyleColorsDark" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "StyleColorsLight" [] Nothing),
    TLOrdinary (TopLevelFunction uint_ "ColorConvertFloat4ToU32" [cppclassref imVec4 "in"] Nothing),
    -- Font
    TLOrdinary (TopLevelFunction (cppclass_ imFont) "GetFont" [] Nothing),
    TLOrdinary (TopLevelFunction float_ "GetFontSize" [] Nothing),
    -- Parameters stacks (shared)
    TLOrdinary (TopLevelFunction void_ "PushFont" [cppclass imFont "font"] Nothing),
    TLOrdinary (TopLevelFunction void_ "PopFont" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "PushStyleVar" [int "idx", float "val"] Nothing),
    TLOrdinary (TopLevelFunction void_ "PopStyleVar" [int "count"] Nothing),
    TLOrdinary (TopLevelFunction void_ "PopStyleVar" [] (Just "PopStyleVar_")),
    -- ID
    TLOrdinary (TopLevelFunction void_ "PushID" [int "int_id"] Nothing),
    TLOrdinary (TopLevelFunction void_ "PopID" [] Nothing),
    -- table
    TLOrdinary (TopLevelFunction bool_ "BeginTable" [cstring "str_id", int "column", int "flags"] Nothing),
    TLOrdinary (TopLevelFunction void_ "EndTable" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "TableHeadersRow" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "TableNextRow" [int "row_flags"] Nothing),
    TLOrdinary (TopLevelFunction void_ "TableNextColumn" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "TableSetupColumn" [cstring "label", int "flags", float "init_width_or_weight"] Nothing),
    TLOrdinary (TopLevelFunction void_ "TableSetupColumn" [cstring "label"] (Just "tableSetupColumn_")),
    TLOrdinary (TopLevelFunction void_ "TableSetColumnIndex" [int "column_n"] Nothing),
    -- backend
    TLOrdinary (TopLevelFunction bool_ "ImGui_ImplGlfw_InitForOpenGL" [cppclass gLFWwindow "window", bool "install_callbacks"] Nothing),
    TLOrdinary (TopLevelFunction void_ "ImGui_ImplGlfw_NewFrame" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "ImGui_ImplGlfw_Shutdown" [] Nothing),
    TLOrdinary (TopLevelFunction bool_ "ImGui_ImplOpenGL3_Init" [cstring "glsl_version"] Nothing),
    TLOrdinary (TopLevelFunction void_ "ImGui_ImplOpenGL3_NewFrame" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "ImGui_ImplOpenGL3_RenderDrawData" [cppclass imDrawData "draw_data"] Nothing),
    TLOrdinary (TopLevelFunction void_ "ImGui_ImplOpenGL3_Shutdown" [] Nothing),
    -- GLFW functions
    TLOrdinary (TopLevelFunction (cppclass_ gLFWwindow) "glfwCreateWindow" [int "width", int "height", cstring "title", cppclass gLFWmonitor "monitor", cppclass gLFWwindow "share"] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwDestroyWindow" [cppclass gLFWwindow "window"] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwGetFramebufferSize" [cppclass gLFWwindow "window", star CTInt "width", star CTInt "height"] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwInit" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwMakeContextCurrent" [cppclass gLFWwindow "window"] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwPollEvents" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwSwapBuffers" [cppclass gLFWwindow "window"] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwSwapInterval" [int "interval"] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwTerminate" [] Nothing),
    TLOrdinary (TopLevelFunction void_ "glfwWindowHint" [int "hint", int "value"] Nothing),
    TLOrdinary (TopLevelFunction int_ "glfwWindowShouldClose" [cppclass gLFWwindow "window"] Nothing),
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
              "imgui_impl_glfw.h",
              "imgui_impl_opengl3.h"
            ]
        }
    ),
    modImports "GLFWwindow" [] ["imgui_impl_glfw.h"],
    modImports "ImColor" [] ["imgui.h"],
    modImports "ImDrawData" [] ["imgui.h"],
    modImports "ImDrawList" [] ["imgui.h"],
    modImports "ImFont" [] ["imgui.h"],
    modImports "ImFontAtlas" [] ["imgui.h"],
    modImports "ImGuiContext" [] ["imgui.h"],
    modImports "ImGuiIO" [] ["imgui.h"],
    modImports "ImGuiTextBuffer" [] ["imgui.h"],
    modImports "ImVec2" [] ["imgui.h"],
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
