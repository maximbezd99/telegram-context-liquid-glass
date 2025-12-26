load("@build_bazel_rules_swift//swift:swift.bzl", "swift_library")
load(
    "@build_bazel_rules_apple//apple:resources.bzl",
    "apple_resource_bundle",
    "apple_resource_group",
)
load("//build-system/bazel-utils:plist_fragment.bzl",
    "plist_fragment",
)

filegroup(
    name = "LiquidGlassComponentsMetalResources",
    srcs = glob([
        "MetalResources/**/*.*",
    ]),
    visibility = ["//visibility:public"],
)

plist_fragment(
    name = "LiquidGlassComponentsBundleInfoPlist",
    extension = "plist",
    template =
    """
    <key>CFBundleIdentifier</key>
    <string>org.telegram.LiquidGlassComponents</string>
    <key>CFBundleDevelopmentRegion</key>
    <string>en</string>
    <key>CFBundleName</key>
    <string>LiquidGlassComponents</string>
    """
)

apple_resource_bundle(
    name = "LiquidGlassComponentsBundle",
    infoplists = [
        ":LiquidGlassComponentsBundleInfoPlist",
    ],
    resources = [
        ":LiquidGlassComponentsMetalResources",
    ],
)

swift_library(
    name = "LiquidGlassComponents",
    module_name = "LiquidGlassComponents",
    srcs = glob([
        "Sources/**/*.swift",
    ]),
    copts = [
        "-warnings-as-errors",
    ],
    data = [
        ":LiquidGlassComponentsBundle",
    ],
    deps = [
        "//submodules/Display",
        "//submodules/ComponentFlow",
        "//submodules/Components/ComponentDisplayAdapters",
        "//submodules/UIKitRuntimeUtils",
        "//submodules/AppBundle",
    ],
    visibility = [
        "//visibility:public",
    ],
)
