#!/bin/bash
# Build a canister from canister-build.toml via the shared Idris2IcWasm pipeline.
set -euo pipefail

usage() {
    cat <<'EOF'
Usage: build-canister-from-config.sh --config=PATH
       build-canister-from-config.sh --config PATH
EOF
}

CONFIG_PATH=""
while [ $# -gt 0 ]; do
    case "$1" in
        --config=*)
            CONFIG_PATH="${1#*=}"
            shift
            ;;
        --config)
            [ $# -ge 2 ] || { echo "Error: --config requires a path" >&2; exit 1; }
            CONFIG_PATH="$2"
            shift 2
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        *)
            echo "Unknown argument: $1" >&2
            usage >&2
            exit 1
            ;;
    esac
done

[ -n "$CONFIG_PATH" ] || { echo "Error: --config is required" >&2; usage >&2; exit 1; }

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
COMMON_SCRIPT="$SCRIPT_DIR/lib/build-canister-common.sh"

declare -a C_FILE_FIND_DIRS=()
declare -a GEN_ENTRY_ARGS=()
declare -a PREFLIGHT_COMMANDS=()
declare -a PRE_LINK_COMMANDS=()
declare -a FFI_HEADERS=()
declare -a EXTRA_C_FILES=()
declare -a EXTRA_FORCE_INCLUDES=()
declare -a EXTRA_INCLUDE_DIRS=()
declare -a EXTRA_EMCC_FLAGS=()

load_config() {
    eval "$(
        python3 - "$CONFIG_PATH" <<'PY'
import os
import shlex
import sys

try:
    import tomllib
except ModuleNotFoundError:
    import tomli as tomllib

config_path = os.path.abspath(sys.argv[1])
config_dir = os.path.dirname(config_path)

with open(config_path, "rb") as f:
    data = tomllib.load(f)

def q(value):
    return shlex.quote(str(value))

def emit_scalar(name, value):
    if value is None:
        return
    print(f"{name}={q(value)}")

def emit_bool(name, value):
    print(f"{name}={'1' if value else '0'}")

def emit_array(name, values):
    quoted = " ".join(q(v) for v in values)
    print(f"{name}=({quoted})")

def path(value):
    if value in (None, ""):
        return ""
    if os.path.isabs(value):
        return os.path.normpath(value)
    return os.path.normpath(os.path.join(config_dir, value))

def paths(values):
    return [path(v) for v in values]

paths_cfg = data.get("paths", {})
build_cfg = data.get("build", {})
gen_cfg = data.get("gen_entry", {})
preflight_cfg = data.get("preflight", {})
pre_link_cfg = data.get("pre_link", {})
ffi_cfg = data.get("ffi", {})
link_cfg = data.get("link", {})
wasi_cfg = data.get("wasi", {})

project_dir = path(paths_cfg.get("project_dir", "."))
workdir = path(build_cfg.get("workdir", project_dir))
build_dir = path(build_cfg.get("build_dir", os.path.join(project_dir, "build")))
ic0_support = path(paths_cfg.get("ic0_support", os.path.join(project_dir, "support", "ic0")))
icwasm_support = path(paths_cfg.get("icwasm_support", ic0_support))
local_idris2_icwasm = path(paths_cfg.get("local_idris2_icwasm", os.environ.get("IDRIS2_ICWASM_BINARY", "")))

emit_scalar("PROJECT_DIR", project_dir)
emit_scalar("WORKDIR", workdir)
emit_scalar("BUILD_DIR", build_dir)
emit_scalar("IC0_SUPPORT", ic0_support)
emit_scalar("ICWASM_SUPPORT", icwasm_support)
emit_scalar("LOCAL_IDRIS2_ICWASM", local_idris2_icwasm)
emit_scalar("BUILD_LABEL", build_cfg.get("label", os.path.basename(project_dir)))
emit_scalar("DEFAULT_OUTPUT_NAME", build_cfg.get("output_name", "canister"))
emit_scalar("BUILD_COMMAND", build_cfg.get("command", ""))
emit_scalar("C_FILE_EXPECTED", path(build_cfg.get("c_file_expected", "")))
emit_scalar("C_FILE_PATTERN", build_cfg.get("c_file_pattern", "*.c"))
emit_array("C_FILE_FIND_DIRS", paths(build_cfg.get("c_file_find_dirs", [build_dir])))

gen_enabled = gen_cfg.get("enabled", False)
emit_bool("GEN_ENTRY_ENABLED", gen_enabled)
if gen_enabled:
    gen_args = [
        f"--did={path(gen_cfg['did'])}",
        f"--prefix={gen_cfg['prefix']}",
    ]
    if "lib" in gen_cfg:
        gen_args.append(f"--lib={gen_cfg['lib']}")
    if "init" in gen_cfg and gen_cfg["init"]:
        gen_args.append(f"--init={gen_cfg['init']}")
    if "cmd_map" in gen_cfg and gen_cfg["cmd_map"]:
        gen_args.append(f"--cmd-map={path(gen_cfg['cmd_map'])}")
    if "timer_cmd" in gen_cfg and gen_cfg["timer_cmd"] is not None:
        gen_args.append(f"--timer-cmd={gen_cfg['timer_cmd']}")
    if "heartbeat_cmd" in gen_cfg and gen_cfg["heartbeat_cmd"] is not None:
        gen_args.append(f"--heartbeat-cmd={gen_cfg['heartbeat_cmd']}")
    if gen_cfg.get("sql_stable", False):
        gen_args.append("--sql-stable")
    out_path = path(gen_cfg.get("out", os.path.join(ic0_support, "canister_entry.c")))
    gen_args.append(f"--out={out_path}")
    emit_array("GEN_ENTRY_ARGS", gen_args)
    emit_scalar("DEFAULT_CANISTER_ENTRY", out_path)
else:
    emit_array("GEN_ENTRY_ARGS", [])
    emit_scalar("DEFAULT_CANISTER_ENTRY", path(gen_cfg.get("out", os.path.join(ic0_support, "canister_entry.c"))))

emit_array("PREFLIGHT_COMMANDS", preflight_cfg.get("commands", []))
emit_array("PRE_LINK_COMMANDS", pre_link_cfg.get("commands", []))
emit_array("FFI_HEADERS", ffi_cfg.get("headers", []))
emit_array("EXTRA_FORCE_INCLUDES", paths(ffi_cfg.get("force_includes", [])))
emit_array("EXTRA_C_FILES", paths(link_cfg.get("extra_c_files", [])))
emit_array("EXTRA_INCLUDE_DIRS", paths(link_cfg.get("extra_include_dirs", [])))
emit_array("EXTRA_EMCC_FLAGS", [str(v) for v in link_cfg.get("extra_emcc_flags", [])])
emit_bool("ENABLE_WASI_STUB", wasi_cfg.get("stub", False))
emit_scalar("STUB_WASI_SCRIPT", path(wasi_cfg.get("stub_script", "")))
PY
    )"
}

run_shell_command_in_workdir() {
    local cmd="$1"
    (
        cd "$WORKDIR"
        export PROJECT_DIR WORKDIR BUILD_DIR MINI_GMP IC0_SUPPORT ICWASM_SUPPORT LOCAL_IDRIS2_ICWASM DEFAULT_OUTPUT_NAME
        bash -lc "$cmd"
    )
}

run_preflight_commands() {
    local cmd
    [ "${PREFLIGHT_COMMANDS+set}" = "set" ] || return 0
    [ ${#PREFLIGHT_COMMANDS[@]} -gt 0 ] || return 0
    echo ">>> Step 0a: Running preflight commands"
    for cmd in "${PREFLIGHT_COMMANDS[@]}"; do
        run_shell_command_in_workdir "$cmd"
    done
}

run_pre_link_commands() {
    local cmd
    [ "${PRE_LINK_COMMANDS+set}" = "set" ] || return 0
    [ ${#PRE_LINK_COMMANDS[@]} -gt 0 ] || return 0
    echo ">>> Step 2.5: Running pre-link commands"
    for cmd in "${PRE_LINK_COMMANDS[@]}"; do
        run_shell_command_in_workdir "$cmd"
    done
}

run_config_build() {
    [ -n "${BUILD_COMMAND:-}" ] || { echo "Error: build.command is required" >&2; exit 1; }
    echo "Step 2: Generating C with RefC codegen..."
    run_shell_command_in_workdir "$BUILD_COMMAND"
}

build_ffi_declarations() {
    local header
    if [ "${FFI_HEADERS+set}" != "set" ] || [ ${#FFI_HEADERS[@]} -eq 0 ]; then
        FFI_DECLARATIONS=""
        return 0
    fi

    FFI_DECLARATIONS='/* FFI Forward Declarations (generated from canister-build.toml) */
#include <stdint.h>
'
    for header in "${FFI_HEADERS[@]}"; do
        FFI_DECLARATIONS+="#include \"$header\""$'\n'
    done
    FFI_DECLARATIONS+='/* End FFI Declarations */
'
}

load_config
build_ffi_declarations

VALIDATE_FN=run_preflight_commands
BUILD_FN=run_config_build
PRE_LINK_FN=run_pre_link_commands

source "$COMMON_SCRIPT"
