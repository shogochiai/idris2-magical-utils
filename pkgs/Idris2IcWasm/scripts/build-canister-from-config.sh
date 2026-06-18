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
    trim() {
        local s="$1"
        s="${s#"${s%%[![:space:]]*}"}"
        s="${s%"${s##*[![:space:]]}"}"
        printf '%s' "$s"
    }

    strip_quotes() {
        local s
        s="$(trim "$1")"
        if [[ "$s" == \"*\" && "$s" == *\" ]]; then
            s="${s#\"}"
            s="${s%\"}"
        elif [[ "$s" == \'*\' && "$s" == *\' ]]; then
            s="${s#\'}"
            s="${s%\'}"
        fi
        printf '%s' "$s"
    }

    abs_dirname() {
        local path="$1"
        (cd "$(dirname "$path")" && pwd)
    }

    normalize_path() {
        local input="$1"
        local absolute=0
        local -a parts
        local -a out
        local part

        [[ "$input" == /* ]] && absolute=1
        IFS='/' read -r -a parts <<< "$input"
        out=()
        for part in "${parts[@]}"; do
            case "$part" in
                ""|".")
                    continue
                    ;;
                "..")
                    if [[ ${#out[@]} -gt 0 ]]; then
                        unset 'out[${#out[@]}-1]'
                    fi
                    ;;
                *)
                    out+=("$part")
                    ;;
            esac
        done

        local joined=""
        if [[ ${#out[@]} -gt 0 ]]; then
            local IFS='/'
            joined="${out[*]}"
        fi

        if [[ $absolute -eq 1 ]]; then
            if [[ -n "$joined" ]]; then
                printf '/%s' "$joined"
            else
                printf '/'
            fi
        else
            printf '%s' "$joined"
        fi
    }

    resolve_path() {
        local value="$1"
        if [[ -z "$value" ]]; then
            printf '%s' ""
        elif [[ "$value" = /* ]]; then
            normalize_path "$value"
        else
            normalize_path "$CONFIG_DIR/$value"
        fi
    }

    toml_get_raw() {
        local section="$1"
        local key="$2"
        awk -v section="$section" -v key="$key" '
            function trim(s) { sub(/^[ \t\r]+/, "", s); sub(/[ \t\r]+$/, "", s); return s }
            /^[ \t]*#/ { next }
            /^[ \t]*$/ { next }
            match($0, /^[ \t]*\[[^]]+\][ \t]*$/) {
                current = substr(trim($0), 2, length(trim($0)) - 2)
                next
            }
            current == section {
                if (!capturing && $0 ~ ("^[ \t]*" key "[ \t]*=")) {
                    line = $0
                    sub("^[ \t]*" key "[ \t]*=[ \t]*", "", line)
                    print line
                    if (line ~ /^\[/ && line !~ /\]/) capturing = 1
                    else exit
                    next
                }
                if (capturing) {
                    print $0
                    if ($0 ~ /\]/) exit
                }
            }
        ' "$CONFIG_PATH"
    }

    toml_get_string() {
        local raw
        raw="$(toml_get_raw "$1" "$2" | head -n 1)"
        strip_quotes "$raw"
    }

    toml_get_bool() {
        local raw
        raw="$(trim "$(toml_get_raw "$1" "$2" | head -n 1)")"
        if [[ "$raw" == "true" ]]; then
            printf '1'
        else
            printf '0'
        fi
    }

    toml_get_array() {
        local raw
        raw="$(toml_get_raw "$1" "$2")"
        if [[ -z "$raw" ]]; then
            return 0
        fi
        printf '%s\n' "$raw" \
          | grep -oE "\"[^\"]*\"|'[^']*'" \
          | sed -e 's/^"//' -e 's/"$//' -e "s/^'//" -e "s/'$//"
    }

    load_array_var() {
        local var_name="$1"
        local section="$2"
        local key="$3"
        local line
        eval "$var_name=()"
        while IFS= read -r line; do
            [[ -n "$line" ]] || continue
            eval "$var_name+=(\"\$line\")"
        done < <(toml_get_array "$section" "$key")
    }

    CONFIG_PATH="$(cd "$(dirname "$CONFIG_PATH")" && pwd)/$(basename "$CONFIG_PATH")"
    CONFIG_DIR="$(abs_dirname "$CONFIG_PATH")"

    local project_dir_raw ic0_support_raw icwasm_support_raw icwasm_sqlite_support_raw local_icwasm_raw
    local workdir_raw build_dir_raw build_label output_name build_command
    local c_file_expected_raw c_file_pattern gen_enabled did_raw prefix lib init cmd_map_raw
    local timer_cmd heartbeat_checkpoint heartbeat_cmd sql_stable out_raw stub_wasi stub_script_raw

    project_dir_raw="$(toml_get_string "paths" "project_dir")"
    ic0_support_raw="$(toml_get_string "paths" "ic0_support")"
    icwasm_support_raw="$(toml_get_string "paths" "icwasm_support")"
    icwasm_sqlite_support_raw="$(toml_get_string "paths" "icwasm_sqlite_support")"
    local_icwasm_raw="$(toml_get_string "paths" "local_idris2_icwasm")"

    PROJECT_DIR="$(resolve_path "${project_dir_raw:-.}")"
    IC0_SUPPORT="$(resolve_path "${ic0_support_raw:-$PROJECT_DIR/support/ic0}")"
    ICWASM_SUPPORT="$(resolve_path "${icwasm_support_raw:-$IC0_SUPPORT}")"
    ICWASM_SQLITE_SUPPORT="$(resolve_path "${icwasm_sqlite_support_raw:-}")"
    LOCAL_IDRIS2_ICWASM="$(resolve_path "${local_icwasm_raw:-${IDRIS2_ICWASM_BINARY:-}}")"

    build_label="$(toml_get_string "build" "label")"
    workdir_raw="$(toml_get_string "build" "workdir")"
    build_dir_raw="$(toml_get_string "build" "build_dir")"
    output_name="$(toml_get_string "build" "output_name")"
    build_command="$(toml_get_string "build" "command")"
    c_file_expected_raw="$(toml_get_string "build" "c_file_expected")"
    c_file_pattern="$(toml_get_string "build" "c_file_pattern")"

    BUILD_LABEL="${build_label:-$(basename "$PROJECT_DIR")}"
    WORKDIR="$(resolve_path "${workdir_raw:-$PROJECT_DIR}")"
    BUILD_DIR="$(resolve_path "${build_dir_raw:-$PROJECT_DIR/build}")"
    DEFAULT_OUTPUT_NAME="${output_name:-canister}"
    BUILD_COMMAND="${build_command:-}"
    C_FILE_EXPECTED="$(resolve_path "${c_file_expected_raw:-}")"
    C_FILE_PATTERN="${c_file_pattern:-*.c}"

    load_array_var C_FILE_FIND_DIRS "build" "c_file_find_dirs"
    if [[ ${#C_FILE_FIND_DIRS[@]} -eq 0 ]]; then
        C_FILE_FIND_DIRS=("$BUILD_DIR")
    else
        local i
        for i in "${!C_FILE_FIND_DIRS[@]}"; do
            C_FILE_FIND_DIRS[$i]="$(resolve_path "${C_FILE_FIND_DIRS[$i]}")"
        done
    fi

    gen_enabled="$(toml_get_bool "gen_entry" "enabled")"
    did_raw="$(toml_get_string "gen_entry" "did")"
    prefix="$(toml_get_string "gen_entry" "prefix")"
    lib="$(toml_get_string "gen_entry" "lib")"
    init="$(toml_get_string "gen_entry" "init")"
    cmd_map_raw="$(toml_get_string "gen_entry" "cmd_map")"
    timer_cmd="$(toml_get_string "gen_entry" "timer_cmd")"
    heartbeat_checkpoint="$(toml_get_string "gen_entry" "heartbeat_checkpoint")"
    heartbeat_cmd="$(toml_get_string "gen_entry" "heartbeat_cmd")"
    sql_stable="$(toml_get_bool "gen_entry" "sql_stable")"
    sql_vfs_memory_id="$(toml_get_string "gen_entry" "sql_vfs_memory_id")"
    out_raw="$(toml_get_string "gen_entry" "out")"

    DEFAULT_CANISTER_ENTRY="$(resolve_path "${out_raw:-$IC0_SUPPORT/canister_entry.c}")"
    GEN_ENTRY_ENABLED="$gen_enabled"
    GEN_ENTRY_ARGS=()
    if [[ "$GEN_ENTRY_ENABLED" == "1" ]]; then
        GEN_ENTRY_ARGS+=("--did=$(resolve_path "$did_raw")")
        GEN_ENTRY_ARGS+=("--prefix=$prefix")
        [[ -n "$lib" ]] && GEN_ENTRY_ARGS+=("--lib=$lib")
        [[ -n "$init" ]] && GEN_ENTRY_ARGS+=("--init=$init")
        [[ -n "$cmd_map_raw" ]] && GEN_ENTRY_ARGS+=("--cmd-map=$(resolve_path "$cmd_map_raw")")
        [[ -n "$timer_cmd" ]] && GEN_ENTRY_ARGS+=("--timer-cmd=$timer_cmd")
        [[ -n "$heartbeat_checkpoint" ]] && GEN_ENTRY_ARGS+=("--heartbeat-checkpoint=$heartbeat_checkpoint")
        [[ -n "$heartbeat_cmd" ]] && GEN_ENTRY_ARGS+=("--heartbeat-cmd=$heartbeat_cmd")
        [[ "$sql_stable" == "1" ]] && GEN_ENTRY_ARGS+=("--sql-stable")
        [[ -n "$sql_vfs_memory_id" ]] && GEN_ENTRY_ARGS+=("--sql-vfs-memory-id=$sql_vfs_memory_id")
        GEN_ENTRY_ARGS+=("--out=$DEFAULT_CANISTER_ENTRY")
    fi

    load_array_var PREFLIGHT_COMMANDS "preflight" "commands"
    load_array_var PRE_LINK_COMMANDS "pre_link" "commands"
    load_array_var FFI_HEADERS "ffi" "headers"
    load_array_var EXTRA_FORCE_INCLUDES "ffi" "force_includes"
    load_array_var EXTRA_C_FILES "link" "extra_c_files"
    load_array_var EXTRA_INCLUDE_DIRS "link" "extra_include_dirs"
    load_array_var EXTRA_EMCC_FLAGS "link" "extra_emcc_flags"

    local i
    for i in "${!EXTRA_FORCE_INCLUDES[@]}"; do
        EXTRA_FORCE_INCLUDES[$i]="$(resolve_path "${EXTRA_FORCE_INCLUDES[$i]}")"
    done
    for i in "${!EXTRA_C_FILES[@]}"; do
        EXTRA_C_FILES[$i]="$(resolve_path "${EXTRA_C_FILES[$i]}")"
    done
    for i in "${!EXTRA_INCLUDE_DIRS[@]}"; do
        EXTRA_INCLUDE_DIRS[$i]="$(resolve_path "${EXTRA_INCLUDE_DIRS[$i]}")"
    done

    append_unique() {
        local var_name="$1"
        local value="$2"
        local existing
        local found=0
        local restore_nounset=0
        case "$-" in
            *u*)
                restore_nounset=1
                set +u
                ;;
        esac
        eval 'for existing in "${'"$var_name"'[@]}"; do if [[ "$existing" == "$value" ]]; then found=1; break; fi; done'
        [[ "$restore_nounset" == "1" ]] && set -u
        [[ "$found" == "1" ]] && return 0
        eval "$var_name+=(\"\$value\")"
    }

    if [[ -n "$sql_vfs_memory_id" ]]; then
        if [[ -z "$ICWASM_SQLITE_SUPPORT" ]]; then
            echo "Error: gen_entry.sql_vfs_memory_id requires paths.icwasm_sqlite_support" >&2
            exit 1
        fi
        # VFS implementation selection. The append-only sqlite_vfs_bridge.c is the
        # default; setting BULLETPROOF_VFS=1 swaps in bulletproof_vfs.c (the in-place
        # vertex-BC VFS that borrows atomicity from IC message atomicity, bounding
        # stable allocation at ~M). Both provide the SAME FFI boundary and
        # sqlite3_os_init, so exactly one must be linked.
        VFS_IMPL_C="$ICWASM_SQLITE_SUPPORT/sqlite_vfs_bridge.c"
        if [[ "${BULLETPROOF_VFS:-0}" == "1" ]]; then
            VFS_IMPL_C="$ICWASM_SQLITE_SUPPORT/bulletproof_vfs.c"
            echo "[vfs] BulletproofVFS selected (in-place, message-atomic; stable bounded at ~M)"
        fi
        for required in \
            "$ICWASM_SQLITE_SUPPORT/sqlite_bridge.h" \
            "$ICWASM_SQLITE_SUPPORT/sqlite_vfs_bridge.h" \
            "$ICWASM_SQLITE_SUPPORT/sqlite_bridge.c" \
            "$VFS_IMPL_C" \
            "$ICWASM_SQLITE_SUPPORT/sqlite/libsqlite3.a"; do
            [[ -f "$required" ]] || { echo "Error: SQLite VFS support file not found: $required" >&2; exit 1; }
        done
        append_unique EXTRA_FORCE_INCLUDES "$ICWASM_SQLITE_SUPPORT/sqlite_bridge.h"
        append_unique EXTRA_FORCE_INCLUDES "$ICWASM_SQLITE_SUPPORT/sqlite_vfs_bridge.h"
        append_unique EXTRA_C_FILES "$ICWASM_SQLITE_SUPPORT/sqlite_bridge.c"
        append_unique EXTRA_C_FILES "$VFS_IMPL_C"
        append_unique EXTRA_C_FILES "$ICWASM_SQLITE_SUPPORT/sqlite/libsqlite3.a"
        append_unique EXTRA_INCLUDE_DIRS "$ICWASM_SQLITE_SUPPORT"
        append_unique EXTRA_INCLUDE_DIRS "$ICWASM_SQLITE_SUPPORT/sqlite"
    fi

    stub_wasi="$(toml_get_bool "wasi" "stub")"
    stub_script_raw="$(toml_get_string "wasi" "stub_script")"
    ENABLE_WASI_STUB="$stub_wasi"
    STUB_WASI_SCRIPT="$(resolve_path "${stub_script_raw:-}")"
}

run_shell_command_in_workdir() {
    local cmd="$1"
    (
        cd "$WORKDIR"
        export PROJECT_DIR WORKDIR BUILD_DIR MINI_GMP IC0_SUPPORT ICWASM_SUPPORT ICWASM_SQLITE_SUPPORT LOCAL_IDRIS2_ICWASM DEFAULT_OUTPUT_NAME
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
