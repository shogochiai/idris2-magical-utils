||| The canonical `RuntimeHost` — "what machine etherclaw ITSELF is running on": the
||| host OS and (for Linux) its distro family. This is the SECOND axis, ORTHOGONAL to
||| `Idris2.DeliveryKind`:
|||   - DeliveryKind (axis A): what a deliverable is delivered AS (evm/web/android/…).
|||   - RuntimeHost  (axis C): where etherclaw runs (macOS / a Linux distro / unknown).
||| A function dispatching on RuntimeHost is the runtime twin of one dispatching on
||| DeliveryKind: adding a host adds a constructor, and the compiler then points at every
||| total projection that must handle it — so a macOS-only assumption can never silently
||| ride along (the old `/opt/homebrew/...` / `/Users/bob/...` hardcodes are exactly that
||| silent ride, and this type is what makes them structural instead).
|||
||| ★ONE source, MANY projections — NOT many enums. The package manager, the lib suffix,
||| the `stat` mtime flag, the default JDK / Android-SDK roots, and the step3 accelerator
||| are all DERIVED from RuntimeHost by total functions. We do NOT introduce a separate
||| `PackageManagerKind` enum detected independently of the host, nor a separate
||| `Step3Accelerator` axis that could drift from the host — those are PROJECTIONS of
||| RuntimeHost, the way coverageFamilyOf/deliveryTag are projections of DeliveryKind.
||| (The step3 "how do I CALL the backend" axis — builtin/cmd/http — is a DIFFERENT,
||| already-complete type in EtherClaw.Commands.Step3Elm; this type does NOT absorb it.)
|||
||| DEPENDENCY DISCIPLINE: `base` ONLY, so the app, the canister, etherclaw, and the
||| bootstrap-facing `etherclaw runtime detect` command can all depend on it.
module Idris2.RuntimeHost

%default total

-- =============================================================================
-- Linux distro family (the payload that rides on the Linux constructor)
-- =============================================================================

||| The Linux distro FAMILY — only as coarse as the things that actually differ per
||| host: which package manager installs system deps, and which default tool roots
||| apply. (We deliberately do not enumerate every distro; `OtherLinux` carries the
||| raw os-release id so an unknown distro is recorded, not lost.)
public export
data LinuxDistro
  = Debian            -- debian/ubuntu/… → apt-get
  | Fedora            -- fedora/rhel/centos → dnf
  | Arch              -- arch/manjaro → pacman
  | OtherLinux String -- any other /etc/os-release ID (kept verbatim)

public export
Eq LinuxDistro where
  Debian        == Debian        = True
  Fedora        == Fedora        = True
  Arch          == Arch          = True
  (OtherLinux a) == (OtherLinux b) = a == b
  _             == _             = False

-- =============================================================================
-- The canonical RuntimeHost
-- =============================================================================

||| The host etherclaw runs on. Linux carries its distro family as a payload (the way
||| DeliveryKind's ICP carries CyclesConfig): the distro is Linux-specific and lives ONLY
||| on the Linux constructor, impossible with a flat enum. `UnknownHost` keeps the raw
||| `uname -s` so an unrecognised OS is surfaced honestly, never silently treated as one
||| of the known hosts.
public export
data RuntimeHost
  = MacOS                       -- Darwin
  | Linux (Maybe LinuxDistro)   -- a Linux host; distro family optional (Nothing = generic)
  | UnknownHost String          -- uname -s we did not recognise (verbatim)

public export
Eq RuntimeHost where
  MacOS          == MacOS          = True
  (Linux a)      == (Linux b)      = a == b
  (UnknownHost a) == (UnknownHost b) = a == b
  _              == _              = False

-- =============================================================================
-- Package manager — a PROJECTION of RuntimeHost (not an independently-detected enum)
-- =============================================================================

||| The system package manager used to install host deps (gmp, jdk, …). Derived from the
||| host: macOS→Brew, Debian→Apt, Fedora→Dnf, Arch→Pacman. `NoPackageManager` is the
||| honest answer for a generic/unknown Linux or unknown host — the caller must then tell
||| the operator to install deps manually rather than guess a manager that isn't there.
public export
data PackageManager = Brew | Apt | Dnf | Pacman | NoPackageManager

public export
Eq PackageManager where
  Brew             == Brew             = True
  Apt              == Apt              = True
  Dnf              == Dnf              = True
  Pacman           == Pacman           = True
  NoPackageManager == NoPackageManager = True
  _                == _                = False

||| The install command prefix for one package, or Nothing if the host has no known
||| package manager (caller surfaces a manual-install hint). Total over PackageManager.
public export
installCommand : PackageManager -> Maybe (String -> String)
installCommand Brew             = Just (\p => "brew install " ++ p)
installCommand Apt              = Just (\p => "sudo apt-get install -y " ++ p)
installCommand Dnf              = Just (\p => "sudo dnf install -y " ++ p)
installCommand Pacman           = Just (\p => "sudo pacman -S --noconfirm " ++ p)
installCommand NoPackageManager = Nothing

||| The host's package manager. The ONE place host→manager is decided.
public export
packageManager : RuntimeHost -> PackageManager
packageManager MacOS                       = Brew
packageManager (Linux (Just Debian))       = Apt
packageManager (Linux (Just Fedora))       = Dnf
packageManager (Linux (Just Arch))         = Pacman
packageManager (Linux (Just (OtherLinux _))) = NoPackageManager
packageManager (Linux Nothing)             = NoPackageManager
packageManager (UnknownHost _)             = NoPackageManager

-- =============================================================================
-- step3 accelerator — a PROJECTION of RuntimeHost (the "metal/gpu" axis)
-- =============================================================================

||| Which accelerator a LOCAL step3 (ollama) inference would use. This is the axis the
||| user flagged ("step3 metal/gpu branches"). It is a PROJECTION of the host (and, for
||| Linux, a runtime GPU probe the caller supplies), NOT an independently-stored field
||| that could drift: macOS Apple-Silicon ⇒ Metal; a Linux box ⇒ Cuda/Rocm if a GPU is
||| present else Cpu. (This is distinct from the step3 "how to CALL the backend" axis —
||| builtin/cmd/http — which is EtherClaw.Commands.Step3Elm.Step3Backend.)
public export
data Step3Accelerator = Metal | Cuda | Rocm | Cpu

public export
Eq Step3Accelerator where
  Metal == Metal = True
  Cuda  == Cuda  = True
  Rocm  == Rocm  = True
  Cpu   == Cpu   = True
  _     == _     = False

public export
step3AcceleratorTag : Step3Accelerator -> String
step3AcceleratorTag Metal = "metal"
step3AcceleratorTag Cuda  = "cuda"
step3AcceleratorTag Rocm  = "rocm"
step3AcceleratorTag Cpu   = "cpu"

||| The accelerator a local step3 would use, given the host and what GPUs a probe found
||| (hasCuda/hasRocm — the caller runs `nvidia-smi` / `rocminfo` and passes the booleans,
||| keeping THIS function pure & total). macOS is always Metal (Apple GPU). A Linux box
||| prefers Cuda, then Rocm, else Cpu. An unknown host falls back to Cpu (safe, slow).
public export
step3AcceleratorFor : RuntimeHost -> (hasCuda : Bool) -> (hasRocm : Bool) -> Step3Accelerator
step3AcceleratorFor MacOS           _    _    = Metal
step3AcceleratorFor (Linux _)       True _    = Cuda
step3AcceleratorFor (Linux _)       _    True = Rocm
step3AcceleratorFor (Linux _)       _    _    = Cpu
step3AcceleratorFor (UnknownHost _) _    _    = Cpu

-- =============================================================================
-- Platform string projections — the ONE place each host-difference lives
-- =============================================================================

||| The lossless host tag (wire/JSON, the `etherclaw runtime detect` output bash reads).
public export
hostTag : RuntimeHost -> String
hostTag MacOS                       = "macos"
hostTag (Linux (Just Debian))       = "debian"
hostTag (Linux (Just Fedora))       = "fedora"
hostTag (Linux (Just Arch))         = "arch"
hostTag (Linux (Just (OtherLinux d))) = "linux:" ++ d
hostTag (Linux Nothing)             = "linux"
hostTag (UnknownHost u)             = "unknown:" ++ u

public export
Show RuntimeHost where
  show = hostTag

||| The dynamic library suffix (verify-install's dylib-vs-so check, total over host).
public export
libExt : RuntimeHost -> String
libExt MacOS = "dylib"
libExt _     = "so"      -- Linux + unknown: ELF .so is the safe default

||| The `stat` invocation that prints a file's mtime epoch (install-built-app's
||| `stat -f '%m'` on BSD/macOS vs `stat -c '%Y'` on GNU/Linux). Total over host.
public export
statMtimeCmd : RuntimeHost -> (path : String) -> String
statMtimeCmd MacOS path = "stat -f '%m' " ++ path
statMtimeCmd _     path = "stat -c '%Y' " ++ path   -- GNU stat (Linux/unknown)

||| The default JDK home when JAVA_HOME is unset, derived from the host (this is the
||| `/opt/homebrew/opt/openjdk@17` macOS hardcode, now host-driven). Linux uses the
||| common distro JVM root; an unknown host has no good default (empty → caller gates).
public export
defaultJdkHome : RuntimeHost -> String
defaultJdkHome MacOS           = "/opt/homebrew/opt/openjdk@17"
defaultJdkHome (Linux _)       = "/usr/lib/jvm/java-17-openjdk"
defaultJdkHome (UnknownHost _) = ""

||| The default Android SDK root when ANDROID_SDK_ROOT is unset, derived from the host
||| (the `/opt/homebrew/share/android-commandlinetools` macOS hardcode, now host-driven).
public export
defaultAndroidSdk : RuntimeHost -> String
defaultAndroidSdk MacOS           = "/opt/homebrew/share/android-commandlinetools"
defaultAndroidSdk (Linux _)       = "/usr/lib/android-sdk"
defaultAndroidSdk (UnknownHost _) = ""

-- =============================================================================
-- Parsing — uname -s + /etc/os-release ID → RuntimeHost
-- =============================================================================

||| Lowercase an ASCII string (parsing is case-insensitive: "Darwin"/"darwin", distro
||| IDs vary in case). Self-contained so the package stays base-only.
lowerAscii : String -> String
lowerAscii = pack . map toLowerChar . unpack
  where
    toLowerChar : Char -> Char
    toLowerChar c = if c >= 'A' && c <= 'Z'
                      then chr (ord c + 32)
                      else c

||| Map an /etc/os-release ID (or ID_LIKE token) to a LinuxDistro family. Unknown IDs are
||| kept verbatim in OtherLinux so nothing is silently lost.
public export
linuxDistroFromId : String -> LinuxDistro
linuxDistroFromId raw =
  let id = lowerAscii raw in
  if id == "debian" || id == "ubuntu" || id == "linuxmint" || id == "pop" || id == "raspbian"
    then Debian
  else if id == "fedora" || id == "rhel" || id == "centos" || id == "rocky" || id == "almalinux"
    then Fedora
  else if id == "arch" || id == "manjaro" || id == "endeavouros"
    then Arch
  else OtherLinux raw

||| Parse `uname -s` (and, for Linux, an optional already-extracted os-release ID) into a
||| RuntimeHost. "Darwin"→MacOS; "Linux"→Linux (with the distro if an ID was given);
||| anything else→UnknownHost (verbatim). The ONE place the platform string becomes a type.
public export
runtimeHostFrom : (unameS : String) -> (osReleaseId : Maybe String) -> RuntimeHost
runtimeHostFrom unameS osReleaseId =
  let os = lowerAscii unameS in
  if os == "darwin"
    then MacOS
  else if os == "linux"
    then Linux (map linuxDistroFromId osReleaseId)
  else UnknownHost unameS

||| Every host (payload-free / generic representatives) — for enumeration & tests.
public export
allRuntimeHosts : List RuntimeHost
allRuntimeHosts =
  [ MacOS
  , Linux (Just Debian), Linux (Just Fedora), Linux (Just Arch), Linux Nothing
  , UnknownHost "Plan9"
  ]
