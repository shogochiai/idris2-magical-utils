||| Tests for Idris2.RuntimeHost — the projections (package manager, lib suffix, stat,
||| default tool roots, step3 accelerator) and the uname/os-release parse round-trip.
||| Mirrors Idris2.DeliveryKind.Tests in shape (a plain IO Bool harness, no deps).
module Idris2.RuntimeHost.Tests

import Idris2.RuntimeHost

%default total

record TestResult where
  constructor MkTestResult
  name : String
  ok   : Bool

check : String -> Bool -> TestResult
check = MkTestResult

tests : List TestResult
tests =
  [ -- parse: uname -s → host
    check "parse_darwin_is_macos" (runtimeHostFrom "Darwin" Nothing == MacOS)
  , check "parse_lowercase_darwin" (runtimeHostFrom "darwin" Nothing == MacOS)
  , check "parse_linux_no_distro" (runtimeHostFrom "Linux" Nothing == Linux Nothing)
  , check "parse_linux_ubuntu_is_debian"
      (runtimeHostFrom "Linux" (Just "ubuntu") == Linux (Just Debian))
  , check "parse_linux_fedora" (runtimeHostFrom "Linux" (Just "fedora") == Linux (Just Fedora))
  , check "parse_linux_arch" (runtimeHostFrom "Linux" (Just "manjaro") == Linux (Just Arch))
  , check "parse_unknown_kept_verbatim"
      (runtimeHostFrom "FreeBSD" Nothing == UnknownHost "FreeBSD")
  , check "distro_unknown_id_preserved"
      (linuxDistroFromId "gentoo" == OtherLinux "gentoo")

    -- package manager projection
  , check "pkgmgr_macos_brew" (packageManager MacOS == Brew)
  , check "pkgmgr_debian_apt" (packageManager (Linux (Just Debian)) == Apt)
  , check "pkgmgr_fedora_dnf" (packageManager (Linux (Just Fedora)) == Dnf)
  , check "pkgmgr_arch_pacman" (packageManager (Linux (Just Arch)) == Pacman)
  , check "pkgmgr_generic_linux_none" (packageManager (Linux Nothing) == NoPackageManager)
  , check "install_cmd_apt"
      (case installCommand Apt of
         Just f  => f "libgmp-dev" == "sudo apt-get install -y libgmp-dev"
         Nothing => False)
  , check "install_cmd_brew"
      (case installCommand Brew of
         Just f  => f "gmp" == "brew install gmp"
         Nothing => False)
  , check "install_cmd_none_is_nothing"
      (case installCommand NoPackageManager of Nothing => True; Just _ => False)

    -- platform string projections
  , check "libext_macos_dylib" (libExt MacOS == "dylib")
  , check "libext_linux_so" (libExt (Linux (Just Debian)) == "so")
  , check "stat_macos_bsd" (statMtimeCmd MacOS "f" == "stat -f '%m' f")
  , check "stat_linux_gnu" (statMtimeCmd (Linux Nothing) "f" == "stat -c '%Y' f")
  , check "jdk_macos_homebrew" (defaultJdkHome MacOS == "/opt/homebrew/opt/openjdk@17")
  , check "jdk_linux_jvm" (defaultJdkHome (Linux (Just Debian)) == "/usr/lib/jvm/java-17-openjdk")
  , check "android_sdk_macos" (defaultAndroidSdk MacOS == "/opt/homebrew/share/android-commandlinetools")
  , check "android_sdk_linux" (defaultAndroidSdk (Linux Nothing) == "/usr/lib/android-sdk")

    -- step3 accelerator projection (the metal/gpu axis)
  , check "accel_macos_always_metal" (step3AcceleratorFor MacOS False False == Metal)
  , check "accel_linux_cuda_when_nvidia" (step3AcceleratorFor (Linux Nothing) True False == Cuda)
  , check "accel_linux_rocm_when_amd" (step3AcceleratorFor (Linux Nothing) False True == Rocm)
  , check "accel_linux_cpu_when_no_gpu" (step3AcceleratorFor (Linux Nothing) False False == Cpu)
  , check "accel_cuda_preferred_over_rocm" (step3AcceleratorFor (Linux Nothing) True True == Cuda)
  , check "accel_unknown_cpu" (step3AcceleratorFor (UnknownHost "x") True True == Cpu)
  , check "accel_tag_metal" (step3AcceleratorTag Metal == "metal")
  , check "accel_tag_cuda" (step3AcceleratorTag Cuda == "cuda")

    -- host tag / show
  , check "hosttag_macos" (hostTag MacOS == "macos")
  , check "hosttag_debian" (hostTag (Linux (Just Debian)) == "debian")
  , check "hosttag_generic_linux" (hostTag (Linux Nothing) == "linux")
  , check "hosttag_otherlinux_prefixed" (hostTag (Linux (Just (OtherLinux "void"))) == "linux:void")
  , check "hosttag_unknown_prefixed" (hostTag (UnknownHost "Plan9") == "unknown:Plan9")
  , check "show_is_hosttag" (show (Linux (Just Debian)) == "debian")
  ]

main : IO ()
main = do
  let failed = filter (not . ok) tests
  let nTotal = length tests
  let nFail  = length failed
  putStrLn $ "RuntimeHost tests: " ++ show (nTotal `minus` nFail) ++ "/" ++ show nTotal ++ " passed"
  if nFail == 0
    then putStrLn "ALL PASS"
    else do
      putStrLn "FAILURES:"
      traverse_ (\t => putStrLn ("  FAIL " ++ t.name)) failed
