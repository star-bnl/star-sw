# STAR Software Build System (`mgr/`)

The STAR software stack uses **cons**, a make-replacement build tool written
in Perl.  The `mgr/` directory contains the cons tool itself and all
supporting Perl modules, scripts, and configuration files.  This document
describes how the build system works, the rules it applies to each part of
the source tree, the supported architectures and compiler options, and which
options are effectively dead.

---

## 1. How the Build System Works

### Entry Points

| File | Role |
|---|---|
| `mgr/Construct` | Top-level build script; processed by cons at startup |
| `mgr/ConsDefs.pm` | Perl module loaded by `Construct`; defines the entire build environment (compilers, flags, external-package locations) |
| `mgr/Conscript-standard` | Default per-package build rules; executed once for every package directory |
| `mgr/ConsExtensions.pm` | Supplementary cons methods (Windows-only; see §4) |

### Build Flow

1. `cons` reads `mgr/Construct` using the Perl interpreter embedded in
   `mgr/cons`.
2. `Construct` loads `ConsDefs.pm`, which queries the host environment
   (`$STAR_HOST_SYS`, compiler executables, `root-config`, `mysql_config`,
   `pkg-config`, etc.) and pushes a large set of key/value pairs onto
   `@param::defaults`.  These become the build environment object `$env`.
3. `Construct` determines output directory names from the build mode
   (debug / optimized / profiling / Insure; see §3).  The build root is
   `.$STAR_HOST_SYS/` relative to the checkout root:

   | Mode trigger | Output sub-tree |
   |---|---|
   | Default (debug) | `lib/`, `bin/`, `obj/` |
   | `NODEBUG` set | `LIB/`, `BIN/`, `OBJ/` |
   | `GPROF` set | `GLIB/`, `GBIN/`, `GOBJ/` |
   | `CXX` contains `-Zoi` (Insure++) | `ILIB/`, `IBIN/`, `IOBJ/` |

4. Headers marked for export (any directory containing a
   `.includes_for_export.flg` file) are installed into the shared
   `include/` directory under the build root.  Special handling exists for
   `StRoot/Vc/` and `StRoot/TBB/` sub-trees.
5. `Construct` walks the source tree to build a list of package
   directories (see §2) and maps each to a target Conscript file path of
   the form `<OBJ>/<dir>/Conscript`.
6. `cons` executes each package Conscript.  If the package provides its
   own `Conscript` file, that is used; otherwise `mgr/Conscript-standard`
   is used (it is configured as `$param::defaultConscript`).
7. `Conscript-standard` discovers sources by globbing the package directory
   for every recognised extension, generates any required derived sources,
   compiles everything, and links into a shared library `lib<PKG>.so`.

### Repository Support

`Construct` calls `Repository($STAR)` to register the reference installation
as a fallback source.  Any file not found locally is transparently fetched
from `$STAR`.  A *Salt* string (default `"user"`) is added to each derived
file's signature when a repository is configured, preventing local objects
from polluting the repository cache.

### Code-generation Steps (within `Conscript-standard`)

| Step | Trigger | Tool | Output |
|---|---|---|---|
| ROOT dictionary | Header contains `ClassDef` or `StCollectionDef` | `mgr/RootCint.pl` → `rootcint`/`rootcling` | `<PKG>_Cint.cxx`, `<PKG>_Cint.h`; also `<PKG>_Cint_rdict.pcm` for ROOT 6 |
| IDL module tables | `.idl` files in package directory | `stic` → `ConstructTable.pl` → `rootcint` | `St_<stem>_Module.{h,cxx}`, `St_<stem>_Table.{h,cxx}`, table dictionaries |
| IDL pam tables | `.idl` files under `pams/*/idl` | Same pipeline | Installed to `include/tables/` |
| Qt UI files | `.ui` files | `uic` (Qt 3: → `.cxx`+`.h`; Qt 4: → `ui_*.h`) | Generated C++ |
| Qt resource files | `.qrc` files (Qt 4 only) | `rcc` | `qrc_*.cxx` |
| Qt MOC | Header contains `Q_OBJECT` | `moc` | `moc_*.cxx` |
| AgML geometry | XML files under `StarVMC/Geometry/` | `mgr/agmlParser.py` (with `mgr/Dyson/`) | `<module>.cxx`, `<module>.h`, `<module>.age` (Mortran) |
| AGETOF Mortran | `.g` or `.age` files | `agetof` → Fortran → `FC` | Object file |
| KUIP/CDF | `.cdf` files | `kuipc` → Fortran → `FC` | Object file |

### Key Supporting Scripts

| Script | Purpose |
|---|---|
| `mgr/cons` | The cons executable (Perl script, ~6 000 lines) |
| `mgr/RootCint.pl` | Wrapper that calls `rootcint`/`rootcling` and fixes up output paths |
| `mgr/ConstructTable.pl` | Generates table-class C++ and `LinkDef` headers from IDL |
| `mgr/agmlParser.py` | AgML XML → C++/Mortran geometry code (uses `mgr/Dyson/`) |
| `mgr/SkipDir.pl` | Utility to list which directories would be skipped |
| `mgr/warnoff_dirs.txt` | Regex list of package paths for which `-Werror` is suppressed |
| `mgr/BFCOpt2Html.pl` | Generates HTML documentation for `StBFChain` options |
| `mgr/precint.pl` | Pre-processes headers for rootcint |
| `mgr/config/` | CSH environment-setup scripts for specific RHEL7 + ROOT combinations |

### Invoking the Build

```csh
# From the checkout root (standard optimized build):
setenv NODEBUG 1
cons

# Build only specific packages:
cons +StEvent +StMuDSTMaker

# Skip extra packages beyond the default exclusion list:
setenv SKIP_DIRS "StFooMaker|StBarMaker"
cons

# Enable -Werror (warnings treated as errors):
cons EXTRA_CXXFLAGS=-Werror

# Debug build (no NODEBUG):
cons

# Profiling build:
cons GPROF=1
```

---

## 2. Directory Rules

### Top-level Source Trees

`Construct` iterates over these directories to collect package targets:

| Directory | Traversal depth | Notes |
|---|---|---|
| `StRoot/` | 1 level (each subdirectory = one package) | Main maker/package tree |
| `StPiD/` | 1 level | Particle-ID packages |
| `StarVMC/` | 1 level; Pool/Generator/Client sub-trees go 2 levels | VMC simulation |
| `pams/` | 1 level (plus `sim/`, `gen/` go 2 levels) | Legacy STAF packages |

In addition, a fixed list of system directories (`sysdirlist`) is always
prepended to the target list (see below).

### Directories Always Excluded from Traversal

The following directory names are never treated as package targets regardless
of their position in the tree:

```
.  ..  macros  CVS  html  doc  inc  idl  local  run  wrk  example  include  kumac*
```

### Default Skipped Packages (`SKIP_DIRS`)

These packages are skipped unless explicitly requested with `+<name>` on
the cons command line:

- `StShadowMaker`
- `PWGTools`

The `SKIP_DIRS` environment variable overrides the default list entirely
(pipe-separated).  On `.dev` and `.DEV` installations all packages are
attempted.

### Conditional Exclusions

| Condition | Excluded packages |
|---|---|
| `STAR_HOST_SYS =~ /sl61_gcc445/` | `St_geom_Maker`, OnlTools (broken Qt4) |
| `STAR_HOST_SYS =~ /x86_darwin/` | `St_geom_Maker`, `GeoTestMaker`, `Pythia6`, `StEStructPool`, `StMCFilter`, `StTriggerUtilities`, `StSpinPool`, DAQBrowser |
| `ROOT_VERSION_MAJOR < 6` | `StRoot/Table` |

### Fixed System Directories (`sysdirlist`)

Always built before the auto-discovered package list:

| Directory | Platform |
|---|---|
| `asps/Simulation/agetof` | All |
| `asps/staf/sdd` | All |
| `StDb/` | All |
| `asps/DAQBrowser` | Linux only |
| `asps/Simulation/geant321` | Linux only |
| `asps/Simulation/gcalor` | Linux only |
| `asps/rexe` | Linux only |
| `asps/Simulation/starsim` | Linux only |
| `OnlTools/OnlinePlots`, `OnlTools/PDFUtil`, `OnlTools/StOnlineDisplay`, `OnlTools/Jevp` | Linux, non-`sl61_gcc445` only |
| `QtRoot/` | Linux only |

### Two-Level Directory Sub-trees

For directories matching the patterns below, `Construct` descends one extra
level and registers each sub-directory as a separate package:

- `StRoot/*/Pool/` — detector pool packages (e.g. `StBTofPool/StBTofMatchMaker`)
- `StRoot/*/Generator/` — event generator packages (e.g. `StarGenerator/Pythia8_...`)
- `StRoot/StRTSClient/` — online/RTS client packages
- `StarVMC/minicern/` — treated as `StarMiniCern`

### Package-Specific Build Overrides in `Conscript-standard`

The following packages receive customised compiler flags, include paths, or
link libraries beyond the defaults:

| Package(s) | Customisation |
|---|---|
| `RTS` | `CPPFLAGS` for DAQ reader; strips `__ROOT__`; custom `CPPPATH` into RTS headers |
| `gcalor`, `geant321` | Static library only (no `.so`); custom `FCPATH` into simulation include trees; CERNLIB `FPPFLAGS` |
| `geant3` (StarVMC) | CERNLIB `FPPFLAGS`/`CPPFLAGS`; custom Fortran sub-directory filter |
| `StarMiniCern` | Custom `FCPATH`; excludes platform-specific sub-directories (`hpxgs`, `sungs`, `lnxgs`, etc.) |
| `xgeometry` / `StarGeometry` | Optimization forced off (`DEBUG=-g`); sources generated from AgML XML at build time |
| `Sti`, `TPCCATracker` | SSE/SSE2 vectorisation flags; Vc library; TBB; pedantic warnings |
| `StDbLib`, `Jevp` | MySQL + libxml2 include/lib paths |
| `St*Db`, `StStarLogger`, `StUCMApi`, `StTriggerUtilities` | MySQL include path; log4cxx linking |
| `OnlinePlots` | Qt thread/shared flags; extra include paths |
| `StEventDisplay` and all Qt-using packages | Qt include/lib paths and Qt version flags |
| `apythia` | PYTHIA6/PDFLIB whole-archive linking from CERNLIB |
| `Pythia8*` | Custom include paths; `DPythia8_version` and `DXMLDIR` macros; HepMC/examples excluded |
| `EvtGen` | HepMC, Pythia8, Photos, Tauola includes; test/validation sources excluded |
| `HepMC` | Version macro; test/example sources excluded |
| `Photos`, `Tauola` | Complex include tree surgery; test/example sources excluded |
| `UrQMD` | Fixed-form line length overridden to 72 |
| `StarGeneratorFilt` | FastJet include path |
| `StJetFinder` | FastJet include path |
| `StHbtMaker` | GSL include path; multi-level `CPPPATH` |
| `StHbtMaker`, `StMuDSTMaker` | Macro workaround for `__PRETTY_FUNCTION__` on non-gcc compilers |
| `StTrsMaker`, `pythia8` | Package directory replaces global `CPPPATH` |
| `StBFChain` | HTML documentation generated from `BigFullChain.h` (dev builds only) |
| `TGeant3`, `StarVMCApplication`, `StVMCMaker` | `CPPPATH` prepended with Geant3 source tree |
| `StFwdTrackMaker` | `$OPTSTAR/include` appended to `CXXFLAGS` |
| `StAssociationMaker`, `Sti`, `StMuDSTMaker`, `StJetMaker`, various Pool packages | Sub-directory globbing to build a flat `CPPPATH` |
| `StRTSClient` sub-packages | Include path includes client base `include/` |

---

## 3. Supported Architectures

Architecture detection is performed once in `ConsDefs.pm` by matching
`$STAR_HOST_SYS` (derived from the `sys` command or the environment
variable of the same name) against a set of patterns.

### Architecture Patterns and Compilers

| `STAR_HOST_SYS` pattern | OS | C/C++ compiler | Fortran compiler |
|---|---|---|---|
| `^i386_linux*` | 32-bit Linux | `gcc` / `g++` | `gfortran` (or `g77`) |
| `^rh*` | Red Hat Linux (any version) | `gcc` / `g++` | `gfortran` (or `g77`) |
| `^sl*` (not `_icc`) | Scientific Linux | `gcc` / `g++` | `gfortran` (or `g77`) |
| `^sl*_icc` or `^rh*_icc` | Linux + Intel compiler | `icc` (C and C++) | `ifort` (ICC ≥ 8) or `ifc` (ICC < 8) |
| `^x86_darwin` | macOS 10.3–10.6 | `gcc` / `g++` | `gfortran` (or `g95` on 10.4 with ROOT/g95) |

**Note:** Any `STAR_HOST_SYS` not matching the above patterns causes
`ConsDefs.pm` to `die` with *"Unsupported platform"*.

If `$PGI` is defined (Portland Group), `pgf77` is used for simulation
packages (`pams/sim`, `pams/gen`, `tls`, `minicern`, `geant3`), reverting
to `g77`/`gfortran` for all others.  In practice this fails at link time
(see §4).

If `USE_G77=1` is set in the environment, `g77` is used instead of
`gfortran` (deprecated; at user's own risk).

### C++ Standard Selection (gcc)

| gcc version | Flag added |
|---|---|
| < 3.0 | (none; `ST_NO_NUMERIC_LIMITS` etc. defined) |
| 3.0–3.3 | `-pedantic` |
| 3.0–4.4 | `-ansi` |
| ≥ 4.4 | `-std=c++0x` |

### Compile-time Build Modes

| Environment / argument | Effect on `DEBUG`/`FDEBUG` | Output directories |
|---|---|---|
| *(none — debug build)* | `-g` | `lib/`, `bin/`, `obj/` |
| `NODEBUG=1` (env) | `-O2 -g` (plus alignment flags) | `LIB/`, `BIN/`, `OBJ/` |
| `OPTIM_OPTIONS=<flags>` or `DEBUG_OPTIONS=<flags>` | Overrides default optimisation string | (same as NODEBUG) |
| `GPROF=1` (env or cons arg) | Adds `-pg` to all compiler/linker flags | `GLIB/`, `GBIN/`, `GOBJ/` |
| `CXX` contains `-Zoi` | Insure++ instrumentation (see §4) | `ILIB/`, `IBIN/`, `IOBJ/` |
| `USE_64BITS=1` (env) | Adds `-m64`; uses `lib64`; CERNLIB `QMLXIA64` flag | (unchanged, 64-bit ABI) |
| `USE_64BITS=0` | Adds `-m32` (gcc ≥ 4); uses `lib` | (32-bit ABI; mostly broken on modern systems — see §4) |

### gcc 4.x Optimisation Tweaks

gcc 4 series required specific workarounds that are still present:

| gcc version range | Workaround |
|---|---|
| 4.0–4.49 | `-fno-inline` (symbol removal bug in 4.4.7) |
| 4.8.x (default) | `DEBUG=-O2 -g`; `FDEBUG` uses selective inlining flags |
| 4.8.x (`OPTIM_WITH_O`) | Fine-grained optimisation set in `FDEBUG` |
| All 4.x with SSE | `-DVC_IMPL=SSE`; `-DVC_IMPL=Scalar` on `gcc432` or no-SSE hosts |

### Key Preprocessor Macros Defined by the Build

| Macro | When defined |
|---|---|
| `__ROOT__` | All compilations (injected via `CPPFLAGS`) |
| `<STAR_HOST_SYS>` | All compilations (e.g. `sl64_x8664_gcc447`) |
| `CERNLIB_TYPE`, `CERNLIB_DOUBLE`, `CERNLIB_NOQUAD`, `CERNLIB_LINUX` | All Linux builds (base CERNLIB flags) |
| `CERNLIB_BLDLIB`, `CERNLIB_CZ`, `CERNLIB_QMGLIBC` | Linux |
| `CERNLIB_GCC<N>` | Linux; `<N>` = major GCC version |
| `CERNLIB_GFORTRAN` | Linux with gfortran |
| `CERNLIB_QMLXIA64`, `CERNLIB_LXIA64` | 64-bit Linux |
| `CERNLIB_MACOSX`, `__DARWIN__` | macOS |
| `CERNLIB_LINUX`, `CERNLIB_BLDLIB`, ... | Intel ICC builds |
| `NEW_DAQ_READER` | All packages (injected unconditionally) |

### External Package Detection

The following external packages are located at configure time and their
paths injected into the build environment:

| Package | Detection method | Used by |
|---|---|---|
| ROOT | `root-config --libs`, `--cflags`, `--version` | All |
| MySQL | `mysql_config` | Database packages |
| libxml2 | `xml2-config` | `StDbLib`, `Jevp` |
| log4cxx | `pkg-config --variable=prefix liblog4cxx` | `StStarLogger`, `StUCMApi` |
| Qt 3/4 | `$QTDIR/bin/moc` presence | Display and online tools |
| FastJet | `fastjet-config --prefix` | `StJetFinder`, `StarGeneratorFilt` |
| GSL | `gsl-config --prefix` | `StHbtMaker` |
| Coin3D/SoQt | `coin-config` in `$IVROOT` or `$OPTSTAR` | `StEventDisplay` (optional) |
| CERNLIB | `cernlib` command in `$CERN_ROOT/bin` | Simulation packages |

---

## 4. Options No Longer Functional

The following features are present in the build scripts but are effectively
dead on any currently-supported platform:

### Compiler/Toolchain Options

| Feature | Location | Reason non-functional |
|---|---|---|
| **`g77` Fortran** (`USE_G77=1`) | `ConsDefs.pm` | `g77` was removed from GCC in the 4.x series; the binary does not exist on any modern system |
| **`icc`/`ifort` (Intel Classic)** (`*_icc` arch tag) | `ConsDefs.pm` | Intel Classic compilers reached end-of-life (2023); version detection uses `icc -V` which does not work with the oneAPI replacement `icx` |
| **PGI Fortran** (`$PGI` set) | `ConsDefs.pm`, `Conscript-standard` | Even the code comments "fail at link-time — TBC"; Portland Group was acquired by NVIDIA; `pgf77` no longer exists as a standalone binary |
| **`g95`** (macOS 10.4 + `$ROOTBUILD =~ /g95/`) | `ConsDefs.pm` | `g95` development stopped in 2012 |
| **KAI C++ (`kcc`)** | `Conscript-standard` | KAI C++ was discontinued in 2002; code zeroes LIBPATH/LIBS for `$STAR_SYS =~ /kcc$/` but the compiler cannot be invoked |
| **Insure++ (`-Zoi` in CXX)** | `Construct` | Insure++ was a 32-bit-only proprietary tool (Parasoft); end-of-life on 64-bit Linux; `ILIB/IBIN/IOBJ` directories are never created |

### Platform Options

| Feature | Location | Reason non-functional |
|---|---|---|
| **32-bit builds** (`USE_64BITS=0`) | `ConsDefs.pm`, `Construct` | Modern RHEL/Alma9 systems no longer ship 32-bit system libraries (`.i686` packages) by default; linking fails |
| **macOS (`x86_darwin`)** | `ConsDefs.pm` | Support targets macOS 10.3–10.6 (PowerPC through early Intel); not updated for modern macOS; multiple packages explicitly excluded; Apple dropped Fink and changed the toolchain substantially |
| **32-bit macOS PowerPC** | `ConsDefs.pm` | `$MACOSX_CPU eq "powerpc"` branch for `-lg2c`; PowerPC Macs are long obsolete |

### Build Features

| Feature | Location | Reason non-functional |
|---|---|---|
| **LIBMAP / `rlibmap`** | `Construct` (lines 446–476) | Entire block is commented out; `rlibmap` was replaced by ROOT's own map mechanism |
| **`gccfilter`** | `ConsDefs.pm` | `mgr/gccfilter` is not present in the repository; the code detects its absence and silently falls back to unfiltered output |
| **Windows DLL/EXE builds** | `ConsExtensions.pm` | The entire file contains MSVC-specific methods (`SharedLibrary`, `StaticLibrary`, `ResourceLibrary`, `WindowsProgram`, `ConsoleProgram`, `MFCSharedLibrary`, `WindowsModule`, `PCHObject`); these use `/Zi`, `/dll`, `.pdb`, `.exp`, `.lib` Windows conventions and are non-functional on any UNIX platform |
| **AFS libraries** (`AFSFLAGS`, `AFSLIBS`) | `ConsDefs.pm` | Variables are defined but never injected into any compilation or link command; STAR migrated off AFS |
| **`LinksCheck` argument** | `Construct` | Walks the build tree to remove broken symlinks; was designed for AFS-based builds where dangling links accumulated; rarely useful now |
| **`Salt` / `NoSalt` / `NoKeep` arguments** | `Construct` | Salt logic is present but the lines that would set `$param::defaultConscript` are commented out; the mechanism still works but has no effect on the chosen Conscript |
| **DSP/DSW project file generation** (`ProjectFile`, `DSPGEN`) | `ConsExtensions.pm` | Generates Visual Studio 6 `.dsp`/`.dsw` files; VS6 is obsolete (released 1998) |
| **`find_PACKAGE` / `PACKAGE` file detection** | `Construct` | `find_PACKAGE` and `wanted_PACKAGE` subroutines are defined but the call `find_PACKAGE(\&find_PACKAGE, ".")` is commented out; per-package `PACKAGE` marker files are not used |
| **Coin3D / Open Inventor** (`IVROOT`, `COIN3D`) | `ConsDefs.pm` | The original IVROOT logic is commented out with the note "Coin3D - WHAT??! JL 2009"; fallback via `coin-config` may still work if installed, but the only consumer (`StEventDisplay`) is not actively maintained |

### Fortran Runtime Libraries

| Variable | Status |
|---|---|
| `$LIBG2C` (`-lg2c`) | Defined and queried from `gcc -print-file-name=libg2c.a`; the library does not exist on any GCC 4+ system; the variable is populated but unused when `gfortran` is the selected compiler |
| `$LIBFRTBEGIN` (Intel) | Queried from `gcc -print-file-name=libfrtbegin.a`; not present on any modern Intel oneAPI installation |
