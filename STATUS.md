# AlmaLinux 9 Migration Status
**Branch:** `alma9-compilation-take-1`  
**Architecture:** `al96_x8664_gcc11` (AlmaLinux 9.6, x86_64, GCC 11.5.0)  
**Date:** 2026-06-01

---

## What Compiles Successfully on AL9

- **`agetof`** — AGE-to-Fortran translator builds and runs correctly
- **`stic`** — STAF IDL compiler (from `asps/staf/sdd`) builds and runs correctly
- **`pams` IDL processing** — all IDL files across `pams/` process cleanly through `stic`; all generated `.F` files compile with `gfortran`
- **`pams/geometry`** — all AGE geometry files (`.g`/`.age`) are translated to Fortran and compiled successfully; link step blocked (see below)
- **`starsim`** — links successfully with local build libraries; runtime is blocked by a CERNLIB ABI issue (see below)

---

## Build System Fixes Applied (Committed)

- Strip `-lnsl` from `$CERNLIBS` — AL9 has no unversioned `libnsl.so` linker stub; the symbols are provided by glibc
- Prefer locally-built `agetof.def` over central installation path (which does not yet exist for AL9)
- Add AlmaLinux (`^al`) to the Linux platform condition that sets `-Df2cFortran` in the starsim `Conscript`
- Fix linker library search path in starsim `Conscript` — cons does not automatically translate `#.arch/lib` to a `-L` flag
- Fix `bison`/`flex` tool selection in the `stic` build — AL9 has `bison` (providing yacc-compatible mode) and `flex` but not `libfl-devel`; resolved by adding `%option noyywrap` to the lexer source

---

## Current Blockers

### 1. CERNLIB — Recompilation Required
- All CERNLIB libraries are compiled for SL5.3 with GCC 4.3.2 (`sl53_x8664_gcc432`)
- AL9 provides only `libgfortran.so.5` (GCC 11); GCC 4.3.2's `libgfortran.so.3` is not available
- **Symptom:** `starsim` crashes immediately with `get_unit(): Bad internal unit KIND` (Fortran internal I/O ABI mismatch)
- **Blocks:** `starsim` runtime, all GEANT3-based simulation

### 2. ROOT — Recompilation Required
- ROOT 6.24.06 on CVMFS is built for `linux-rhel7-x86_64` with GCC 4.8.5
- The embedded `cling` interpreter (Clang 9.0.1) includes headers incompatible with AL9 glibc:
  - `xlocale.h` removed from glibc on RHEL8/9
  - `__BEGIN_NAMESPACE_C99` / `__END_NAMESPACE_STD` macros no longer defined
  - `max_align_t` typedef conflict
- **Symptom:** `rootcint` (ROOT dictionary generator) fails with fatal preprocessor errors
- **Blocks:** every package that generates a ROOT dictionary — `rexe`, `StTableUtilities`, and most of `StRoot`

### 3. `libStarMagFieldNoDict.so` — Not Yet Built
- `pams/geometry` links against `libStarMagFieldNoDict.so` (the dictionary-free build of `StarMagField`)
- The `NoDict` variant should not require `rootcint` and may be buildable today
- **Blocks:** final link step and shared library for `pams/geometry`

### 4. `libStDb_Tables.so` — Origin Unknown
- Several `pams` libraries depend on `libStDb_Tables.so`
- This library is not produced by any examined source directory and is not present in the AL9 central install tree
- Its source package has not yet been identified
- **Blocks:** `pams/tables` and all libraries depending on database table definitions

---

## Items Needed to Make Progress

1. **Rebuild CERNLIB** for AL9 / GCC 11 (`al96_x8664_gcc11`) — required before `starsim` can be used for simulation
2. **Rebuild ROOT** for AL9 / GCC 11 — required before the majority of `StRoot` packages can be compiled
3. **Identify and build `StarMagField` (`NoDict` variant)** — unblocks `pams/geometry` final link; likely buildable without ROOT today
4. **Identify the source package for `StDb_Tables`** — unblocks `pams/tables` and dependent libraries; may or may not require ROOT
5. **Coordinate with CVMFS/spack maintainers** — all current spack packages on CVMFS are `linux-rhel7-*`; AL9 builds of ROOT, CERNLIB, and other externals need to be provided there

---

## Summary

The AL9 build system infrastructure is working.  Pure Fortran and C/C++ code (without ROOT dictionaries) compiles and links cleanly under GCC 11 once the tool-selection and linker-path fixes are applied.  The two systemic blockers — CERNLIB and ROOT — require recompilation of external packages and cannot be resolved within the STAR source tree alone.
