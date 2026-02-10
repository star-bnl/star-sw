# Fortran 90 Compiler Support

This document describes the Fortran 90 compiler support added to the cons build system.

## Overview

The cons build system in the `mgr/` directory now supports compilation and linking of Fortran 90 source files (.f90 and .F90). This extends the existing Fortran 77 support (.f and .F).

## Compiler Configuration

The Fortran 90 compiler uses the same compiler binary as the Fortran 77 compiler (typically `gfortran`), which supports both Fortran 77 and Fortran 90 standards.

### Build Variables

The following build variables have been added to `ConsDefs.pm`:

- **F90**: The Fortran 90 compiler command (defaults to the value of FC, typically gfortran)
- **F90FLAGS**: Compiler flags for Fortran 90 compilation
- **F90DEBUG**: Debug flags for Fortran 90 (defaults to FDEBUG value)
- **F90PATH**: Include paths for Fortran 90 modules
- **EXTRA_F90PATH**: Additional include paths for Fortran 90 modules
- **F90PPFLAGS**: Preprocessor flags for Fortran 90
- **EXTRA_F90PPFLAGS**: Additional preprocessor flags for Fortran 90
- **F90COM**: The complete compilation command for Fortran 90 files

### Default Compiler Flags

The default F90FLAGS are compatible with Fortran 77 flags and include:
- `-fd-lines-as-comments` or `-fd-lines-as-code`: Handle debug lines (depending on NODEBUG setting)
- `-fno-second-underscore`: Disable second underscore for symbol names
- `-fno-automatic`: Variables are static by default
- `-Wall -W -Wsurprising`: Enable comprehensive warnings
- `-fPIC`: Generate position-independent code for shared libraries

Note: The `-std=legacy` flag used for Fortran 77 is intentionally excluded as it's F77-specific.

## File Extensions

The build system now recognizes the following Fortran 90 file extensions:

- **.f90**: Free-form Fortran 90 source files
- **.F90**: Free-form Fortran 90 source files with preprocessing

Both extensions are mapped to the `build::command::f90` builder in the cons system.

## Usage

To use Fortran 90 in your project:

1. Create your Fortran 90 source files with `.f90` or `.F90` extensions in your source directory
2. The cons build system will automatically discover these files when scanning the directory
3. The build system will automatically use the Fortran 90 compiler for these files

**Note**: The build system uses the `find_sources` function to automatically discover source files. The following Fortran file extensions are recognized for automatic discovery:
- `.F` - Fortran 77 fixed-form with preprocessing
- `.f90` - Fortran 90 free-form
- `.F90` - Fortran 90 free-form with preprocessing

**Important**: `.f` files (Fortran 77 fixed-form without preprocessing) use a separate preprocessing compilation mechanism and are not included in automatic source file discovery. They are handled through a different build path.

Example directory structure:
```
MyPackage/
├── mycode.cxx
├── myfortran.F        # Fortran 77 with preprocessing (auto-discovered)
├── myfortran90.f90    # Fortran 90 (auto-discovered)
└── preprocessed.F90   # Fortran 90 with preprocessing (auto-discovered)
```

The build system will automatically find all these files and compile them appropriately.

## Differences from Fortran 77

The key differences in how Fortran 90 files are handled:

1. **Free-form source**: Fortran 90 uses free-form source format by default (no column restrictions)
2. **Module support**: The compiler will generate .mod files for Fortran 90 modules
3. **Include paths**: Use F90PATH instead of FCPATH for Fortran 90-specific include directories

## Notes

- The same linker and libraries are used for both Fortran 77 and Fortran 90 code
- Fortran 90 modules can be used from Fortran 77 code and vice versa
- The gfortran compiler supports mixing Fortran 77 and Fortran 90 code in the same project
