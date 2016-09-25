# ----------------------------------------------------------------------
# This is the HepMC configuration input file
# Process this file with autoconf to produce a configure script.
# ----------------------------------------------------------------------

AC_PREREQ(2.59)
AC_INIT([HepMC],[2.06.09],[https://savannah.cern.ch/projects/hepmc/],[HepMC])
AC_CANONICAL_TARGET
AM_INIT_AUTOMAKE(1.9 foreign)
AC_CONFIG_SRCDIR([src/GenEvent.cc])
AC_CONFIG_HEADER([HepMC/defs.h])

# ----------------------------------------------------------------------
# shared library versioning
# for various reasons, this cannot match the HepMC version
# ----------------------------------------------------------------------

LIBRARY_VERSION="-version-info 4:0:0"
AC_SUBST(LIBRARY_VERSION)

# ----------------------------------------------------------------------
# define --enable-visual
# ----------------------------------------------------------------------
AC_ARG_ENABLE(visual,
   AC_HELP_STRING([--enable-visual],[on by default when using Visual C++]),
        [case "${enableval}" in
          yes) build_visual="yes";;
          no)  build_visual="no";;
          *)   build_visual="no";;
        esac],
        [build_visual="no"])

# ----------------------------------------------------------------------
# define the default momentum and length (position) units
# these MUST be declared
# ----------------------------------------------------------------------

AC_ARG_WITH(momentum,
   AC_HELP_STRING([--with-momentum],
                  [--with-momentum=MEV or GEV, no other values allowed]),
   [HEPMC_DEFAULT_MOM_UNIT=${withval}],
   [AC_MSG_ERROR([You MUST configure --with-momentum=XX, where XX is MEV or GEV])])
AC_SUBST(HEPMC_DEFAULT_MOM_UNIT)

AC_ARG_WITH(length,
   AC_HELP_STRING([--with-length],
                  [--with-length=MM or CM, no other values allowed]),
   [HEPMC_DEFAULT_LEN_UNIT=${withval}],
   [AC_MSG_ERROR([You MUST configure --with-length=YY, where YY is MM or CM])])
AC_SUBST(HEPMC_DEFAULT_LEN_UNIT)

# ----------------------------------------------------------------------
# find CLHEP and/or GENSER:
# these are used ONLY in the examples
# ----------------------------------------------------------------------

AC_ARG_WITH(CLHEP,
   AC_HELP_STRING([--with-CLHEP],[--with-CLHEP=$CLHEP_DIR: CLHEP is used in the examples]),
   [CLHEPdir=${withval}],[CLHEPdir=" "])
AC_SUBST(CLHEPdir)

AC_ARG_WITH(GENSER,
   AC_HELP_STRING([--with-GENSER],[--with-GENSER=$GENSER_DIR: GENSER is used in the examples]),
   [GENSERdir=${withval}],[GENSERdir=" "])
AC_SUBST(GENSERdir)

# ----------------------------------------------------------------------
# Checks for programs.
# ----------------------------------------------------------------------
case "$CXX" in
cl) 
  AC_MSG_ERROR([configure is not supported for Visual C++, use cmake instead])
  ;;
*)
esac

AC_PROG_CXX
AC_PROG_CC
AC_PROG_MAKE_SET
AC_PROG_F77(gfortran g77 f77)

# special libtool rules for Windows - work with gcc and cygwin
AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL

# ----------------------------------------------------------------------
# Checks for libraries.
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
# Checks for header files.
# ----------------------------------------------------------------------
AC_CHECK_HEADERS([stdint.h])

# ----------------------------------------------------------------------
# Checks for typedefs, structures, and compiler characteristics.
# ----------------------------------------------------------------------
AC_HEADER_STDBOOL
AC_C_CONST
AC_C_INLINE
AC_CHECK_TYPES([ptrdiff_t])

# ----------------------------------------------------------------------
# Checks for library functions.
# ----------------------------------------------------------------------
AC_HEADER_STDC

# ----------------------------------------------------------------------
# OS dependent differences:
# ----------------------------------------------------------------------

# copy
case "$target" in
*-*-win32*)
  COPY_P="copy -p"
  AC_MSG_WARN([we recommend using cmake instead of configure on Windows])
  ;;
*-*-darwin*)
  COPY_P="cp -p"
  AC_MSG_WARN([we recommend using cmake instead of configure on MacOSX])
  ;;
*)
  COPY_P="cp -p"
esac

# diff
DIFF_Q="diff -q -b"

AC_SUBST(COPY_P)
AC_SUBST(DIFF_Q)

# ----------------------------------------------------------------------
# compiler dependent differences
# notice that the default is presumed to be some flavor of g++
# ----------------------------------------------------------------------
case "$CXX" in
cl) 
  AM_CXXFLAGS="-EHsc -nologo -GR -MD"
  AC_SUBST(AM_MAKEFLAGS)
  CXXFLAGS=" "
  AC_SUBST(CXXFLAGS)
  build_visual="yes"
  AC_MSG_ERROR([configure is not supported for Visual C++, use cmake instead])
  ;;
*)  
  AM_CXXFLAGS="-ansi -pedantic -Wall"
esac

AC_SUBST(AM_CXXFLAGS)

AM_CONDITIONAL(BUILD_VISUAL, test x$build_visual = xyes)

# ----------------------------------------------------------------------
# process Makefile.in and other *.in files
# ----------------------------------------------------------------------
AC_CONFIG_FILES([Makefile
		 HepMC/Makefile
                 doc/Makefile
                 examples/Makefile
                 examples/fio/Makefile
                 examples/pythia8/Makefile
                 fio/Makefile
                 src/Makefile
                 src/Units.cc
                 test/Makefile
                 test/testHepMC.cc
                 test/testMass.cc
                 test/testHepMCIteration.cc
                 test/testMultipleCopies.cc
                 test/testStreamIO.cc
                 examples/GNUmakefile.example
                 examples/fio/GNUmakefile.example
                 examples/pythia8/config.csh
                 examples/pythia8/config.sh
                 examples/pythia8/GNUmakefile.example])

AC_CONFIG_FILES([test/testHepMC.sh], [chmod +x test/testHepMC.sh])
AC_CONFIG_FILES([test/testFlow.sh], [chmod +x test/testFlow.sh])
AC_CONFIG_FILES([test/testMass.sh], [chmod +x test/testMass.sh])
AC_CONFIG_FILES([test/testHepMCIteration.sh], [chmod +x test/testHepMCIteration.sh])
AC_CONFIG_FILES([test/testPolarization.sh], [chmod +x test/testPolarization.sh])
AC_CONFIG_FILES([test/testPrintBug.sh], [chmod +x test/testPrintBug.sh])
AC_CONFIG_FILES([test/testStreamIO.sh], [chmod +x test/testStreamIO.sh])

# ----------------------------------------------------------------------
# Finish up:
# ----------------------------------------------------------------------
AC_OUTPUT
