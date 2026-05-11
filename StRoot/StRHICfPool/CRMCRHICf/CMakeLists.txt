CMAKE_MINIMUM_REQUIRED (VERSION 3.5)
PROJECT (crmc)

include (FeatureSummary)

macro (CRMC_OPTION NAME TEXT DEFAULT)
  option ("${NAME}" "${TEXT}" "${DEFAULT}")
  add_feature_info ("${NAME}" "${NAME}" "${TEXT}")
endmacro ()

######################################ONLY EDIT THIS######################################

# Enable/Disable models to be built
CRMC_OPTION (CRMC_STATIC "Build with static library" OFF)   #if ON should not combined DPMJET/PHOJET/PYTHIA because they use different version of pythia (for dynamic library no problem)

OPTION (CRMC_PROG       "Build standalone CRMC program" ON) # standard CRMC installation (crmc program)
OPTION (CRMC_GEANT4     "Build with geant4 interface"   OFF)  # crmc-geant4 interface 
OPTION (CRMC_GEANT4PHYS "Build with geant4 interface"   OFF)  # crmc geant4 physics list (requires geant4 to be installed)

CRMC_OPTION (CRMC_ENABLE_ROOT "Build with ROOT support" ON)   # not encouraged (switch off HepMC2) ! Please use HepMC

CRMC_OPTION (CRMC_ENABLE_HepMC3 "Build with HepMC3 support" ON)   # Recommended
CRMC_OPTION (CRMC_ENABLE_Rivet "Build with Rivet support" OFF)   # requires HepMC3 and switch off HepMC2

CRMC_OPTION (CRMC_EXT_RNDM   "Build with external random generator"        OFF)
CRMC_OPTION (CRMC_QGSJET01   "Build with QGSJet 2003"                      OFF)
CRMC_OPTION (CRMC_GHEISHA    "Build with GHeisha 2002"                     OFF)
CRMC_OPTION (CRMC_PYTHIA     "Build with Pythia 6.4.28  (crash)"           OFF)
CRMC_OPTION (CRMC_HIJING     "Build with Hijing 1.383"                     OFF)
CRMC_OPTION (CRMC_SIBYLL     "Build with Sibyll 2.3e"                      ON)
CRMC_OPTION (CRMC_PHOJET     "Build with Phojet 1.12"                      OFF)
CRMC_OPTION (CRMC_DPMJET06   "Build with DPMJet 3.0-6 (not recommended)"   OFF)
CRMC_OPTION (CRMC_DPMJET17   "Build with DPMJet 3.2017 (only one version)" OFF)
CRMC_OPTION (CRMC_DPMJET19   "Build with DPMJet 3.2019 (only one version)" OFF)
CRMC_OPTION (CRMC_QGSJETII03 "Build with QGSJet II-03"                     OFF)
CRMC_OPTION (CRMC_QGSJETII04 "Build with QGSJet-II-04"                     ON)
CRMC_OPTION (CRMC_QGSJETIII  "Build with QGSJet-III (big (1.4GB) data)"    ON)

SET (HepMC_HEPEVT_SIZE 10000 CACHE STRING "Set size of HEPEVT common block")  #
######################################ONLY EDIT THIS######################################


if (NOT HepMC_HEPEVT_SIZE MATCHES "^[1-9][0-9]+$")
  MESSAGE(FATAL_ERROR "HEPEVT size must be a number")
else ()
  MESSAGE("HEPEVT size set to ${HepMC_HEPEVT_SIZE}")
  if (HepMC_HEPEVT_SIZE LESS 10000)
    MESSAGE(WARNING "HEPEVT size is small - consider enlarging it")
  elseif (HepMC_HEPEVT_SIZE GREATER 1000000)
    MESSAGE(WARNING "HEPEVT size is very large - consider reducing it")
  endif()
endif ()

if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set (CMAKE_INSTALL_PREFIX $ENV{PWD} CACHE PATH "default install path" FORCE )
endif()

MESSAGE (STATUS "source  ${CMAKE_SOURCE_DIR}")
MESSAGE (STATUS "build   ${CMAKE_BINARY_DIR}")
MESSAGE (STATUS "install ${CMAKE_INSTALL_PREFIX}")

## The version number. Only nummeric PLEASE!
SET (CRMC_VERSION_MAJOR 2)
SET (CRMC_VERSION_MINOR 2)
SET (CRMC_VERSION_PATCH 0)

##Compiler options
# hotfix for lxbatch not sure why he selects wrong compiler
#SET (CMAKE_Fortran_COMPILER gfortran)
#SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -lgfortran")

# comprehensible compiler languages (default c cxx)
ENABLE_LANGUAGE (Fortran)

INCLUDE (CheckCXXCompilerFlag)
INCLUDE (CheckFortranCompilerFlag)

message("CXXFLAGS: ${CMAKE_CXX_FLAGS}")
message("FFLAGS: ${CMAKE_FORTRAN_FLAGS}")
#add some warning to the compiler flags
FOREACH(lang C CXX Fortran)
  IF(CMAKE_${lang}_COMPILER_ID STREQUAL GNU)
    ADD_COMPILE_OPTIONS(-Wunused -Wuninitialized -O0)
  ENDIF()
ENDFOREACH()

## RPATH (where to find library)
# use, i.e. don't skip the full RPATH for the build tree
SET(CMAKE_SKIP_BUILD_RPATH  FALSE)

# when building, don't use the install RPATH already
# (but later on when installing)
SET(CMAKE_BUILD_WITH_INSTALL_RPATH TRUE)

SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
# add the automatically determined parts of the RPATH
# which point to directories outside the build tree to the install RPATH
SET(CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE)

# the RPATH to be used when installing, but only if it's not a system directory
LIST(FIND CMAKE_PLATFORM_IMPLICIT_LINK_DIRECTORIES
  "${CMAKE_INSTALL_PREFIX}/lib" isSystemDir)
IF("${isSystemDir}" STREQUAL "-1")
  SET(CMAKE_INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
ENDIF("${isSystemDir}" STREQUAL "-1")

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  ADD_COMPILE_OPTIONS("$<$<COMPILE_LANGUAGE:Fortran>:-std=legacy>")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  ADD_COMPILE_OPTIONS("$<$<COMPILE_LANGUAGE:Fortran>:-std90>")
endif()

# Add code model flags in case we statically link - otherwise, it may fail. 
IF(CRMC_STATIC)
  CHECK_CXX_COMPILER_FLAG("-mcmodel=large" COMPILER_CXX_MCMODEL)
  IF (COMPILER_CXX_MCMODEL)
    MESSAGE("Adding large code model flag for C++")
    ADD_COMPILE_OPTIONS("$<$<COMPILE_LANGUAGE:CXX>:-mcmodel=large>")
  ENDIF(COMPILER_CXX_MCMODEL)

  CHECK_Fortran_COMPILER_FLAG("-mcmodel=large" COMPILER_FORTRAN_MCMODEL)
  IF (COMPILER_FORTRAN_MCMODEL)
    MESSAGE("Adding large code model flag for Fortran")
    ADD_COMPILE_OPTIONS("$<$<COMPILE_LANGUAGE:Fortran>:-mcmodel=large>")
  ENDIF(COMPILER_FORTRAN_MCMODEL)

  # check if both PhoJet and DPMJet are enabled - cannot be -
  # conflicting definition of PHO_RNDM sub-routine
  IF (CRMC_PHOJET AND (CRMC_DPMJET06 OR CRMC_DPMJET17))
    MESSAGE(FATAL_ERROR "PhoJet and DPMJet 3.0-6 or 3.2017-1 cannot be enabled at the same time")
  ENDIF(CRMC_PHOJET AND (CRMC_DPMJET06 OR CRMC_DPMJET17))
  
ENDIF(CRMC_STATIC)

# Remove some warnings from fortran compilations
SET(FORTRAN_NO_WARNINGS
  "-Wno-maybe-uninitialized"
  "-Wno-unused-variable"
  "-Wno-unused-label")
FOREACH(thewarn ${FORTRAN_NO_WARNINGS})
  CHECK_Fortran_COMPILER_FLAG(${thewarn} FORTRAN_NOWARN_FLAG)
  IF (FORTRAN_NOWARN_FLAG)
    ADD_COMPILE_OPTIONS("$<$<COMPILE_LANGUAGE:Fortran>:${thewarn}>")
  ENDIF (FORTRAN_NOWARN_FLAG)
ENDFOREACH()


## find packages
SET (CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/CMakeModules"
  CACHE PATH "Module Path" FORCE)

SET (CRMC_SOURCES
  src/CRMC.cc
  src/CRMCinterface.cc
  src/CRMCoptions.cc
  src/OutputPolicyLHE.cc
  src/OutputPolicyNone.cc
  src/CRMCtimer.c
  src/CRMCtrapfpe.c)
SET (CRMC_HEADERS
  src/CRMC.h
  src/CRMCstat.h
  src/OutputPolicyNone.h
  src/OutputPolicyLHE.h
  src/CRMCoptions.h
  ${CMAKE_BINARY_DIR}/src/CRMCinterface.h)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)


if (CRMC_ENABLE_ROOT)
  FIND_PACKAGE (Root)

  if (Root_FOUND)
    INCLUDE_DIRECTORIES ("${ROOT_INCLUDE_DIR}")
    LIST(APPEND CRMC_SOURCES src/OutputPolicyROOT.cc)
    LIST(APPEND CRMC_HEADERS src/OutputPolicyROOT.h)
    string(REPLACE "-m64" "" ROOT_CPPFLAGS "${ROOT_CPPFLAGS}")
    string(REPLACE "-pthread" "" ROOT_CPPFLAGS "${ROOT_CPPFLAGS}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${ROOT_CPPFLAGS}")

    set_property(SOURCE src/CRMC.cc src/CRMCoptions.cc src/crmcMain.cc
      APPEND PROPERTY COMPILE_DEFINITIONS WITH_ROOT)
    include(CheckCXXCompilerFlag)
    CHECK_CXX_COMPILER_FLAG("-std=c++17" COMPILER_SUPPORTS_CXX17)
    CHECK_CXX_COMPILER_FLAG("-std=c++14" COMPILER_SUPPORTS_CXX14)
    CHECK_CXX_COMPILER_FLAG("-std=c++11" COMPILER_SUPPORTS_CXX11)
    CHECK_CXX_COMPILER_FLAG("-std=c++0x" COMPILER_SUPPORTS_CXX0X)
    if(COMPILER_SUPPORTS_CXX17)
      set(CMAKE_CXX_STANDARD 17)
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++17")
    elseif(COMPILER_SUPPORTS_CXX14)
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
    elseif(COMPILER_SUPPORTS_CXX11)
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
    elseif(COMPILER_SUPPORTS_CXX0X)
      set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++0x")
    else()
      message(STATUS "The compiler ${CMAKE_CXX_COMPILER} has no proper C++ support for ROOT. Please consider updating your C++ compiler.")
    endif()
    MESSAGE("Build ROOT Output Interface")
  else (Root_FOUND)
    MESSAGE("Cannot Build ROOT Output Interface")
  endif (Root_FOUND)
endif (CRMC_ENABLE_ROOT)


# SET(Boost_DEBUG TRUE)
FIND_PACKAGE (Boost 1.35 REQUIRED
  COMPONENTS filesystem iostreams system program_options)

IF (CRMC_ENABLE_HepMC3)
FIND_PACKAGE (HepMC3)
if (HepMC3_FOUND)
  INCLUDE_DIRECTORIES ("${HEPMC3_INCLUDE_DIR}")
  LIST(APPEND CRMC_SOURCES src/OutputPolicyHepMC3.cc)
  LIST(APPEND CRMC_HEADERS
    src/OutputPolicyHepMC3.h
    src/CRMChepevt.h
    src/CRMChepmc3.h)
  set_property(SOURCE src/CRMC.cc src/CRMCoptions.cc src/crmcMain.cc
    APPEND PROPERTY COMPILE_DEFINITIONS WITH_HEPMC3)
  MESSAGE("Build HepMC3 Output Interface: ${HepMC3_DIR}")

  IF (CRMC_ENABLE_Rivet)
   FIND_PACKAGE(Rivet)
   if (Rivet_FOUND)
    set(CMAKE_CXX_STANDARD 17)
    INCLUDE_DIRECTORIES ("${RIVET_INCLUDE_DIR}")
    LIST(APPEND CRMC_SOURCES src/OutputPolicyRivet.cc)
    LIST(APPEND CRMC_HEADERS src/OutputPolicyRivet.h)
    set_property(SOURCE src/CRMC.cc src/CRMCoptions.cc src/crmcMain.cc
      APPEND PROPERTY COMPILE_DEFINITIONS WITH_RIVET)
    MESSAGE("Build Rivet interface: ${Rivet_DIR}")
   endif (Rivet_FOUND)
  ENDIF (CRMC_ENABLE_Rivet)
endif (HepMC3_FOUND)
ENDIF (CRMC_ENABLE_HepMC3)

IF ( Rivet_FOUND )
  CRMC_OPTION (CRMC_ENABLE_HepMC "Build with HepMC2 support" OFF)   # do not compile with HepMC2 together with ROOT or RIVET (because of C++14 vs C++17)
ELSE ( Rivet_FOUND )
if ( Root_FOUND )
  CRMC_OPTION (CRMC_ENABLE_HepMC "Build with HepMC2 support" OFF)   # do not compile with HepMC2 together with ROOT or RIVET (because of C++14 vs C++17)
else ( Root_FOUND )
  CRMC_OPTION (CRMC_ENABLE_HepMC "Build with HepMC2 support" ON)   # default HepMC2 if found
endif ( Root_FOUND )
ENDIF ( Rivet_FOUND )

IF (CRMC_ENABLE_HepMC)
FIND_PACKAGE (HepMC COMPONENTS HepMC HepMCfio)
if (HEPMC_FOUND)
  INCLUDE_DIRECTORIES ("${HepMC_INCLUDE_DIRS}")
  LIST(APPEND CRMC_SOURCES src/OutputPolicyHepMC.cc)
  LIST(APPEND CRMC_HEADERS src/OutputPolicyHepMC.h)
  set_property(SOURCE src/CRMC.cc src/CRMCoptions.cc src/crmcMain.cc
    APPEND PROPERTY COMPILE_DEFINITIONS WITH_HEPMC)
  MESSAGE("Build HEPMC Output Interface")
endif (HEPMC_FOUND)
ENDIF (CRMC_ENABLE_HepMC)



FIND_PACKAGE (Fastjet COMPONENTS fastjet)
if (Fastjet_FOUND)
  include_directories("${Fastjet_INCLUDE_DIRS}")
  MESSAGE("Include fastjet filter: ${Fastjet_INCLUDE_DIRS}")
endif (Fastjet_FOUND)


# prepare crmc to use same size HEPMC common block than HepMC was compiled with
configure_file(src/epos.inc-HEPMC-TEMPLATE src/epos.inc)
configure_file(src/CRMCinterface.h-HEPMC-TEMPLATE 
  src/CRMCinterface.h)
configure_file(src/pythia/pythia6115.f-HEPMC-TEMPLATE 
  src/pythia/pythia6115.f)
configure_file(src/pythia/pythia6215.f-HEPMC-TEMPLATE 
  src/pythia/pythia6215.f)
configure_file(src/pythia/pythia-6.4.28.f-HEPMC-TEMPLATE 
  src/pythia/pythia-6.4.28.f)
configure_file(src/hijing/hipyset1.35.f-HEPMC-TEMPLATE 
  src/hijing/hipyset1.35.f)
configure_file(src/dpmjet/3.2017-1/pythia-6427_cors.f-HEPMCTEMPLATE 
  src/dpmjet/3.2017-1/pythia-6427_cors.f)
configure_file(src/dpmjet/3.2017-1/phojet.f-HEPMCTEMPLATE 
  src/dpmjet/3.2017-1/phojet.f)
configure_file(src/dpmjet/3.2017-1/dpmjet.f-HEPMCTEMPLATE 
  src/dpmjet/3.2017-1/dpmjet.f)
configure_file(src/dpmjet/3.2019-1/hepevt-HEPMCTEMPLATE
  src/dpmjet/3.2019-1/DPMJET-19.1/include/pythia/inc/hepevt)
configure_file(src/dpmjet/3.0-6/pythia6115dpm3v1.f-HEPMCTEMPLATE 
    src/dpmjet/3.0-6/pythia6115dpm3v1.f)

## configure a header file to pass some of the CMake settings to the source code
CONFIGURE_FILE (
  "${CMAKE_SOURCE_DIR}/src/CRMCconfig.h.in"
  "${CMAKE_BINARY_DIR}/src/CRMCconfig.h"
  )
INSTALL (FILES ${CMAKE_BINARY_DIR}/src/CRMCconfig.h DESTINATION include/crmc)

INCLUDE_DIRECTORIES ("${CMAKE_SOURCE_DIR}/src")
INCLUDE_DIRECTORIES ("${CMAKE_BINARY_DIR}/src")
INCLUDE_DIRECTORIES ("${CMAKE_SOURCE_DIR}/externals/tclap-1.2.4/include")
#INCLUDE_DIRECTORIES ("${CMAKE_SOURCE_DIR}/src/readLib/externals/compress_stream/include")
#INCLUDE_DIRECTORIES ("${CMAKE_SOURCE_DIR}/src/readLib/")

# Set basic model source files
FILE(GLOB base_files
  ${CMAKE_SOURCE_DIR}/src/crmc-aaa.f
  ${CMAKE_SOURCE_DIR}/src/models.F)

IF (NOT CRMC_EXT_RNDM)
  LIST(APPEND base_files ${CMAKE_SOURCE_DIR}/src/crmc-rndm.f)
ENDIF (NOT CRMC_EXT_RNDM)

# The base and EPOS library used by all EGs (contain EPOS)
ADD_SUBDIRECTORY (src/epos)
  

# Dummy source which includes epos.inc file so HEPMC will find the
# symbol for hepevt_ with fPIC linker option it should always point to
# right symbol also for other libs
IF (NOT CRMC_STATIC)
  LIST(APPEND CRMC_SOURCES src/dummy.f)
ENDIF (NOT CRMC_STATIC)

# The interface library
IF (CRMC_STATIC)
  add_library(Crmc STATIC ${CRMC_SOURCES}  ${base_files})
ELSE (CRMC_STATIC)
  add_library(Crmc SHARED ${CRMC_SOURCES})
ENDIF (CRMC_STATIC)
INSTALL (TARGETS Crmc
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib/static)

if (HEPMC_FOUND)
  TARGET_LINK_LIBRARIES (Crmc ${HepMC_LIBRARIES})
endif(HEPMC_FOUND)
if (HepMC3_FOUND)
  TARGET_LINK_LIBRARIES (Crmc ${HEPMC3_LIBRARIES})
  if (Rivet_FOUND)
    STRING (STRIP "${RIVET_LIB_DIR}" RIVET_LIBRARY_DIR)
    TARGET_LINK_LIBRARIES (Crmc ${RIVET_LIBRARY_DIR}/libRivet.so) 
    TARGET_COMPILE_OPTIONS (Crmc PUBLIC ${RIVET_CXXFLAGS})
  endif(Rivet_FOUND)
endif(HepMC3_FOUND)
TARGET_LINK_LIBRARIES (Crmc ${Boost_LIBRARIES})
if (Root_FOUND)
  TARGET_LINK_LIBRARIES (Crmc ${ROOT_LIBRARIES})
endif (Root_FOUND)
# IF (CRMC_STATIC)
# ELSE (CRMC_STATIC)
#   TARGET_LINK_LIBRARIES (Crmc HepEvtDummy)
# ENDIF (CRMC_STATIC)
  
SET (TABS "")
SET (STATIC_LIBS "")
IF (CRMC_STATIC)
ELSE (CRMC_STATIC)
  LIST(APPEND STATIC_LIBS Epos)
ENDIF(CRMC_STATIC)

# data tables for epos
LIST(APPEND TABS epos.inics)
LIST(APPEND TABS epos.inics.nf)
LIST(APPEND TABS epos.inidi)
LIST(APPEND TABS epos.iniev)
LIST(APPEND TABS epos.inirj)
LIST(APPEND TABS epos.inirj.nf)
LIST(APPEND TABS epos.initl)
LIST(APPEND TABS idt.dt)
LIST(APPEND TABS idresi.dt)
LIST(APPEND TABS idky5.dt)
LIST(APPEND TABS tables.dat)

# Check if more than one DPMJet is enabled
MATH(EXPR dpmjet_count "0")
IF (CRMC_DPMJET06)
  MATH(EXPR dpmjet_count "${dpmjet_count}+1")
ENDIF(CRMC_DPMJET06)
IF (CRMC_DPMJET17)
  MATH(EXPR dpmjet_count "${dpmjet_count}+1")
ENDIF(CRMC_DPMJET17)
IF (CRMC_DPMJET19)
  MATH(EXPR dpmjet_count "${dpmjet_count}+1")
ENDIF(CRMC_DPMJET19)
IF (${dpmjet_count} GREATER 1)
  MESSAGE(FATAL_ERROR "More than one DPMJet version requested - select one")
ENDIF()

IF (CRMC_QGSJET01)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/qgsjet")
  LIST(APPEND TABS qgsjet.dat)
  LIST(APPEND TABS qgsjet.ncs)
  LIST(APPEND STATIC_LIBS Qgsjet01)
  set_property(SOURCE src/models.F APPEND PROPERTY COMPILE_DEFINITIONS __QGSJET01__)
  MESSAGE("QGSJet 2001 enabled")
ENDIF (CRMC_QGSJET01)

IF (CRMC_GHEISHA)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/gheisha")
  LIST(APPEND STATIC_LIBS Gheisha)
  set_property(SOURCE src/models.F APPEND PROPERTY COMPILE_DEFINITIONS __GHEISHA__)
  MESSAGE("GHeisha enabled")
ENDIF (CRMC_GHEISHA)

IF (CRMC_PYTHIA)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/pythia")
  LIST(APPEND STATIC_LIBS Pythia)
  set_property(SOURCE src/models.F APPEND PROPERTY COMPILE_DEFINITIONS __PYTHIA__)
  MESSAGE("Pythia enabled")
ENDIF (CRMC_PYTHIA)

IF (CRMC_HIJING)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/hijing")
  LIST(APPEND STATIC_LIBS Hijing)
  set_property(SOURCE src/models.F APPEND PROPERTY COMPILE_DEFINITIONS __HIJING__)
  MESSAGE("Hijing enabled")
ENDIF (CRMC_HIJING)

IF (CRMC_SIBYLL)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/sibyll")
  LIST(APPEND STATIC_LIBS Sibyll)
  set_property(SOURCE src/models.F APPEND PROPERTY COMPILE_DEFINITIONS __SIBYLL__)
  MESSAGE("Sibyll enabled")
ENDIF (CRMC_SIBYLL)

IF (CRMC_PHOJET)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/phojet")
  LIST(APPEND STATIC_LIBS Phojet)
  set_property(SOURCE src/models.F APPEND PROPERTY COMPILE_DEFINITIONS __PHOJET__)
  MESSAGE("PhoJet enabled")
ENDIF (CRMC_PHOJET)

IF (CRMC_DPMJET19)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/dpmjet/3.2019-1")
  LIST(APPEND TABS dpmjpar.dat_higher_diff)
  LIST(APPEND TABS dpmjpar.dat)
  LIST(APPEND TABS CT14LL.pds)
  LIST(APPEND TABS dpmjet.dat)
  LIST(APPEND TABS conextar.glb)
  LIST(APPEND STATIC_LIBS Dpmjet19)
  set_property(SOURCE src/models.F
    APPEND PROPERTY COMPILE_DEFINITIONS __DPMJET__ __DPMJET19__)
  unset(CRMC_DPMJET17)
  unset(CRMC_DPMJET06)
  MESSAGE("DPMJet 3-2019 enabled")
ELSEIF (CRMC_DPMJET17)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/dpmjet/3.2017-1")
  LIST(APPEND TABS dpmjet.dat)
  LIST(APPEND TABS dpmjpar17.dat)
  LIST(APPEND TABS dpmCT14LL.pds)
  LIST(APPEND TABS conextar.glb)
  LIST(APPEND STATIC_LIBS Dpmjet17)
  set_property(SOURCE src/models.F
    APPEND PROPERTY COMPILE_DEFINITIONS __DPMJET__ __DPMJET17__)
  unset(CRMC_DPMJET19)
  unset(CRMC_DPMJET06)
  MESSAGE("DPMJet 3-2017 enabled")
ELSEIF (CRMC_DPMJET06)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/dpmjet/3.0-6")
  LIST(APPEND TABS dpmjet.dat)
  LIST(APPEND TABS phojet_fitpar.dat)
  LIST(APPEND STATIC_LIBS Dpmjet06)
  set_property(SOURCE src/models.F
    APPEND PROPERTY COMPILE_DEFINITIONS __DPMJET__ __DPMJET06__)
  unset(CRMC_DPMJET17)
  unset(CRMC_DPMJET06)
  MESSAGE("DPMJet 3.06 enabled")
ENDIF (CRMC_DPMJET19)



#put definition outside "if" because it is always in crmc.param file
SET (QGSJETII_TABS "03")
IF (CRMC_QGSJETII03)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/qgsjetII")
  LIST(APPEND TABS qgsdat-II-03.lzma)
  LIST(APPEND TABS sectnu-II-03)
  LIST(APPEND STATIC_LIBS QgsjetII03)
  set_property(SOURCE src/models.F
     APPEND PROPERTY COMPILE_DEFINITIONS __QGSJETII03__)
  MESSAGE("QGSJet=II-03 enabled")
ENDIF (CRMC_QGSJETII03)

IF (CRMC_QGSJETII04)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/qgsjetII-04")
  LIST(APPEND TABS qgsdat-II-04.lzma)
  LIST(APPEND TABS sectnu-II-04)
  LIST(APPEND STATIC_LIBS QgsjetII04)
  SET (QGSJETII_TABS "04")
  set_property(SOURCE src/models.F
     APPEND PROPERTY COMPILE_DEFINITIONS __QGSJETII04__)
  MESSAGE("QGSJet=II-04 enabled")
ENDIF (CRMC_QGSJETII04)

IF (CRMC_QGSJETIII)
  ADD_SUBDIRECTORY ("${CMAKE_SOURCE_DIR}/src/qgsjetIII")
  LIST(APPEND TABS qgsdat-III.lzma)
  LIST(APPEND TABS sectnu-III)
  LIST(APPEND STATIC_LIBS QgsjetIII)
#  SET (QGSJETIII_TABS "")
  set_property(SOURCE src/models.F
     APPEND PROPERTY COMPILE_DEFINITIONS __QGSJETIII__)
  MESSAGE("QGSJet=III enabled")
ENDIF (CRMC_QGSJETIII)

get_property(HELPER SOURCE src/models.F PROPERTY COMPILE_DEFINITIONS)
MESSAGE("Flags for ${CMAKE_SOURCE_DIR}/src/models.F: ${HELPER}")
get_property(HELPER SOURCE ${CMAKE_SOURCE_DIR}/src/CRMCoptions.cc
  PROPERTY COMPILE_DEFINITIONS)
MESSAGE("Flags for ${CMAKE_SOURCE_DIR}/src/CRMCoptions.cc: ${HELPER}")

#include directories for headers
INCLUDE_DIRECTORIES ("${CMAKE_BINARY_DIR}/src")
INCLUDE_DIRECTORIES ("${Boost_INCLUDE_DIRS}")

if (CRMC_GEANT4 OR CRMC_GEANT4PHYS)
  # require Geant4
  # set (Geant4_CONFIG_DEBUG ON)
  find_package (Geant4 REQUIRED CONFIG)
  if (NOT Geant4_FOUND)
    message (error "Geant4 not found. Provide Geant4_DIR to find it, or add geant4-config to your PATH.")
  endif (NOT Geant4_FOUND)
  message ("Geant4 found: ${Geant4_VERSION}")
endif (CRMC_GEANT4 OR CRMC_GEANT4PHYS)

# ========= USED IN GEANT4 interface =========
if (CRMC_GEANT4)  
  ADD_LIBRARY (Crmc SHARED src/CRMCinterface.cc)
  INSTALL (TARGETS Crmc
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib/static
    )
endif (CRMC_GEANT4)
# ========= USED IN GEANT4 interface =========

IF (CRMC_GEANT4PHYS)
  ADD_SUBDIRECTORY ("${PROJECT_SOURCE_DIR}/src/geant4")
ENDIF (CRMC_GEANT4PHYS)




# the executable
IF (CRMC_PROG)
  ADD_EXECUTABLE(crmc   src/crmcMain.cc)
  
  # linking of modules
  TARGET_LINK_LIBRARIES (crmc Crmc)
  IF (CRMC_STATIC)
    TARGET_LINK_LIBRARIES (crmc ${STATIC_LIBS} CrmcBasic)
  ENDIF(CRMC_STATIC)
  TARGET_LINK_LIBRARIES (crmc ${CMAKE_DL_LIBS})
    
  INSTALL (TARGETS crmc RUNTIME DESTINATION bin)
ENDIF (CRMC_PROG)

## installation
# configure the parameter file
#SET(CRMC_TABDIR "${CMAKE_SOURCE_DIR}/tabs")
SET(CRMC_TABDIR "${CMAKE_INSTALL_PREFIX}/share/crmc")
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/src/crmc.param.in
  ${CMAKE_BINARY_DIR}/crmc.param @ONLY IMMEDIATE)
#SET(CRMC_TABDIR "${CMAKE_INSTALL_PREFIX}/share/crmc")
CONFIGURE_FILE(${CMAKE_SOURCE_DIR}/src/crmc.param.in
  ${CMAKE_BINARY_DIR}/crmc.param.install @ONLY IMMEDIATE)
install(FILES ${CMAKE_BINARY_DIR}/crmc.param.install DESTINATION etc
  RENAME crmc.param) 

# install tabs
SET (TABSDIR "")
foreach(theTab ${TABS})
  list(APPEND TABSDIR ${CMAKE_SOURCE_DIR}/tabs/${theTab} )
endforeach()
INSTALL (FILES ${TABSDIR} DESTINATION share/crmc/)
IF (CRMC_QGSJETIII)
  INSTALL (CODE "MESSAGE (\"unpack share/crmc/qgsdat-III.lzma\")")
  INSTALL (CODE "EXECUTE_PROCESS(COMMAND unlzma share/crmc/qgsdat-III.lzma)")
ENDIF (CRMC_QGSJETIII)

# install tables
INSTALL (FILES ${CMAKE_BINARY_DIR}/src/epos.inc
  src/ak.h
  src/epos.incems
  src/epos.inchy
  src/epos.incho
  src/epos.incico
  src/epos.incpar
  src/epos.incsem
  src/epos.incxan
  DESTINATION include/crmc)
INSTALL (FILES ${CRMC_HEADERS} DESTINATION include/crmc)

# enable dashboard scripting
include (CTest)
#ADD_SUBDIRECTORY (ExampleAnalyser)

#ADD_TEST (crmcRuns bin/crmc -o hepmc -f ctest.hepmc -T 1)
# ADD_TEST (crmcCheckFile md5sum "ctest.hepmc")
# SET_TESTS_PROPERTIES (crmcCheckFile PROPERTIES
#   PASS_REGULAR_EXPRESSION ".*777cb7e60b7c8104393a4fb22b137538  ctest.hepmc")
#ADD_TEST (crmcRemoveFile rm "ctest.hepmc")

#+++++++++++++++++++++++++++++++
#
# final summary output
#
feature_summary (WHAT ALL)
