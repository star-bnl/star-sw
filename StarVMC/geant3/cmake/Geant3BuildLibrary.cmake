#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2014 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# CMake Configuration file for geant4_vmc
# I. Hrivnacova, 13/06/2014

#---CMake required version -----------------------------------------------------
cmake_minimum_required(VERSION 2.8.12 FATAL_ERROR)

#-------------------------------------------------------------------------------
# Define installed names
#
set(library_name geant321)

#-------------------------------------------------------------------------------
# Includes
#
include_directories(${ROOT_INCLUDE_DIRS})
include_directories(${VMC_INCLUDE_DIRS})

#-------------------------------------------------------------------------------
# Setup project include directories; compile definitions; link libraries
#
include_directories(
  ${PROJECT_SOURCE_DIR}
  ${PROJECT_SOURCE_DIR}/minicern
  ${PROJECT_SOURCE_DIR}/TGeant3
  ${CMAKE_CURRENT_BINARY_DIR})

#-------------------------------------------------------------------------------
# Generate Root dictionaries
#
ROOT_GENERATE_DICTIONARY(
  ${library_name}_dict
  TCallf77.h
  TG3Application.h
  TGeant3f77.h
  TGeant3gu.h
  TGeant3.h
  TGeant3TGeo.h
  MODULE ${library_name}
  OPTIONS "-I${CMAKE_INSTALL_PREFIX}/include/TGeant3"
    -excludePath "${CMAKE_CURRENT_BINARY_DIR}"
    -excludePath "${PROJECT_SOURCE_DIR}/TGeant3"
  LINKDEF TGeant3/geant3LinkDef.h)

# Files produced by the dictionary generation
SET(root_dict
  ${library_name}_dict.cxx)
SET(root_dict_libs
  ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_SHARED_LIBRARY_PREFIX}${library_name}_rdict.pcm
  ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_SHARED_LIBRARY_PREFIX}${library_name}.rootmap)

#-------------------------------------------------------------------------------
# Always use '@rpath' in install names of libraries.
#
set(CMAKE_MACOSX_RPATH 1)

#-------------------------------------------------------------------------------
# Locate sources for this project
#
set(directories
    added gbase gcons geocad ggeom gheisha ghits ghrout ghutils giface giopa
    gkine gparal gphys gscan gstrag gtrak matx55 miface miguti neutron peanut
    fiface cgpack fluka block comad erdecks erpremc minicern gdraw)

if(BUILD_GCALOR)
  list(APPEND directories gcalor)
endif()

# Fortran sources
set(fortran_sources gcinit.F)
foreach(_directory ${directories})
  file(GLOB add_f_sources
       ${PROJECT_SOURCE_DIR}/${_directory}/*.F)
  list(APPEND fortran_sources ${add_f_sources})
endforeach()
list(APPEND fortran_sources ${PROJECT_SOURCE_DIR}/minicern/lnxgs/rdmin.F)
if(BUILD_GCALOR)
  # special compiler flags for gcalor
  set_source_files_properties(${PROJECT_SOURCE_DIR}/gcalor/gcalor.F PROPERTIES COMPILE_OPTIONS "-Wno-aggressive-loop-optimizations")
endif()

# Exclude some files from the list
list(REMOVE_ITEM fortran_sources ${PROJECT_SOURCE_DIR}/gtrak/grndm.F)
list(REMOVE_ITEM fortran_sources ${PROJECT_SOURCE_DIR}/gtrak/grndmq.F)
list(REMOVE_ITEM fortran_sources ${PROJECT_SOURCE_DIR}/erdecks/eustep.F)
#message(STATUS "fortran_sources ${fortran_sources}")

# C sources
set(c_sources)
foreach(_directory ${directories})
  file(GLOB add_c_sources
       ${PROJECT_SOURCE_DIR}/${_directory}/*.c)
  list(APPEND c_sources ${add_c_sources})
endforeach()
list(APPEND c_sources ${PROJECT_SOURCE_DIR}/minicern/lnxgs/ishftr.c)
# Linux specific, the file is kept on macosx, macosx64)
list(REMOVE_ITEM c_sources ${PROJECT_SOURCE_DIR}/minicern/lnblnk.c)
if(BUILD_GCALOR)
  list(REMOVE_ITEM c_sources ${PROJECT_SOURCE_DIR}/added/dummies_gcalor.c)
endif()
#message(STATUS "c_sources ${c_sources}")

# C++ sources
file(GLOB cxx_sources
     ${PROJECT_SOURCE_DIR}/comad/gcadd.cxx
     ${PROJECT_SOURCE_DIR}/TGeant3/*.cxx)
#message(STATUS "cxx_sources ${cxx_sources}")


#-------------------------------------------------------------------------------
# Locate headers for this project
#
file(GLOB headers ${PROJECT_SOURCE_DIR}/TGeant3/*.h)

#---Add definitions-------------------------------------------------------------
# cernlib settings
add_definitions(-DCERNLIB_BLDLIB -DCERNLIB_CZ)
# add flags to make gfortran build stable at -O2
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -finit-local-zero -fno-strict-overflow")
# allow non-standard-conform BOZ literal constants in GCC >=10
if (        "${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU"
    AND NOT "${CMAKE_Fortran_COMPILER_VERSION}" VERSION_LESS 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-invalid-boz")
endif()
# Architecture dependent not ported flags:
# -DCERNLIB_LINUX (linux, linuxx8664icc, linuxicc, macosx, macosxxlc, macosicc)
# -DCERNLIB_PPC (macosx64, macosxxlc, macosicc)
# -DCERNLIB_UNIX (alphagcc)
# -DCERNLIB_DECS (alphagcc, alphacxx6)
# -DCERNLIB_SUN (solarisCC5)
# -DCERNLIB_HPUX (hpuxacc)
if (CMAKE_SIZEOF_VOID_P EQUAL 8)
  add_definitions(-DCERNLIB_LXIA64)
else()
  add_definitions(-DCERNLIB_LINUX)
endif()
if (${CMAKE_Fortran_COMPILER} MATCHES gfortran+)
  add_definitions(-DCERNLIB_GFORTRAN)
endif()
if (${CMAKE_Fortran_COMPILER} MATCHES gfortran+)
  add_definitions(-DCERNLIB_GFORTRAN)
endif()
if (${CMAKE_Fortran_COMPILER} MATCHES g95+)
  add_definitions(-DCERNLIB_G95)
endif()
# using Clang on Mac OSX
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang" AND APPLE)
  set(CMAKE_SHARED_LINKER_FLAGS
      "${CMAKE_SHARED_LINKER_FLAGS} -undefined dynamic_lookup -Wl,-no_compact_unwind")
endif()

#---Add library-----------------------------------------------------------------
add_library(${library_name} ${fortran_sources} ${c_sources} ${cxx_sources}
            ${root_dict} ${headers})
set(DEPS ${ROOT_DEPS} ${VMC_DEPS})
target_link_libraries(${library_name} ${VMC_DEPS} ${ROOT_DEPS})
set_target_properties(geant321 PROPERTIES INTERFACE_LINK_LIBRARIES "${DEPS}")
target_include_directories(geant321 INTERFACE $<INSTALL_INTERFACE:include/TGeant3>)

#----Installation---------------------------------------------------------------
install(FILES ${headers} DESTINATION include/TGeant3)
install(TARGETS ${library_name} EXPORT Geant3Targets DESTINATION ${CMAKE_INSTALL_LIBDIR})
install(FILES ${root_dict_libs} DESTINATION ${CMAKE_INSTALL_LIBDIR})
