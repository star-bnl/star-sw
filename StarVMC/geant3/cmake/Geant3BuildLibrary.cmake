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
cmake_minimum_required(VERSION 2.6.4 FATAL_ERROR)

#-- ROOT (required) ------------------------------------------------------------
if(NOT ROOT_FOUND)
  find_package(ROOT REQUIRED)
endif(NOT ROOT_FOUND)  
include_directories(${ROOT_INCLUDE_DIRS})

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
  ${CMAKE_SHARED_LIBRARY_PREFIX}geant321
  with_rootmap
  ${CMAKE_CURRENT_SOURCE_DIR}/TGeant3/TCallf77.h
  ${CMAKE_CURRENT_SOURCE_DIR}/TGeant3/TG3Application.h
  ${CMAKE_CURRENT_SOURCE_DIR}/TGeant3/TGeant3f77.h
  ${CMAKE_CURRENT_SOURCE_DIR}/TGeant3/TGeant3gu.h
  ${CMAKE_CURRENT_SOURCE_DIR}/TGeant3/TGeant3.h
  ${CMAKE_CURRENT_SOURCE_DIR}/TGeant3/TGeant3TGeo.h
  LINKDEF ${CMAKE_CURRENT_SOURCE_DIR}/TGeant3/geant3LinkDef.h)

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

# Fortran sources
set(fortran_sources gcinit.F)
foreach(_directory ${directories})
  file(GLOB add_f_sources 
       ${PROJECT_SOURCE_DIR}/${_directory}/*.F)
  list(APPEND fortran_sources ${add_f_sources})
endforeach()
list(APPEND fortran_sources ${PROJECT_SOURCE_DIR}/minicern/lnxgs/rdmin.F)

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
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
  # using Clang
  set(CMAKE_SHARED_LINKER_FLAGS
      "${CMAKE_SHARED_LINKER_FLAGS} -undefined dynamic_lookup -Wl,-no_compact_unwind")
endif()

#---Add library-----------------------------------------------------------------
add_library(geant321 ${fortran_sources} ${c_sources} ${cxx_sources}
            ${CMAKE_SHARED_LIBRARY_PREFIX}geant321_dict.cxx ${headers})
target_link_libraries(geant321 ${ROOT_LIBRARIES} -lVMC -lEG)

#----Installation---------------------------------------------------------------
install(FILES ${headers} DESTINATION include/TGeant3)
install(TARGETS geant321 EXPORT Geant3Targets DESTINATION ${CMAKE_INSTALL_LIBDIR})

# Install dictionary map (only if ROOT 6.x
if (${ROOT_FOUND_VERSION} GREATER 59999)
  install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_SHARED_LIBRARY_PREFIX}geant321_dict_rdict.pcm
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_SHARED_LIBRARY_PREFIX}geant321.rootmap
    DESTINATION ${CMAKE_INSTALL_LIBDIR})
endif()
