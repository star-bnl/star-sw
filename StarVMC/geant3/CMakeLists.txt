#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2014 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# Configuration file for CMake build for Geant3 with VMC package.
# Inspired by Geant4, Geant4 VMC and VGM projects.
#
# I. Hrivnacova, 13/06/2014

#---  Enforce an out-of-source builds before anything else ---------------------
if(${CMAKE_SOURCE_DIR} STREQUAL ${CMAKE_BINARY_DIR})
    message(STATUS "Geant4 VMC requires an out-of-source build.")
    message(STATUS "Please remove these files from ${CMAKE_BINARY_DIR} first:")
    message(STATUS "CMakeCache.txt")
    message(STATUS "CMakeFiles")
    message(STATUS "Once these files are removed, create a separate directory")
    message(STATUS "and run CMake from there")
    message(FATAL_ERROR "in-source build detected")
endif()

#--- Define CMake requirements -------------------------------------------------
cmake_minimum_required(VERSION 2.6.4 FATAL_ERROR)

#--- Prepend our own CMake Modules to the search path --------------------------
set(CMAKE_MODULE_PATH 
    ${CMAKE_SOURCE_DIR}/cmake
    ${CMAKE_MODULE_PATH}) 

#--- Project definitions -------------------------------------------------------
project(Geant3)
set(${PROJECT_NAME}_VERSION_MAJOR "2")
set(${PROJECT_NAME}_VERSION_MINOR "4")
set(${PROJECT_NAME}_VERSION_PATCH "0")
set(${PROJECT_NAME}_VERSION
    ${${PROJECT_NAME}_VERSION_MAJOR}.${${PROJECT_NAME}_VERSION_MINOR}.${${PROJECT_NAME}_VERSION_PATCH})

#--- Default project build mode ------------------------------------------------
include(VMCBuildMode)

#--- Options -------------------------------------------------------------------
option(Geant3_INSTALL_DATA "Install data directory" ON)
option(BUILD_SHARED_LIBS "Build the dynamic libraries" ON)

#--- Enable Fortran ------------------------------------------------------------
enable_language(Fortran)

#--- Find required packages ----------------------------------------------------
find_package(ROOT REQUIRED)

#--- Utility to defined installation lib directory -----------------------------
include(VMCInstallLibDir)

#--- Project library -----------------------------------------------------------
include(Geant3BuildLibrary)

#--- Build project configuration -----------------------------------------------
include(Geant3BuildProject)

#--- CPack ----------------- --------------------------------------------------
include(Geant3CPack)

