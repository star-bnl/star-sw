#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2019 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# Configuration file for CMake build for geant3
# which finds all required packages.
#
# I. Hrivnacova, 15/04/2019

#-- ROOT (required) ------------------------------------------------------------
find_package(ROOT CONFIG COMPONENTS EG Geom REQUIRED)
set(ROOT_DEPS ROOT::Core ROOT::RIO ROOT::Tree ROOT::Rint ROOT::Physics
    ROOT::MathCore ROOT::Thread ROOT::Geom ROOT::EG)
include(${ROOT_USE_FILE})

#-- VMC (required) ------------------------------------------------------------
if(ROOT_vmc_FOUND)
  message(STATUS "Using VMC built with ROOT")
  set(VMC_DEPS ROOT::VMC)
  message(STATUS "Adding -DUSE_ROOT_VMC")
  add_definitions(-DUSE_ROOT_VMC)
else()
  #-- VMC (required) ------------------------------------------------------------
  find_package(VMC CONFIG REQUIRED)
  set(VMC_DEPS VMCLibrary)
  if(NOT VMC_FIND_QUIETLY)
    message(STATUS "Found VMC ${VMC_VERSION} in ${VMC_DIR}")
  endif()
endif()
