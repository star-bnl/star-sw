#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2014 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# Configuration file for CMake build for VMC applications.
# It finds the ROOT package (required) and MtRoot package (according
# to user selection) and sets VMC_FOUND
#
# I. Hrivnacova, 26/02/2014

#---Options---------------------------------------------------------------------
option(VMC_WITH_MTRoot      "Build with MTRoot" ON)
option(VMC_INSTALL_EXAMPLES "Install examples libraries and programs" ON)
option(BUILD_SHARED_LIBS    "Build the dynamic libraries" ON)

#---Find required packages------------------------------------------------------

# ROOT (required)
find_package(ROOT REQUIRED)

# MTRoot
if(VMC_WITH_MTRoot)
  # Do not search for MTRoot if building within Geant4VMC
  if (NOT Geant4VMC_BUILD_MTRoot)
    find_package(MTRoot REQUIRED)
  endif()
endif()

# If all required packages above were found we can update VMC_FOUND
set(VMC_FOUND TRUE)
