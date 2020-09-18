#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2014 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# Configuration file for CMake build for VMC applications.
# It finds the MC package according to user selection and sets
# MCPackages_FOUND
#
# I. Hrivnacova, 07/07/2014

#---Find MC packages -----------------------------------------------------------

#---Options---------------------------------------------------------------------
option(VMC_WITH_Geant4   "Build with Geant4" OFF)
option(VMC_WITH_Geant3   "Build with Geant3" OFF)
option(VMC_WITH_Multi    "Build with Multiple engines" OFF)

if (VMC_WITH_Geant4 AND VMC_WITH_Geant3 AND (NOT VMC_WITH_Multi))
  message(STATUS "VMC applications cannot be built with two MCs at one time.")
  message(STATUS "Please select one of [VMC_WITH_Geant4,VMC_WITH_Geant3] options")
  message(STATUS "and use another build/installation directory for another MC,")
  message(STATUS "or select also VMC_WITH_Multi option to build with multiple engines.")
  message(FATAL_ERROR "unsupported configuration detected.")
endif()

# Geant4
if(VMC_WITH_Geant4)
  # External G4Root (if required)
  if (Geant4VMC_USE_EXTERN_G4Root)
      find_package(G4Root REQUIRED)
  endif()

  # Geant4VMC
  # (it includes also Geant4 configuration options used in Geant4 VMC installation)
  set(Geant4VMC_DIR "" CACHE PATH "Directory where Geant4VMC is installed")
  find_package(Geant4VMC REQUIRED)

  # Geant4
  set(_components)
  if(Geant4VMC_USE_GEANT4_UI)
    list(APPEND _components ui_all)
  endif()
  if(Geant4VMC_USE_GEANT4_VIS)
    list(APPEND _components vis_all)
  endif()
  if(Geant4VMC_USE_GEANT4_G3TOG4)
    list(APPEND _components g3tog4)
  endif()
  find_package(Geant4 REQUIRED ${_components})
  add_definitions(-DUSE_GEANT4)

  # G4Root
  if (Geant4VMC_USE_G4Root)
    if (NOT G4Root_FOUND)
      find_package(G4Root REQUIRED)
    endif()
  endif()

  # VGM
  if (Geant4VMC_USE_VGM)
    find_package(VGM REQUIRED)
  endif()

  # If all required packages above were found we can update MCPackages_FOUND
  if(NOT VMC_WITH_Multi)
    set(MCPackages_FOUND TRUE)
    return()
  else()
    set(G4MCPackages_FOUND TRUE)
  endif()
endif()

# Geant3
if(VMC_WITH_Geant3)
  find_package(Geant3 REQUIRED)
  #PYTHIA6
  find_package(Pythia6 REQUIRED)
  # If all required packages above were found we can update MCPackages_FOUND
  if(NOT VMC_WITH_Multi)
    set(MCPackages_FOUND TRUE)
    return()
  else()
    set(G3MCPackages_FOUND TRUE)
  endif()
endif()

if(VMC_WITH_Multi AND G4MCPackages_FOUND AND G3MCPackages_FOUND)
  set(MCPackages_FOUND TRUE)
endif()
