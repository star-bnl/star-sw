#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2014 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# Configuration file for CMake build for VMC applications.
# It defines:
# - include directories
# - compile definitions
# - link libraries (MC_LIBRARIES) for all required and optional packages
# - MC_PREFIX - a prefix which can be included in the name of the executables
#
# I. Hrivnacova, 26/02/2014

#message(STATUS "Processing UseMC.cmake")

# VMC packages
#
if (NOT VMC_FOUND)
  find_package(VMC REQUIRED)
endif(NOT VMC_FOUND)

# MC packages
#
if (NOT MC_FOUND)
  find_package(MC)
endif(NOT MC_FOUND)

set(MC_LIBRARIES)

# ROOT (required)
include_directories(${ROOT_INCLUDE_DIRS})

# Geant4
if(VMC_WITH_Geant4)
  add_definitions(-DUSE_GEANT4) 
  # Workaround for upstream bug: http://bugzilla-geant4.kek.jp/show_bug.cgi?id=1663
  #include(${Geant4_USE_FILE})
  include(UseGeant4)
  
  if(Geant4VMC_FOUND)
     # build outside Geant4VMC
    include_directories(${Geant4VMC_INCLUDE_DIRS})
    set(MC_LIBRARIES ${MC_LIBRARIES} ${Geant4VMC_LIBRARIES})
  else()
    # build inside Geant4VMC
    include_directories(${Geant4VMC_SOURCE_DIR}/source/global/include)
    include_directories(${Geant4VMC_SOURCE_DIR}/source/geometry/include)
    include_directories(${Geant4VMC_SOURCE_DIR}/source/digits+hits/include)
    include_directories(${Geant4VMC_SOURCE_DIR}/source/physics/include)
    include_directories(${Geant4VMC_SOURCE_DIR}/source/physics_list/include)
    include_directories(${Geant4VMC_SOURCE_DIR}/source/event/include)
    include_directories(${Geant4VMC_SOURCE_DIR}/source/run/include)
    include_directories(${Geant4VMC_SOURCE_DIR}/source/visualization/include)
    set(MC_LIBRARIES ${MC_LIBRARIES} geant4vmc)
  endif(Geant4VMC_FOUND)

  if(G4Root_FOUND)
    # build outside Geant4VMC
    set(MC_LIBRARIES ${MC_LIBRARIES} ${G4Root_LIBRARIES})
  else()
    # build inside Geant4VMC
    include_directories(${Geant4VMC_SOURCE_DIR}/g4root/include)
    set(MC_LIBRARIES ${MC_LIBRARIES} g4root)
  endif(G4Root_FOUND)

  if(VGM_FOUND)
    set(MC_LIBRARIES ${MC_LIBRARIES} ${VGM_LIBRARIES})
  endif(VGM_FOUND)

  set(MC_LIBRARIES ${MC_LIBRARIES} ${Geant4_LIBRARIES})
  set(MC_PREFIX "g4")
endif(VMC_WITH_Geant4)

if(VMC_WITH_Geant3)
  # always build outside Geant4VMC
  add_definitions(-DUSE_GEANT3) 
  include_directories(${Geant3_INCLUDE_DIRS})
  
  #Pythia6
  if(Pythia6_FOUND)
    set(MC_LIBRARIES ${MC_LIBRARIES} ${Pythia6_LIBRARIES} ${Geant3_LIBRARIES})
  else()
    set(MC_LIBRARIES ${Geant3_LIBRARIES} ${MC_LIBRARIES})
  endif(Pythia6_FOUND)

  set(MC_PREFIX "g3")
    
endif(VMC_WITH_Geant3)

# MTRoot (optional)
if (VMC_WITH_MTRoot)
  # MTRoot
  if (MTRoot_FOUND)
     # build outside Geant4VMC
    set(MC_LIBRARIES ${MTRoot_LIBRARIES} ${MC_LIBRARIES})
  else()
     # build inside Geant4VMC
     # includes are already defined
     set(MC_LIBRARIES ${MC_LIBRARIES} mtroot)
  endif(MTRoot_FOUND)
endif(VMC_WITH_MTRoot)

# Finally add Root libraries
set(MC_LIBRARIES ${MC_LIBRARIES} ${ROOT_LIBRARIES} -lVMC -lEG)

#message(STATUS "MC_LIBRARIES ${MC_LIBRARIES}")
