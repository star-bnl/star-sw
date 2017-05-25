#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2014 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# Adopted from Virtual Geometry Model and ROOT (ROOT_GENERATE_DICTIONARY)
# (http://ivana.home.cern.ch/ivana/VGM.html)
# I. Hrivnacova, 31/01/2014
#
# The CMake build for Virtual Geometry Model is a result of a merge 
# of the CMake configuration files kindly provided 
# by Florian Uhlig, GSI and Pere Mato, CERN.
# (http://ivana.home.cern.ch/ivana/VGM.html)

# - Try to find Pythia6
# in a directory defined via Pythia6 environment variable
# Once done this will define
#
# Defines:
#  Pythia6_FOUND
#  Pythia6_LIBRARIES
#  Pythia6_LIBRARY_DIR - PATH to the library directory 

#message(STATUS "Looking for Pythia6 ...")

find_path(Pythia6_LIBRARY_DIR 
  NAMES libPythia6.so libPythia6.dylib
        pythia6-$ENV{PYTHIA6_VERSION}.so pythia6-$ENV{PYTHIA6_VERSION}.dylib
  PATHS
  ${Pythia6_LIB_DIR}
  ${Pythia6_DIR}/lib
  $ENV{PYTHIA6}
  $ENV{PYTHIA6}/lib
  ${ROOT_LIBRARY_DIR} 
)

find_library(Pythia6_LIBRARY 
  NAMES Pythia6 pythia6-$ENV{PYTHIA6_VERSION}
  HINTS ${Pythia6_LIBRARY_DIR})

set(Pythia6_LIBRARIES ${Pythia6_LIBRARY}) 

#message(STATUS Pythia6_LIBRARY_DIR ${Pythia6_LIBRARY_DIR} )
#message(STATUS Pythia6_LIBRARIES ${Pythia6_LIBRARIES} )

if (Pythia6_LIBRARY)
  set(Pythia6_FOUND TRUE)
endif ()

if (Pythia6_FOUND)
  set(LD_LIBRARY_PATH ${LD_LIBRARY_PATH} ${Pythia6_LIBRARY_DIR})
  if (NOT Pythia6_FIND_QUIETLY)
    message(STATUS "Found Pythia6 in ${Pythia6_LIBRARY_DIR}")
  endif (NOT Pythia6_FIND_QUIETLY)
else (Pythia6_FOUND)
  if (Pythia6_FIND_REQUIRED)
    message(FATAL_ERROR "Looking for Pythia6 ... - Not found")
  endif (Pythia6_FIND_REQUIRED)
endif (Pythia6_FOUND)

mark_as_advanced(Pythia6_FOUND Pythia6_LIBRARIES Pythia6_LIBRARY_DIR)
