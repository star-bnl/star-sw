#------------------------------------------------
# The Virtual Monte Carlo packages
# Copyright (C) 2015 Ivana Hrivnacova
# All rights reserved.
#
# For the licensing terms see geant4_vmc/LICENSE.
# Contact: root-vmc@cern.ch
#-------------------------------------------------

# - Try to find Garfield++ instalation
# This module sets up Garfield information
# It defines:
# Garfield_FOUND          If Garfiled++ is found
# Garfield_INCLUDE_DIRS   PATH to the include directories
# Garfield_LIBRARIES      the libraries needed to use Garfield++

#message(STATUS "Looking for Garfield ...")

# Alternative paths which can be defined by user
set(Garfield_DIR "" CACHE PATH "Directory where Garfield is installed")
set(Garfield_INC_DIR "" CACHE PATH "Alternative directory for Garfield includes")
set(Garfield_LIB_DIR "" CACHE PATH "Alternative directory for Garfield libraries")

find_path(Garfield_INCLUDE_DIRS Sensor.hh
          HINTS ${Garfield_DIR}/include ${Garfield_INC_DIR}
          $ENV{GARFIELD_HOME}/Include)
#message(STATUS Garfield_INCLUDE_DIRS ${Garfield_INCLUDE_DIRS})

find_library(Garfield_LIBRARIES NAMES Garfield
	         HINTS ${Garfield_DIR}/lib ${Garfield__LIB_DIR}
             HINTS $ENV{GARFIELD_HOME}/Library)
#message(STATUS Garfield_LIBRARIES ${Garfield_LIBRARIES})

#if (${Garfield_LIBRARY_DIR})
#  set (Garfield_LIBRARIES -L${Garfield_LIBRARY_DIR} -lGarfield)
#endif()

if (Garfield_INCLUDE_DIRS AND Garfield_LIBRARIES)
  set (Garfield_FOUND TRUE)
endif()

if (Garfield_FOUND)
  if (NOT Garfield_FIND_QUIETLY)
      message(STATUS "Found Garfield includes in ${Garfield_INCLUDE_DIRS}")
      message(STATUS "Found Garfield libraries ${Garfield_LIBRARIES}")
  endif (NOT Garfield_FIND_QUIETLY)
else(Garfield_FOUND)
  if (Garfield_FIND_REQUIRED)
    message(FATAL_ERROR "Garfield required, but not found")
  endif (Garfield_FIND_REQUIRED)
endif(Garfield_FOUND)

# Make variables changeble to the advanced user
mark_as_advanced(Garfield_INCLUDE_DIRS)
mark_as_advanced(Garfield_LIBRARIES)
mark_as_advanced(Garfield_LIBRARY_DIR)
