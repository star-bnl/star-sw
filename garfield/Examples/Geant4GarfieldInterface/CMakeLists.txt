# $Id: CMakeLists.txt 86065 2015-11-09 15:50:00Z dpfeiffe $

#----------------------------------------------------------------------------
# Setup the project
#
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
set(CMAKE_CXX_COMPILER "g++")
set(CMAKE_C_COMPILER "gcc")
set(CMAKE_CXX_FLAGS "-std=c++11")
set(CMAKE_C_FLAGS "-std=c++11")

project(garfieldpp)
option(G4ANALYSIS_USE "use ROOT" ON)
SET(CMAKE_MODULE_PATH /afs/cern.ch/sw/lcg/external/geant4/10.1/share/cmake/Modules)
set(CMAKE_BUILD_TYPE "Debug")
find_package(ROOT)
# Try to find Garfield using cmake assuming it was build using cmake
# If it is not found it is assumed it was build using the make file
find_package(Garfield)
if(NOT Garfield_FOUND)
  MESSAGE(STATUS "Could not find cmake configuration for packgage Garfield. Assuming it was build using the make file.")
else()
  MESSAGE(STATUS "Found package Garfield in ${GARFIELD_SOURCE_DIR}")
endif()
#----------------------------------------------------------------------------
# Find Geant4 package, activating all available UI and Vis drivers by default
# You can set WITH_GEANT4_UIVIS to OFF via the command line or ccmake/cmake-gui
# to build a batch mode only executable
#
option(WITH_GEANT4_UIVIS "Build example with Geant4 UI and Vis drivers" ON)
if(WITH_GEANT4_UIVIS)
  find_package(Geant4 REQUIRED ui_all vis_all)
else()
  find_package(Geant4 REQUIRED)
endif()

#----------------------------------------------------------------------------
# Setup Geant4 include directories and compile definitions
# Setup include directory for this project
#
include(${Geant4_USE_FILE})
include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories($ENV{ROOTSYS}/include)

link_directories($ENV{ROOTSYS}/lib)
if(NOT Garfield_FOUND)
include_directories($ENV{GARFIELD_HOME}/Include $ENV{GARFIELD_HOME}/Heed)
link_directories($ENV{GARFIELD_HOME}/Library)
else()
include_directories(${GARFIELD_INCLUDE_DIRS})
endif()



#----------------------------------------------------------------------------
# Locate sources and headers for this project
# NB: headers are included so they will show up in IDEs
#
file(GLOB sources ${PROJECT_SOURCE_DIR}/src/*.cc)
file(GLOB headers ${PROJECT_SOURCE_DIR}/include/*.hh)
if(NOT Garfield_FOUND)
file(GLOB headers $ENV{GARFIELD_HOME}/Include/*.hh)
else()
file(GLOB headers ${GARFIELD_SOURCE_DIR}/Include/*.hh)
endif()
#----------------------------------------------------------------------------
# Add the executable, and link it to the Geant4 libraries
#
add_executable(exampleGarfield exampleGarfield.cc ${sources} ${headers})
if(NOT Garfield_FOUND)
target_link_libraries(exampleGarfield ${Geant4_LIBRARIES} -lGarfield -lgfortran  -lGeom  -lGdml ${ROOT_LIBRARIES})
else()
target_link_libraries(exampleGarfield ${Geant4_LIBRARIES} ${GARFIELD_LIBRARIES} -lgfortran  -lGeom  -lGdml ${ROOT_LIBRARIES})
endif()
#----------------------------------------------------------------------------
# Copy all scripts to the build directory, i.e. the directory in which we
# build exampleGarfield. This is so that we can run the executable directly because it
# relies on these scripts being in the current working directory.
#
set(EXAMPLEGARFIELD_SCRIPTS
  icons.mac
  gui.mac
  physics.mac
  init_vis.mac
  run.mac
  vis.mac
  ar_70_co2_30_1000mbar.gas
  )


foreach(_script ${EXAMPLEGARFIELD_SCRIPTS})
  configure_file(
    ${PROJECT_SOURCE_DIR}/${_script}
    ${PROJECT_BINARY_DIR}/${_script}
    COPYONLY
    )
endforeach()

#----------------------------------------------------------------------------
# Install the executable to 'bin' directory under CMAKE_INSTALL_PREFIX
#
install(TARGETS exampleGarfield DESTINATION bin)
