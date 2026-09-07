#----------------------------------------------------------------------------
# Setup the project
cmake_minimum_required(VERSION 2.6 FATAL_ERROR)
project(RHICfSTARSimulation)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -m64")

#----------------------------------------------------------------------------
# Find Geant4 package, activating all available Vis drivers by default
# You can set WITH_GEANT4_VIS to OFF via the command line or ccmake/cmake-gui
# to build a batch mode only executable
#
option(WITH_GEANT4_VIS "Build example with Geant4 Vis drivers" ON)
if(WITH_GEANT4_VIS)
  find_package(Geant4 REQUIRED gdml vis_all)
else()
  find_package(Geant4 REQUIRED gdml)
endif()

#----------------------------------------------------------------------------
# Setup Geant4 include directories and compile definitions
#
include(${Geant4_USE_FILE})
message(STATUS "GEANT4 Path: ${Geant4_INCLUDE_DIRS}")

#----------------------------------------------------------------------------
# Find ROOT (required package)
#set(ROOTPATH /afs/rhic.bnl.gov/star/ROOT/5.34.38/.sl73_gcc485/rootdeb CACHE PATH "Path to the ROOT library") # for 32bit (old)
set(ROOTPATH /star/nfs4/AFS/star/ROOT/5.34.38/.sl73_x8664_gcc485/rootdeb CACHE PATH "Path to the ROOT library") # for 64bit in NFS4
message(STATUS "ROOT Path: ${ROOTPATH}")
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
list(APPEND CMAKE_PREFIX_PATH ${ROOTPATH})

find_package(ROOT REQUIRED COMPONENTS RIO Net Eve EG RGL Geom Gui Graf Graf3d Ged Matrix MathCore Table Physics Minuit2 Spectrum ROOTDataFrame)

#----------------------------------------------------------------------------
# Find RHICf+STAR simulation library
set(RHICFSTARSIMDIR ${CMAKE_CURRENT_LIST_DIR}/)
message(STATUS "RHICfSTARSim Path: ${RHICFSTARSIMDIR}")

set(COMPILE_SOURCE_DIRECTORY_LIST
  ${RHICFSTARSIMDIR}/source/Geant4/base
  ${RHICFSTARSIMDIR}/source/Geant4/sensitiveDetectors
  #${RHICFSTARSIMDIR}/source/Reconstruction
  ${RHICFSTARSIMDIR}/source/Util

  CACHE INTERNAL ""
)

set(TOOLS_LIBRARIES_LIST ${Geant4_LIBRARIES} ${ROOT_LIBRARIES} CACHE INTERNAL "")

foreach(_directory ${COMPILE_SOURCE_DIRECTORY_LIST})
  include_directories(${_directory})

  string(REPLACE ${CMAKE_SOURCE_DIR} "" tmpString ${_directory})
  string(REPLACE / _ _name ${tmpString})

  list(APPEND SOURCE_NAME_LIST ${_name})
  set(${_name}_G_NAME G__${_name}Dict)
  file(GLOB ${_name}_SOURCE_FILES ${_directory}/*.cc)
  file(GLOB ${_name}_HEADER_FILES ${_directory}/*.hh)
endforeach(_directory)

set(COMPILE_LIB_DIRECTORY_LIST ${RHICFSTARSIMDIR}/source/StRHICfSimDst)
link_directories(${LINK_DIRECTORIES})

foreach(_directory ${COMPILE_LIB_DIRECTORY_LIST})
  include_directories(${_directory})

  string(REPLACE ${CMAKE_SOURCE_DIR} "" tmpString ${_directory})
  string(REPLACE / _ _name ${tmpString})

  list(APPEND DIR_NAME_LIST ${_name})
  set(${_name}_G_NAME G__${_name}Dict)
  file(GLOB ${_name}_LINKDEF_FILE ${_directory}/LinkDef.h)
  file(GLOB ${_name}_SOURCE_FILES ${_directory}/StRHICfSim*.cxx)
  file(GLOB ${_name}_HEADER_FILES ${_directory}/StRHICfSim*.h)
endforeach(_directory)

include_directories( 
        ${ROOT_INCLUDE_DIR}
        ${Geant4_INCLUDE_DIR})

add_definitions(${ROOT_CXX_FLAGS})

foreach(_name ${DIR_NAME_LIST})
  ROOT_GENERATE_DICTIONARY(${${_name}_G_NAME} 
    ${${_name}_HEADER_FILES} 
    LINKDEF ${${_name}_LINKDEF_FILE})
endforeach(_name)

foreach(_name ${SOURCE_NAME_LIST})
  foreach(_file ${${_name}_SOURCE_FILES})
    list(APPEND LIB_LIST ${_file})
  endforeach(_file)
endforeach(_name)

foreach(_name ${DIR_NAME_LIST})
  foreach(_file ${${_name}_SOURCE_FILES})
    list(APPEND LIB_LIST ${_file})
  endforeach(_file)
  list(APPEND LIB_LIST ${${_name}_G_NAME}.cxx)
endforeach(_name)

add_library(RHICfSTARSimLib SHARED ${LIB_LIST})
target_link_libraries(RHICfSTARSimLib ${TOOLS_LIBRARIES_LIST})

add_executable(mainRHICfSTARSim RHICfSTARSim.cc)
target_link_libraries(mainRHICfSTARSim RHICfSTARSimLib)