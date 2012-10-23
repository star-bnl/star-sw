########################################################
# build scripts to run garfield simulations
# depend on: ROOT
# @author Klaus Zenker
CMAKE_MINIMUM_REQUIRED(VERSION 2.6 FATAL_ERROR)
########################################################

PROJECT( Garfield CXX Fortran)
SET( ${PROJECT_NAME}_MAJOR_VERSION 0 )
SET( ${PROJECT_NAME}_MINOR_VERSION 1 )
SET( ${PROJECT_NAME}_PATCH_VERSION 0 )
SET( ${PROJECT_NAME}_VERSION
  ${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION} )
SET( ${PROJECT_NAME}_SOVERSION ${${PROJECT_NAME}_VERSION} )

### DEPENDENCIES #######################################

SET( CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMake )
INCLUDE( MacroCheckPackageLibs )
INCLUDE( MacroCheckPackageVersion )
INCLUDE( MacroRootDict )
INCLUDE( FindROOT )

## Add root libs ##
FIND_PACKAGE( ROOT 5.0 REQUIRED COMPONENTS Geom )
INCLUDE_DIRECTORIES ( ${ROOT_INCLUDE_DIRS} ) 
LINK_LIBRARIES( ${ROOT_LIBRARIES} ${ROOT_COMPONENT_LIBRARIES} )
MESSAGE( STATUS "The ROOT Version is found to be ${ROOT_VERSION_MAJOR}.${ROOT_VERSION_MINOR}.${ROOT_VERSION_PATCH}" )

## Flags to pass to the compiler #######################
ADD_DEFINITIONS( "-Wall -Wextra -pedantic -ansi -Wabi -Wno-long-long -Woverloaded-virtual -fpic -fno-common -Os -c" )

## Allow to use debug symbols ##########################
IF( CMAKE_BUILD_TYPE STREQUAL "Debug" OR
 CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo" )
 ADD_DEFINITIONS( "-DDEBUG -g" )
ENDIF()
## add garfield  #######################################
AUX_SOURCE_DIRECTORY( Source garfield_sources )
LIST( APPEND ROOT_DICT_INCLUDE_DIRS Include )

## add magboltz ########################################
SET( magboltz_sources Source/magboltz-8.97.f )
SET_SOURCE_FILES_PROPERTIES( ${magboltz_sources}  PROPERTIES LANGUAGE Fortran )

## add Heed ############################################
SET( heed_dirs 
    Heed/heed++/code 
    Heed/wcpplib/clhep_units 
    Heed/wcpplib/geometry 
    Heed/wcpplib/ioniz 
    Heed/wcpplib/math 
    Heed/wcpplib/matrix 
    Heed/wcpplib/matter 
    Heed/wcpplib/particle 
    Heed/wcpplib/random 
    Heed/wcpplib/safetl 
    Heed/wcpplib/stream 
    Heed/wcpplib/util )
FOREACH( heed_dir ${heed_dirs} )
    AUX_SOURCE_DIRECTORY( ${heed_dir} heed_sources )
ENDFOREACH()

# Don't build HeedParticle_BGM.c.o because it will not compile
LIST( REMOVE_ITEM heed_sources Heed/heed++/code/HeedParticle_BGM.c )

SET_SOURCE_FILES_PROPERTIES( ${heed_sources} PROPERTIES LANGUAGE CXX)
## Add compiler flags needed to build heed ##############
ADD_DEFINITIONS( "-O2 -pg -DGARFIELD_HEED_INTERFACE -DUSE_SRANLUX -DEXCLUDE_FUNCTIONS_WITH_HISTDEF -DINS_CRETURN" )

# Not sure about this, but without this flag the same symbols appear in GasLib.c.o and PhotoAbsCSLib.c.o and I can't compile
ADD_DEFINITIONS( "-DNOT_INCLUDE_GASLIB_IN_PACSLIB" )

## create dictionary  ##################################
SET( ROOT_DICT_CINT_DEFINITIONS "-DHANDLE_DICT_EXCEPTIONS=IGNORED_FOR_CINT" )
INCLUDE( ${ROOT_DICT_MACROS_FILE} )
PREPARE_ROOT_DICT_HEADERS( Include )
GEN_ROOT_DICT_SOURCES( GarfieldDict.cxx )
LIST( APPEND garfield_sources ${ROOT_DICT_OUTPUT_SOURCES} )

MESSAGE( STATUS "###########################################################" )
MESSAGE( STATUS "Considered Garfield files :             ${garfield_sources}" )
MESSAGE( STATUS "-----------------------------------------------------------" )
MESSAGE( STATUS "Considered heed files :                 ${heed_sources}" )
MESSAGE( STATUS "-----------------------------------------------------------" )
MESSAGE( STATUS "Considered magboltz files :             ${magboltz_sources}" )
MESSAGE( STATUS "-----------------------------------------------------------" )
MESSAGE( STATUS "Directory used to find root dict files: ${ROOT_DICT_INCLUDE_DIRS}" )
MESSAGE( STATUS "###########################################################" )


## SET output path for libraries 
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib/)
## SET include directories 
INCLUDE_DIRECTORIES( ${ROOT_DICT_INCLUDE_DIRS} Heed )
## build the library
ADD_LIBRARY( ${PROJECT_NAME} SHARED
    ${garfield_sources} 
    ${magboltz_sources}
    ${heed_sources} )


SET_TARGET_PROPERTIES( ${PROJECT_NAME} PROPERTIES
    VERSION ${${PROJECT_NAME}_VERSION}
    SOVERSION ${${PROJECT_NAME}_SOVERSION} )

# set library install dir    
INSTALL( TARGETS ${PROJECT_NAME} DESTINATION ${PROJECT_SOURCE_DIR}/Library/  COMPONENT library )
# put cmake config files in the CMake folder
INSTALL( FILES ${PROJECT_BINARY_DIR}/GarfieldConfig.cmake
        ${PROJECT_BINARY_DIR}/GarfieldConfigVersion.cmake
        ${PROJECT_BINARY_DIR}/GarfieldBuildTreeSettings.cmake
        ${PROJECT_BINARY_DIR}/GarfieldLibraryDepends.cmake
        DESTINATION ${PROJECT_SOURCE_DIR}/Library/cmake/
        COMPONENT cmakefiles )

## EXPORT PACKAGE INFORMATION ##########################
# Add all targets to the build-tree export set
export(TARGETS ${PROJECT_NAME}
  FILE "${PROJECT_BINARY_DIR}/GarfieldLibraryDepends.cmake")
 
# Export the package for use from the build-tree
# (this registers the build-tree with a global CMake-registry)
export(PACKAGE ${PROJECT_NAME})
 
# Create a GarfieldBuildTreeSettings.cmake file for the use from the build tree
configure_file(CMake/GarfieldBuildTreeSettings.cmake.in
  "${PROJECT_BINARY_DIR}/GarfieldBuildTreeSettings.cmake" @ONLY)
 
# Create the GarfieldConfig.cmake and GarfieldConfigVersion files
SET( INSTALL_CMAKE_DIR ${PROJECT_BINARY_DIR} CACHE PATH
  "Installation directory for CMake files" )
SET( INSTALL_INCLUDE_DIR ${PROJECT_BINARY_DIR}/inlcude CACHE PATH 
  "Installation directory for Include files" )

configure_file(CMake/GarfieldConfig.cmake.in
  "${PROJECT_BINARY_DIR}/GarfieldConfig.cmake" @ONLY)
configure_file(CMake/GarfieldConfigVersion.cmake.in
  "${PROJECT_BINARY_DIR}/GarfieldConfigVersion.cmake" @ONLY)