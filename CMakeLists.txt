########################################################
# build Garfield++ library and examples
# depend on: ROOT
# @author Klaus Zenker
CMAKE_MINIMUM_REQUIRED(VERSION 2.6 FATAL_ERROR)
########################################################

PROJECT( Garfield CXX Fortran)

### DEPENDENCIES #######################################
IF(DEFINED ENV{ROOTSYS})
    SET( CMAKE_MODULE_PATH $ENV{ROOTSYS}/etc/cmake )
ELSEIF(DEFINED ROOTSYS)
    SET( CMAKE_MODULE_PATH ${ROOTSYS}/etc/cmake )
ELSEIF(DEFINED ROOT_CMAKE_DIR)
    SET( CMAKE_MODULE_PATH ${ROOT_CMAKE_DIR} )
ELSE()
    MESSAGE(FATAL_ERROR "Please consider to set ROOTSYS or use -DROOTSYS=..."
                        "If still there is a proplem, point to the directory which includes FindROOT.cmake using -DROOT_CMAKE_DIR=...")
ENDIF(DEFINED ENV{ROOTSYS})


FIND_PACKAGE( ROOT 5.0 REQUIRED COMPONENTS Geom )
SET( CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/CMake )
INCLUDE( MacroCheckPackageLibs )
INCLUDE( MacroRootDict )

# If you cannot create symbolic links turn FALSE the library version
SET(BUILD_WITH_VERSION_NUMBER TRUE)
IF(BUILD_WITH_VERSION_NUMBER)
    SET( ${PROJECT_NAME}_MAJOR_VERSION 0 )
    SET( ${PROJECT_NAME}_MINOR_VERSION 2 )
    SET( ${PROJECT_NAME}_PATCH_VERSION 0 )
    SET( ${PROJECT_NAME}_VERSION
    ${${PROJECT_NAME}_MAJOR_VERSION}.${${PROJECT_NAME}_MINOR_VERSION}.${${PROJECT_NAME}_PATCH_VERSION} )
    SET( ${PROJECT_NAME}_SOVERSION ${${PROJECT_NAME}_VERSION} )
    INCLUDE( MacroCheckPackageVersion )
ENDIF(BUILD_WITH_VERSION_NUMBER)

## Add root libs ##
MESSAGE(" Root libraries: ${ROOT_LIBRARIES} ")
IF(DEFINED ROOT_INCLUDE_DIRS)
    # ROOT 6
    INCLUDE_DIRECTORIES ( ${PROJECT_SOURCE_DIR} ${ROOT_INCLUDE_DIRS} )
ELSE()
    # ROOT 5
    INCLUDE_DIRECTORIES ( ${PROJECT_SOURCE_DIR} ${ROOT_INCLUDE_DIR} )
    # COMPONENT mechanism in the FIND_PACKAGE is not supported 
    SET(ROOT_COMPONENT_LIBRARIES "-lGeom")
ENDIF(DEFINED ROOT_INCLUDE_DIRS)

## add garfield  #######################################
AUX_SOURCE_DIRECTORY( Source garfield_sources )

## add magboltz ########################################
SET( magboltz_sources Source/magboltz-9.01.f )
SET_SOURCE_FILES_PROPERTIES( ${magboltz_sources}  PROPERTIES LANGUAGE Fortran )

## add Heed ############################################
SET( heed_dirs 
    Heed
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

## Flags to pass to the compiler #######################
set(CMAKE_CXX_FLAGS "-Wall -Wextra -pedantic -ansi -Wabi -Wno-long-long -Woverloaded-virtual -fno-common -pg " )

# Add common flags for gfortran and g++
ADD_DEFINITIONS( "-c" )
# Allow to use debug symbols
IF( CMAKE_BUILD_TYPE STREQUAL "Debug" OR
 CMAKE_BUILD_TYPE STREQUAL "RelWithDebInfo" )
 ADD_DEFINITIONS( "-ggdb -O0" )
ELSE()
 ADD_DEFINITIONS( "-O2" )
ENDIF()
IF(ROOT_VERSION VERSION_GREATER 6.0)
    ADD_DEFINITIONS( "-std=c++0x" )
ENDIF()

# Add switches used in HEED
# -DNOT_INCLUDE_GASLIB_IN_PACSLIB is used in Heed/heed++/code/PhotoAbsCS.c
SET_SOURCE_FILES_PROPERTIES( ${heed_sources} PROPERTIES COMPILE_FLAGS "-DNOT_INCLUDE_GASLIB_IN_PACSLIB -DGARFIELD_HEED_INTERFACE -DUSE_SRANLUX -DEXCLUDE_FUNCTIONS_WITH_HISTDEF -DINS_CRETURN -DFUNNAMESTACK")

## create dictionary  ##################################
LIST( APPEND ROOT_DICT_INCLUDE_DIRS Include Heed)
SET( ROOT_DICT_CINT_DEFINITIONS "-DHANDLE_DICT_EXCEPTIONS=IGNORED_FOR_CINT" )
PREPARE_ROOT_DICT_HEADERS( Include )
GEN_ROOT_DICT_SOURCES( GarfieldDict.cxx )
LIST( APPEND garfield_sources ${ROOT_DICT_OUTPUT_SOURCES} )

## print information on considered files and the root dictionary include directories
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
TARGET_LINK_LIBRARIES( ${PROJECT_NAME} ${ROOT_LIBRARIES} ${ROOT_COMPONENT_LIBRARIES} )
IF(BUILD_WITH_VERSION_NUMBER)
    SET_TARGET_PROPERTIES( ${PROJECT_NAME} PROPERTIES
        VERSION ${${PROJECT_NAME}_VERSION}
        SOVERSION ${${PROJECT_NAME}_SOVERSION} )
ENDIF(BUILD_WITH_VERSION_NUMBER)

## BUILD examples             ##########################
if(NoExamples)
    MESSAGE(STATUS "Building of examples is switched off.")
else()
    MESSAGE(STATUS "Garfield++ exmaples will be build. In order disable this option use -DNoExamples=true")
    add_subdirectory(Examples)
endif()
## EXPORT PACKAGE INFORMATION ##########################
export(PACKAGE ${PROJECT_NAME})

# Configure the config files 
configure_file(CMake/GarfieldConfig.cmake.in
  "${PROJECT_BINARY_DIR}/GarfieldConfig.cmake" @ONLY)
configure_file(CMake/GarfieldConfigVersion.cmake.in
  "${PROJECT_BINARY_DIR}/GarfieldConfigVersion.cmake" @ONLY)

MESSAGE(STATUS "Install path: ${CMAKE_INSTALL_PREFIX}")
MESSAGE(STATUS "If you want to change this path call cmake -DCMAKE_INSTALL_PREFIX=my_install_path ...")
SET(CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_PREFIX})


# decide were the library will end up and to write info in  GarfieldLibraryDepends.cmake
INSTALL(TARGETS ${PROJECT_NAME} DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/ EXPORT GarfieldLibraryDepends)

# decide were to install GarfieldLibraryDepends.camke
INSTALL(EXPORT GarfieldLibraryDepends DESTINATION ${CMAKE_INSTALL_PREFIX}/cmake  COMPONENT cmakefiles)

# now install cofigured config files 
INSTALL( FILES ${PROJECT_BINARY_DIR}/GarfieldConfig.cmake
        ${PROJECT_BINARY_DIR}/GarfieldConfigVersion.cmake
        DESTINATION ${CMAKE_INSTALL_PREFIX}/cmake
        COMPONENT cmakefiles )
# install pcm file in case of root 6
IF(ROOT_VERSION VERSION_GREATER 6.0)
    INSTALL( FILES ${PROJECT_BINARY_DIR}/rootdict/${PROJECT_NAME}Dict_rdict.pcm 
             DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/ 
             COMPONENT dict)
ENDIF()   
