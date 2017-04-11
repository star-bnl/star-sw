# #require a certain version
CMAKE_MINIMUM_REQUIRED( VERSION 2.6 )

# declare the project name
PROJECT(GBL)

# project version
SET( ${PROJECT_NAME}_VERSION_MAJOR 2 )
SET( ${PROJECT_NAME}_VERSION_MINOR 0 )
SET( ${PROJECT_NAME}_VERSION_PATCH 0 )

# make life easier and simply use the ilcsoft default settings
# load default ilcsoft settings (install prefix, build type, rpath, etc.)
LIST( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )
INCLUDE( ilcsoft_default_settings )

# enable ROOT support
OPTION( SUPPORT_ROOT "Support ROOT for user I/O" True )
IF( SUPPORT_ROOT )
  FIND_PACKAGE(ROOT REQUIRED)
  ADD_DEFINITIONS(-DGBL_EIGEN_SUPPORT_ROOT)
ENDIF()
FIND_PACKAGE(Eigen3 REQUIRED)

# require proper c++
#ADD_DEFINITIONS( "-Wall -g -ansi -pedantic -Wno-long-long" )

# include directories
IF( SUPPORT_ROOT )
  INCLUDE_DIRECTORIES( BEFORE ./include ${ROOT_INCLUDE_DIRS} ${EIGEN3_INCLUDE_DIR} )
ELSE()
  INCLUDE_DIRECTORIES( BEFORE ./include ${EIGEN3_INCLUDE_DIR} )
ENDIF()
INSTALL( DIRECTORY ./include DESTINATION . PATTERN ".svn" EXCLUDE )

# declare the stupid ROOT library path
LINK_DIRECTORIES(${ROOT_LIBRARY_DIR})

# add library
AUX_SOURCE_DIRECTORY( ./src library_sources )
ADD_SHARED_LIBRARY( ${PROJECT_NAME} ${library_sources} )
INSTALL( TARGETS ${PROJECT_NAME} DESTINATION lib )

TARGET_LINK_LIBRARIES(${PROJECT_NAME} ${ROOT_LIBS})

# the examples, build with "make examples"
ADD_SUBDIRECTORY( examples/ )
  
# add a target to generate API documentation with Doxygen
FIND_PACKAGE(Doxygen)

if(DOXYGEN_FOUND)

    INCLUDE(GlobalSVNRevision)
    
    CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/doc/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/doc/Doxyfile @ONLY)

    ADD_CUSTOM_TARGET(doc
        ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/doc/Doxyfile
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/doc
        COMMENT "Generating API documentation with Doxygen" VERBATIM
    )

ELSE()
    MESSAGE( STATUS "Doxygen not installed or found -- API documentation can't be generatred!" )

ENDIF(DOXYGEN_FOUND)

# display some variables and write them to cache
DISPLAY_STD_VARIABLES()

# generate and install following configuration files
GENERATE_PACKAGE_CONFIGURATION_FILES( GBLConfig.cmake GBLConfigVersion.cmake )
