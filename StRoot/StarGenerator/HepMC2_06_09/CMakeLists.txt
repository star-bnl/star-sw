#------------------------------------------------------------------------------
# Top Level CMakeLists.txt for HepMC
#  cmake -Dmomentum:STRING=[MEV|GEV] -Dlength:STRING=[MM|CM]
#        [-DCMAKE_INSTALL_PREFIX=/install/path] 
#        [-DCMAKE_BUILD_TYPE=Debug|Release|RelWithDebInfo|MinSizeRel]
#        [-Dbuild_docs:BOOL=ON] 
#        /path/to/source
#  make
#  make test
#  make install
#
# The default HepMC build type is CMAKE_BUILD_TYPE=RelWithDebInfo,
# which matches the default HepMC autoconf flags
#------------------------------------------------------------------------------

# use cmake 2.6 or later
cmake_minimum_required (VERSION 2.6)

# project name and version
project(HepMC)
set( version 2.06.09 )

if ( momentum )
  set( HEPMC_DEFAULT_MOM_UNIT ${momentum} )
else()
  message(FATAL_ERROR "You must specify the momentum units with -Dmomentum:STRING=[MEV|GEV]")
endif()

if ( length )
  set( HEPMC_DEFAULT_LEN_UNIT ${length} )
else()
  message(FATAL_ERROR "You must specify the length units with -Dlength:STRING=[MM|CM]")
endif()

message(STATUS "default momentum and length are ${HEPMC_DEFAULT_MOM_UNIT} ${HEPMC_DEFAULT_LEN_UNIT}")

# find the HepMC cmake modules
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules ${CMAKE_MODULE_PATH})

# make sure we are not building from within the source code directory
string(COMPARE EQUAL "${CMAKE_SOURCE_DIR}" "${CMAKE_BINARY_DIR}" in_source)
string( REGEX MATCH "${CMAKE_SOURCE_DIR}/" in_source_subdir
"${CMAKE_BINARY_DIR}")
if (in_source OR in_source_subdir)
  message(FATAL_ERROR "
ERROR: In source builds of this project are not allowed.
A separate build directory is required.
Please create one and run cmake from the build directory.
Also note that cmake has just added files to your source code directory.
We suggest getting a new copy of the source code.
Otherwise, delete `CMakeCache.txt' and the directory `CMakeFiles'.
  ")
endif ()

# build_docs is OFF (false) by default
if ( build_docs )
   message(STATUS "documents WILL be built and installed" )
else()
   message(STATUS "documents WILL NOT be installed" )
endif()

# various handy macros
include(HepMCVariables)
#include(HepMCParseVersion)

# because we want to move these libraries about,
# do not embed full path in shared libraries or executables
set (CMAKE_SKIP_RPATH)

ENABLE_TESTING()

# include search path
include_directories ("${CMAKE_CURRENT_SOURCE_DIR}")

#build all libraries in a single directory to facilitate testing
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)

# parse the version 
STRING( REGEX REPLACE "^([0-9]+)[.][0-9]+[.][0-9]+.*" "\\1" VERSION_MAJOR "${version}" )
STRING( REGEX REPLACE "^[0-9]+[.]([0-9]+)[.][0-9]+.*" "\\1" VERSION_MINOR "${version}" )
STRING( REGEX REPLACE "^[0-9]+[.][0-9]+[.]([0-9]+)+.*" "\\1" VERSION_PATCH "${version}" )
message(STATUS "${project} version ${version} parses to ${VERSION_MAJOR} ${VERSION_MINOR} ${VERSION_PATCH}" )

# set our preferred compiler flags and other variables
include(CMakeDetermineFortranCompiler)
hepmc_set_compiler_flags()

add_subdirectory(HepMC) 
add_subdirectory(src) 
add_subdirectory(fio) 
add_subdirectory(test) 
add_subdirectory(examples) 
add_subdirectory(doc)

# Packaging utility
include(HepMCUseCpack)

