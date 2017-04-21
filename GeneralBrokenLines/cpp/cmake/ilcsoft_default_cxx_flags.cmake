## Set the default compiler flags for all projects picking up default ilcutil settings
## This runs checks if compilers support the flag and sets them, if they do
## this will create a humongous amount of warnings when compiling :)

INCLUDE(CheckCXXCompilerFlag)

SET(COMPILER_FLAGS -Wall -Wextra -Wshadow -Weffc++ -pedantic -Wno-long-long -Wuninitialized )

IF( NOT APPLE )
 LIST( APPEND COMPILER_FLAGS -Wl,-no-undefined )
ENDIF()


MESSAGE( STATUS "FLAGS ${COMPILER_FLAGS}" )
FOREACH( FLAG ${COMPILER_FLAGS} )
  CHECK_CXX_COMPILER_FLAG( "${FLAG}" CXX_FLAG_WORKS_${FLAG} )
  IF( ${CXX_FLAG_WORKS_${FLAG}} )
    MESSAGE ( STATUS "Adding ${FLAG} to CXX_FLAGS" )
    ### We prepend the flag, so they are overwritten by whatever the user sets in his own configuration
    SET ( CMAKE_CXX_FLAGS "${FLAG} ${CMAKE_CXX_FLAGS} ")
  ELSE()
    MESSAGE ( STATUS "NOT Adding ${FLAG} to CXX_FLAGS" )
  ENDIF()
ENDFOREACH()

OPTION( USE_CXX11 "Use CXX Standard 2011" True )
IF( USE_CXX11 )
  SET( FLAG "-std=c++11" )
  CHECK_CXX_COMPILER_FLAG( ${FLAG} CXX_FLAG_WORKS_${FLAG} )
  IF( ${CXX_FLAG_WORKS_${FLAG}} )
    SET( CMAKE_CXX_FLAGS " ${FLAG} ${CMAKE_CXX_FLAGS}")
  ELSE()
    MESSAGE( FATAL_ERROR "Cannot add ${FLAG} to CMAKE_CXX_FLAGS, but c++11 was requested, check your compiler" )
  ENDIF()
ELSE()
  MESSAGE( STATUS "NOT building with CXX11 standard" )
ENDIF()
