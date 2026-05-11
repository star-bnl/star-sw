# - Try to find Rivet
# Defines:
#
#  RIVET_FOUND
#  RIVET_CXXFLAGS
#  RIVET_LDFLAGS
#  RIVET_INCLUDE_DIR
#  RIVET_INCLUDE_DIRS (not cached)
#  RIVET_LIBRARY
#  RIVET_LIBRARIES (not cached)
#  RIVET_LIBRARY_DIRS (not cached)

find_library(RIVET_LIBRARY NAMES Rivet)

# the following disables all default paths (either from cmake, from environment)
FIND_PATH (RIVET_DIR rivet-config)

if (RIVET_DIR)
  MESSAGE("Executing ${RIVET_DIR}/rivet-config")
  EXECUTE_PROCESS (COMMAND ${RIVET_DIR}/rivet-config --version
    OUTPUT_VARIABLE RIVET_VERSION
    )
  #  STRING (REGEX REPLACE "[ \t\r\n]+" "" ROOT_VERSION "${ROOT_VERSION}")
  #  STRING (REGEX REPLACE "/" "." ROOT_VERSION "${ROOT_VERSION}")

  EXECUTE_PROCESS (COMMAND ${RIVET_DIR}/rivet-config --includedir
    OUTPUT_VARIABLE RIVET_INCLUDE_DIR)

  EXECUTE_PROCESS (COMMAND ${RIVET_DIR}/rivet-config --libdir
    OUTPUT_VARIABLE RIVET_LIB_DIR)
  
  EXECUTE_PROCESS (COMMAND ${RIVET_DIR}/rivet-config --cppflags
    OUTPUT_VARIABLE RIVET_CXXFLAGS)

  EXECUTE_PROCESS (COMMAND ${RIVET_DIR}/rivet-config --ldflags
    OUTPUT_VARIABLE RIVET_LDFLAGS)

  EXECUTE_PROCESS (COMMAND ${RIVET_DIR}/rivet-config --libs
    OUTPUT_VARIABLE RIVET_LIBS)
  
  #set(RIVET_LIBRARIES ${RIVET_LIBRARY})
  #  mark_as_advanced(Rivet_FOUND)

   string (REGEX REPLACE "\n$" "" RIVET_CXXFLAGS "${RIVET_CXXFLAGS}")
    string (REPLACE " " ";" RIVET_CXXFLAGS "${RIVET_CXXFLAGS}")
  
  SET(Rivet_FOUND TRUE)

  MESSAGE (STATUS
    "Compatible version of Rivet found: " ${RIVET_VERSION} " at " ${RIVET_DIR})
  
endif(RIVET_DIR)
