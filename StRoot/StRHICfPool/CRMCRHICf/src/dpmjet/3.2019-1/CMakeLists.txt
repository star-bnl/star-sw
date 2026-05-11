INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}")
INCLUDE_DIRECTORIES("${CMAKE_CURRENT_BINARY_DIR}")
FILE(GLOB files
  ${CMAKE_CURRENT_SOURCE_DIR}/*_epos.f  
  ${CMAKE_CURRENT_SOURCE_DIR}/DPMJET-19.1/common/*.f
  ${CMAKE_CURRENT_SOURCE_DIR}/DPMJET-19.1/src/dpmjet/*.f
  ${CMAKE_CURRENT_SOURCE_DIR}/DPMJET-19.1/src/phojet/*.f
  ${CMAKE_CURRENT_SOURCE_DIR}/DPMJET-19.1/src/pythia/*.f)

set_source_files_properties(${base_files} ${files} PROPERTIES COMPILE_FLAGS "-cpp -mtune=native -Og -Wno-uninitialized -Wno-unused-dummy-argument -std=legacy")
set_source_files_properties(${files} PROPERTIES COMPILE_DEFINITIONS FOR_CORSIKA )
set_property(SOURCE ${base_files} APPEND PROPERTY COMPILE_DEFINITIONS __DPMJET__ __DPMJET19__)


IF (CRMC_STATIC)
  add_library(Dpmjet19 STATIC ${files})
ELSE (CRMC_STATIC)
  add_library(Dpmjet19 SHARED ${base_files} ${files})
  target_link_libraries(Dpmjet19 CrmcBasic)
ENDIF (CRMC_STATIC)

target_include_directories(Dpmjet19 PUBLIC
  DPMJET-19.1/include/pythia
  PUBLIC DPMJET-19.1/include/phojet
  PUBLIC DPMJET-19.1/include/dpmjet
  PUBLIC DPMJET-19.1/include/flinclude )

INSTALL (TARGETS Dpmjet19
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib/static
        )
