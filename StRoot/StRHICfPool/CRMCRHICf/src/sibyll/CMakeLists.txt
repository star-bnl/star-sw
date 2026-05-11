INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}")
INCLUDE_DIRECTORIES("${CMAKE_CURRENT_BINARY_DIR}")
FILE(GLOB files
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f
  ${CMAKE_CURRENT_BINARY_DIR}/*.f)
set_source_files_properties(${base_files} PROPERTIES COMPILE_DEFINITIONS __SIBYLL__)

IF (CRMC_STATIC)
  add_library(Sibyll STATIC ${files})
ELSE (CRMC_STATIC)
  add_library(Sibyll SHARED ${base_files} ${files})
  target_link_libraries(Sibyll CrmcBasic)
ENDIF (CRMC_STATIC)

INSTALL (TARGETS Sibyll
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib/static
        )
