INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}")
INCLUDE_DIRECTORIES("${CMAKE_CURRENT_BINARY_DIR}")
FILE(GLOB files
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f
  ${CMAKE_CURRENT_BINARY_DIR}/*.f)
set_source_files_properties(${base_files} PROPERTIES COMPILE_DEFINITIONS __DPMJET__)

IF (CRMC_STATIC)
  add_library(Dpmjet06 STATIC ${files})
ELSE (CRMC_STATIC)
  add_library(Dpmjet06 SHARED ${base_files} ${files})
  target_link_libraries(Dpmjet06 CrmcBasic)
ENDIF (CRMC_STATIC)

INSTALL (TARGETS Dpmjet06
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib/static
        )
