INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}")
INCLUDE_DIRECTORIES("${CMAKE_CURRENT_BINARY_DIR}")
FILE(GLOB files
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f
  ${CMAKE_CURRENT_BINARY_DIR}/*.f)
set_source_files_properties( ${base_files} PROPERTIES COMPILE_DEFINITIONS __QGSJET01__)

IF (CRMC_STATIC)
  add_library(Qgsjet01 STATIC ${files})
ELSE (CRMC_STATIC)
  add_library(Qgsjet01 SHARED ${base_files} ${files})
  target_link_libraries(Qgsjet01 CrmcBasic)
ENDIF (CRMC_STATIC)

INSTALL (TARGETS Qgsjet01
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib/static
        )
