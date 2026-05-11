INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}")
INCLUDE_DIRECTORIES("${CMAKE_CURRENT_BINARY_DIR}")
FILE(GLOB files
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f
  ${CMAKE_CURRENT_BINARY_DIR}/*.f
  ${CMAKE_BINARY_DIR}/src/pythia/pythia6215.f)

set_source_files_properties(${base_files} PROPERTIES COMPILE_DEFINITIONS __PHOJET__)

IF (CRMC_STATIC)
  add_library(Phojet STATIC ${files})
ELSE (CRMC_STATIC)
  add_library(Phojet SHARED ${base_files} ${files})
  target_link_libraries(Phojet CrmcBasic)
ENDIF (CRMC_STATIC)

INSTALL (TARGETS Phojet
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib/static
        )
