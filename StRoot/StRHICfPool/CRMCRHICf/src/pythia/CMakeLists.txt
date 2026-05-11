INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}")
INCLUDE_DIRECTORIES("${CMAKE_CURRENT_BINARY_DIR}")
FILE(GLOB files
  ${CMAKE_CURRENT_SOURCE_DIR}/*epos.f
  ${CMAKE_CURRENT_BINARY_DIR}/*6.4.*.f)
set_source_files_properties(${base_files} PROPERTIES COMPILE_DEFINITIONS __PYTHIA__)

IF (CRMC_STATIC)
  add_library(Pythia STATIC ${files})
ELSE (CRMC_STATIC)
  add_library(Pythia SHARED ${base_files} ${files})
  target_link_libraries(Pythia CrmcBasic)
ENDIF (CRMC_STATIC)

INSTALL (TARGETS Pythia
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib/static
        )
