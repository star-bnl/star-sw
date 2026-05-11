INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}")
INCLUDE_DIRECTORIES("${CMAKE_CURRENT_BINARY_DIR}")
FILE(GLOB files
  ${CMAKE_CURRENT_SOURCE_DIR}/hi*.f
  ${CMAKE_CURRENT_BINARY_DIR}/hi*.f)

set_source_files_properties(${base_files} PROPERTIES COMPILE_DEFINITIONS __HIJING__)

IF (CRMC_STATIC)
  add_library(Hijing STATIC ${files})
ELSE (CRMC_STATIC)
  add_library(Hijing SHARED ${base_files} ${files})
  target_link_libraries(Hijing CrmcBasic)
ENDIF (CRMC_STATIC)

INSTALL (TARGETS Hijing
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib/static
        )
