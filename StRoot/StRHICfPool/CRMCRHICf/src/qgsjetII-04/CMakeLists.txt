INCLUDE_DIRECTORIES("${CMAKE_CURRENT_SOURCE_DIR}")
INCLUDE_DIRECTORIES("${CMAKE_CURRENT_BINARY_DIR}")
FILE(GLOB files
  ${CMAKE_CURRENT_SOURCE_DIR}/*.f
  ${CMAKE_CURRENT_BINARY_DIR}/*.f
  ${CMAKE_SOURCE_DIR}/src/lzma-read/*.cc)
set_source_files_properties(${base_files} PROPERTIES COMPILE_DEFINITIONS __QGSJETII04__)

IF (CRMC_STATIC)
  add_library(QgsjetII04 STATIC ${files})
ELSE (CRMC_STATIC)
  add_library(QgsjetII04 SHARED ${base_files} ${files})
  target_link_libraries(QgsjetII04 CrmcBasic)
ENDIF (CRMC_STATIC)


INSTALL (TARGETS QgsjetII04
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib/static
        )
